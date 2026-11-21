(* Store whole cents as exactly representable floating-point integers. The
   bound keeps arithmetic exact in both native OCaml and JavaScript. *)
let maximum_cents = 9_000_000_000_000_000.

let cents amount =
  if (not (Float.is_finite amount)) || amount <= 0. then
    Error "Amount must be a positive, finite number."
  else
    let scaled = amount *. 100. in
    let rounded = Float.round scaled in
    if (not (Float.is_finite scaled)) || rounded > maximum_cents then
      Error "Amount is too large."
    else if rounded < 1. || abs_float (scaled -. rounded) > 0.000001 then
      Error "Amount must use whole cents (at most two decimal places)."
    else Ok rounded

type goal = {
  id : int;
  name : string;
  target_cents : float;
  deadline : string option;
  contribution_cents : float list;
}

type progress = {
  saved : float;
  remaining : float;
  percent : float;
  complete : bool;
}

let id goal = goal.id
let name goal = goal.name
let target goal = goal.target_cents /. 100.
let deadline goal = goal.deadline

let contributions goal =
  List.map (fun cents -> cents /. 100.) goal.contribution_cents

let saved_cents goal = List.fold_left ( +. ) 0. goal.contribution_cents

let create_goal ~id ~name ~target ~deadline =
  if id <= 0 then Error "Goal ID must be positive."
  else
    Result.bind (cents target) (fun target_cents ->
        Result.bind (Transaction.parse_text "Goal name" name) (fun name ->
            let parsed =
              match deadline with
              | None -> Ok None
              | Some date ->
                  Result.map Option.some (Transaction.parse_date date)
            in
            Result.map
              (fun deadline ->
                { id; name; target_cents; deadline; contribution_cents = [] })
              parsed))

let progress goal =
  let saved = saved_cents goal in
  {
    saved = saved /. 100.;
    remaining = max 0. (goal.target_cents -. saved) /. 100.;
    percent = min 1. (saved /. goal.target_cents) *. 100.;
    complete = saved >= goal.target_cents;
  }

let get_goal id goals = List.find_opt (fun goal -> goal.id = id) goals

let add_goal goal goals =
  match get_goal goal.id goals with
  | Some _ -> Error "A goal with this ID already exists."
  | None -> Ok (goals @ [ goal ])

let next_goal_id goals =
  let maximum = List.fold_left (fun acc goal -> max acc goal.id) 0 goals in
  if maximum = max_int then Error "No more goal IDs are available."
  else Ok (maximum + 1)

let replace id replacement goals =
  List.map (fun goal -> if goal.id = id then replacement else goal) goals

let edit_goal id ~name ~target ~deadline goals =
  match get_goal id goals with
  | None -> Error "Goal not found."
  | Some original ->
      Result.map
        (fun goal ->
          replace id
            { goal with contribution_cents = original.contribution_cents }
            goals)
        (create_goal ~id ~name ~target ~deadline)

let delete_goal id goals =
  match get_goal id goals with
  | None -> Error "Goal not found."
  | Some _ -> Ok (List.filter (fun goal -> goal.id <> id) goals)

let contribute id ~amount goals =
  match get_goal id goals with
  | None -> Error "Goal not found."
  | Some goal ->
      Result.bind (cents amount) (fun amount ->
          if amount > maximum_cents -. saved_cents goal then
            Error "Contribution would make the saved total too large."
          else
            Ok
              (replace id
                 {
                   goal with
                   contribution_cents = goal.contribution_cents @ [ amount ];
                 }
                 goals))

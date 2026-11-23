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
(** Gives id of the goal *)
let id goal = goal.id

(** Gives the name of the goal *)
let name goal = goal.name

(** Gives the target amount of the goal *)
let target goal = goal.target_cents /. 100.

(** Gives the deadline of the goal *)
let deadline goal = goal.deadline

(** Gives the list of contributions to the goal *)
let contributions goal =
  List.map (fun cents -> cents /. 100.) goal.contribution_cents

(** Calculates the total amount saved in the goal *)
let saved_cents goal = List.fold_left ( +. ) 0. goal.contribution_cents

(** Creates a new goal with the specified parameters *)
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

(** Calculates the progress of the goal *)
let progress goal =
  let saved = saved_cents goal in
  {
    saved = saved /. 100.;
    remaining = max 0. (goal.target_cents -. saved) /. 100.;
    percent = min 1. (saved /. goal.target_cents) *. 100.;
    complete = saved >= goal.target_cents;
  }

(** Gets a goal from a list of goals by its id*)
let get_goal id goals = List.find_opt (fun goal -> goal.id = id) goals

(** Adds a new goal to the list of goals *)
let add_goal goal goals =
  match get_goal goal.id goals with
  | Some _ -> Error "A goal with this ID already exists."
  | None -> Ok (goals @ [ goal ])

(** Gets the next available goal ID *)
let next_goal_id goals =
  let maximum = List.fold_left (fun acc goal -> max acc goal.id) 0 goals in
  if maximum = max_int then Error "No more goal IDs are available."
  else Ok (maximum + 1)

(** Replaces a goal in the list at the current id with a new goal *)
let replace id replacement goals =
  List.map (fun goal -> if goal.id = id then replacement else goal) goals

(** Edits an existing goal in the list *)
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

(** Deletes a goal from the list *)
let delete_goal id goals =
  match get_goal id goals with
  | None -> Error "Goal not found."
  | Some _ -> Ok (List.filter (fun goal -> goal.id <> id) goals)

(** Adds a contribution to a goal from the list of all goals*)
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

(** Adds a contribution to an individual goal *)
let add_contribution goal contribution =
  { goal with contribution_cents = goal.contribution_cents @ [ contribution ] } 

(** Converts the data of the current goals into one string. *)
let string_of_goals goals =
  String.concat "\n"
    (List.map
       (fun goal ->
         let contributions =
           String.concat ";"
             (List.map
                (fun amount -> Printf.sprintf "%.0f" (amount *. 100.))
                (contributions goal))
         in
         [
           string_of_int (id goal);
           (name goal);
           Printf.sprintf "%.0f" (target goal);
           (Option.value ~default:"" (deadline goal));
           contributions;
         ]
         |> String.concat ",")
       goals)


(** Parse a list of contribution amounts in cents. Contributions are seperated by semicolons. *)
let parse_contributions text =
  let contributions = String.split_on_char ';' text in
  match contributions with
  | [""] -> []
  | lst -> List.map (fun amount ->
            Result.get_ok (Transaction.parse_amount amount)) lst
         
  
(** Parse one CSV row into a goal *)
let load_goal line : (goal,string) result =
  match String.split_on_char ',' line with
  | [ id; name; target; deadline; contributions ] -> 
      let id = int_of_string (String.trim id) in
      let target = float_of_string (String.trim target) in
      let contributions = parse_contributions contributions in
      let deadline =
        match String.trim deadline with
        | "" -> None
        | date -> Some date
      in
      let goal = (create_goal ~id:id ~name:name ~target:target ~deadline:deadline) in
      if Result.is_error goal then Error "Saved goal data is invalid."
      else
      let goal = ref (Result.get_ok goal) in
      List.iter (fun con -> goal := (add_contribution !goal con)) contributions;
      Ok !goal
  | _ -> Error "Saved goal data is invalid."
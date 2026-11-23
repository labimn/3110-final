open Js_of_ocaml
module S = Spendwise.Savings

let document = Dom_html.document
let goals = ref []
let editing = ref None

(** Temporary. Will change once multiple accounts are implemented*)
let storage_file = "spendwise-goals.swg"



(** We currently store the data in the browser's localStorage. This is persistent upon reloading and closing the browser. *)
let storage =
  Js.Optdef.get Dom_html.window##.localStorage (fun () ->
      failwith "Browser localStorage is unavailable")

(** Retrieves the saved goals document. *)
let load_file key =
  let saved = storage##getItem (Js.string key) in
  if Js.Opt.test saved then
    Js.to_string (Js.Opt.get saved (fun () -> Js.string ""))
  else ""

(** Save the complete goals document after every goal change with content. *)
let save_file key content =
  storage##setItem (Js.string key) (Js.string content)

(** Retrieves the DOM element with the given string id. *)
let element id = Dom_html.getElementById id

(** Retrieves the input DOM element with the given string id. Raises Failure if given string id does not exist or cannot be coerced into an inputElement *)
let input id =
  Js.Opt.get
    (Dom_html.CoerceTo.input (element id))
    (fun () -> failwith ("Missing input: " ^ id))

(** Get the value of an input element as a string. *)
let value id = Js.to_string (input id)##.value

(** Sets the value of the input with string id to value*)
let set_value id value = (input id)##.value := Js.string value

(** Set the text of a DOM node to value. *)
let text node value = node##.textContent := Js.some (Js.string value)

(** Append a child node to a parent node. *)
let append parent child = Dom.appendChild parent child

(** Returns a new DOM element with the given tag and text content. *)
let node tag content =
  let result = document##createElement (Js.string tag) in
  text result content;
  result

(** Display value on the page*)
let message value = text (element "message") value

(** Attach a callback function to a button's click event. *)
let click button callback =
  button##.onclick :=
    Dom_html.handler (fun _ ->
        callback ();
        Js._false)

(** Create a button with the given label and click callback function. *)
let button label callback =
  let result = Dom_html.createButton document in
  result##setAttribute (Js.string "type") (Js.string "button");
  text result label;
  click result callback;
  result

(** Format a number as USD. *)
let money amount = Printf.sprintf "$%.2f" amount

(** Clears the goal form and exit editing mode. *)
let reset_form () =
  editing := None;
  List.iter
    (fun id -> set_value id "")
    [ "goal-name"; "goal-target"; "goal-deadline" ];
  text (element "save-goal") "Create goal";
  (element "cancel-edit")##setAttribute (Js.string "hidden") (Js.string "")

(** Applies a goal update. *)
let rec apply result =
  match result with
  | Error error -> message error
  | Ok updated ->
      goals := updated;
      message "Saved for this session.";
      render ()

(** Renders the current savings goals and their controls. *)
and render () =
  let container = element "goals" in
  text container "";
  if !goals = [] then
    append container
      (node "p" "No savings goals yet. Create your first goal above.");
  List.iter
    (fun goal ->
      let card = node "article" "" in
      card##.className := Js.string "goal";
      let p = S.progress goal in
      append card (node "h3" (S.name goal));
      append card
        (node "p"
           (match S.deadline goal with
           | None -> "No deadline"
           | Some date -> "Deadline: " ^ date));
      append card
        (node "p"
           (Printf.sprintf "%s saved of %s · %s remaining" (money p.saved)
              (money (S.target goal))
              (money p.remaining)));
      let bar = node "progress" "" in
      bar##setAttribute (Js.string "max") (Js.string "100");
      bar##setAttribute (Js.string "value")
        (Js.string (string_of_float p.percent));
      bar##setAttribute (Js.string "aria-label")
        (Js.string (S.name goal ^ " savings progress"));
      append card bar;
      append card
        (node "p"
           (Printf.sprintf "%.1f%% complete%s" p.percent
              (if p.complete then " — Goal reached!" else "")));
      let label = node "label" "Contribution amount ($) " in
      let amount = Dom_html.createInput document in
      amount##setAttribute (Js.string "type") (Js.string "number");
      amount##setAttribute (Js.string "min") (Js.string "0.01");
      amount##setAttribute (Js.string "step") (Js.string "0.01");
      append label amount;
      append card label;
      append card
        (button "Add contribution" (fun () ->
             match
               Spendwise.Transaction.parse_amount (Js.to_string amount##.value)
             with
             | Error error -> message error
             | Ok amount -> apply (S.contribute (S.id goal) ~amount !goals); save_file storage_file (S.string_of_goals !goals)));
      append card
        (node "p"
           ("Contributions: "
           ^
           match S.contributions goal with
           | [] -> "None yet"
           | amounts -> String.concat ", " (List.map money amounts)));
      append card
        (button "Edit" (fun () ->
             editing := Some (S.id goal);
             set_value "goal-name" (S.name goal);
             set_value "goal-target" (Printf.sprintf "%.2f" (S.target goal));
             set_value "goal-deadline"
               (Option.value ~default:"" (S.deadline goal));
             text (element "save-goal") "Save changes";
             (element "cancel-edit")##removeAttribute (Js.string "hidden");
             (input "goal-name")##focus;
             message "Editing this goal. Contributions will be preserved."));
      append card
        (button "Delete" (fun () ->
             if
               Js.to_bool
                 (Dom_html.window##confirm
                    (Js.string
                       ("Delete " ^ S.name goal
                      ^ " and its contribution history?")))
             then (
               if !editing = Some (S.id goal) then reset_form ();
               apply (S.delete_goal (S.id goal) !goals))));
      append container card)
    !goals

(** Checks if the goal form is valid and creates or updates a savings goal. *)
let save () =
  match Spendwise.Transaction.parse_amount (value "goal-target") with
  | Error error -> message error
  | Ok target -> (
      let name = value "goal-name" in
      let deadline =
        match String.trim (value "goal-deadline") with
        | "" -> None
        | date -> Some date
      in
      let result =
        match !editing with
        | Some id -> S.edit_goal id ~name ~target ~deadline !goals
        | None ->
            Result.bind (S.next_goal_id !goals) (fun id ->
                Result.bind (S.create_goal ~id ~name ~target ~deadline)
                  (fun goal -> S.add_goal goal !goals))
      in
      match result with
      | Error error -> message error
      | Ok _ ->
          reset_form ();
          apply result;
          save_file storage_file (S.string_of_goals !goals))

let () =
  let form =
    Js.Opt.get
      (Dom_html.CoerceTo.form (element "goal-form"))
      (fun () -> failwith "Missing goal form")
  in
  (*Load saved goals *)
  let file_string = load_file storage_file in
  if file_string = "" then
    message "No saved goals found. Create your first goal above."
  else (
    let string_list = String.split_on_char '\n' file_string in
    List.iter
      (fun line ->
        match S.load_goal line with
        | Error error -> message error
        | Ok goal -> apply (S.add_goal goal !goals))
      string_list);
  
  form##.onsubmit :=
    Dom_html.handler (fun _ ->
        save ();
        Js._false);
  click (element "cancel-edit") (fun () ->
      reset_form ();
      message "Edit cancelled.");
  render ()

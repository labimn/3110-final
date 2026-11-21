
(* The two possible transaction types. *)
type transaction_type =
  | Income
  | Expense

(* Represents one financial transaction. *)
type transaction = {
  id : int;
  amount : float;
  date : string;
  description : string;
  category : string;
  transaction_type : transaction_type;
}

(* Parse an amount entered by the user. *)
let parse_amount input =
  try
    let amount = float_of_string (String.trim input) in
    if amount > 0. then Ok amount
    else Error "Amount must be greater than zero."
  with Failure _ ->
    Error "Please enter a valid number."

(* Parse the transaction type entered by the user. *)
let parse_transaction_type input =
  match String.lowercase_ascii (String.trim input) with
  | "income" -> Ok Income
  | "expense" -> Ok Expense
  | _ -> Error "Transaction type must be income or expense."

(* Parse a nonempty text field. *)
let parse_text field_name input =
  let value = String.trim input in
  if value = "" then
    Error (field_name ^ " cannot be empty.")
  else
    Ok value

(* Parse and validate a date in YYYY-MM-DD format. *)
let parse_date input =
  let parts = String.split_on_char '-' (String.trim input) in
  match parts with
  | [year; month; day] ->
      (try
         let y = int_of_string year in
         let m = int_of_string month in
         let d = int_of_string day in
         let leap_year =
           y mod 400 = 0 || (y mod 4 = 0 && y mod 100 <> 0)
         in
         let days_in_month =
           match m with
           | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
           | 4 | 6 | 9 | 11 -> 30
           | 2 -> if leap_year then 29 else 28
           | _ -> 0
         in
         if String.length year <> 4
            || String.length month <> 2
            || String.length day <> 2
            || y < 1
            || d < 1
            || d > days_in_month
         then Error "Please enter a valid date in YYYY-MM-DD format."
         else Ok (Printf.sprintf "%04d-%02d-%02d" y m d)
       with Failure _ ->
         Error "Please enter a valid date in YYYY-MM-DD format.")
  | _ -> Error "Please enter a date in YYYY-MM-DD format."

(* Create a transaction record. *)
let create_transaction id amount date description category transaction_type =
  {
    id;
    amount;
    date;
    description;
    category;
    transaction_type;
  }

(* Add a transaction, preventing duplicate IDs. *)
let add_transaction transaction transactions =
  if List.exists (fun t -> t.id = transaction.id) transactions then
    Error "A transaction with this ID already exists."
  else
    Ok (transactions @ [transaction])

(* Find a transaction by its ID. *)
let get_transaction id transactions =
  List.find_opt (fun t -> t.id = id) transactions

(* Return all transactions. *)
let get_transactions transactions =
  transactions

(* Edit an existing transaction. The original ID is preserved. *)
let edit_transaction id updated_transaction transactions =
  if not (List.exists (fun t -> t.id = id) transactions) then
    Error "Transaction not found."
  else
    let updated_transaction = { updated_transaction with id } in
    Ok (
      List.map
        (fun t ->
          if t.id = id then updated_transaction else t)
        transactions
    )

(* Delete a transaction by its ID. *)
let delete_transaction id transactions =
  if not (List.exists (fun t -> t.id = id) transactions) then
    Error "Transaction not found."
  else
    Ok (List.filter (fun t -> t.id <> id) transactions)

(* Filter transactions by exact date. *)
let filter_by_date date transactions =
  List.filter (fun t -> t.date = date) transactions

(* Filter transactions by category. *)
let filter_by_category category transactions =
  List.filter
    (fun t ->
      String.lowercase_ascii t.category =
      String.lowercase_ascii category)
    transactions

(* Filter transactions by income or expense. *)
let filter_by_type transaction_type transactions =
  List.filter
    (fun t -> t.transaction_type = transaction_type)
    transactions

(* Apply multiple optional filters together. *)
let filter_transactions ?date ?category ?transaction_type transactions =
  transactions
  |> (fun ts ->
       match date with
       | Some d -> filter_by_date d ts
       | None -> ts)
  |> (fun ts ->
       match category with
       | Some c -> filter_by_category c ts
       | None -> ts)
  |> (fun ts ->
       match transaction_type with
       | Some typ -> filter_by_type typ ts
       | None -> ts)

(* Check whether a string contains another string. *)
let contains_substring text query =
  let text = String.lowercase_ascii text in
  let query = String.lowercase_ascii query in
  let text_length = String.length text in
  let query_length = String.length query in

  let rec check position =
    if position + query_length > text_length then
      false
    else if String.sub text position query_length = query then
      true
    else
      check (position + 1)
  in

  query = "" || check 0

(* Search transactions by description. *)
let search_transactions query transactions =
  List.filter
    (fun t -> contains_substring t.description query)
    transactions

(* Generate the next available transaction ID. *)
let next_transaction_id transactions =
  List.fold_left
    (fun max_id t -> max max_id t.id)
    0
    transactions
  + 1
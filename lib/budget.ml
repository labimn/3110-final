(* A budget sets a spending limit for one category in one month. *)
type budget = {
  month : string;
  category : string;
  limit : float;
}

(* The spending status that the UI can use to display a warning. *)
type warning =
  | Below_threshold
  | Approaching_limit
  | At_limit
  | Over_limit

(* Remaining is negative when spending exceeds the limit. *)
type summary = {
  spent : float;
  remaining : float;
  warning : warning;
}

let is_digit c = c >= '0' && c <= '9'

(* Check the YYYY-MM format, require a nonzero year, and allow
   month numbers from 01 through 12. The length check makes the
   string indexing and substring operations safe. *)
let valid_month month =
  String.length month = 7
  && month.[4] = '-'
  && String.for_all is_digit (String.sub month 0 4)
  && String.for_all is_digit (String.sub month 5 2)
  && String.sub month 0 4 <> "0000"
  && (let number = int_of_string (String.sub month 5 2) in
      number >= 1 && number <= 12)

(* Validate the inputs before constructing a budget. *)
let create_budget ~month ~category ~limit =
  let month = String.trim month in
  let category = String.trim category in
  if not (valid_month month) then
    Error "Please enter a month in YYYY-MM format."
  else if category = "" then
    Error "Category cannot be empty."
  else if not (Float.is_finite limit) || limit <= 0. then
    Error "Budget limit must be a positive, finite number."
  else
    Ok { month; category; limit }

(* Accessors let callers read the fields of the abstract budget type. *)
let month budget = budget.month
let category budget = budget.category
let limit budget = budget.limit

(* Normalize categories for comparisons while preserving the original
   spelling in the budget record. *)
let normalize_category category =
  String.lowercase_ascii (String.trim category)

(* A budget is identified by its month and category together. *)
let matches_budget ~month ~category budget =
  budget.month = String.trim month
  && normalize_category budget.category = normalize_category category

let get_budget ~month ~category budgets =
  List.find_opt (matches_budget ~month ~category) budgets

(* Allow at most one budget for each month and category pair. *)
let add_budget budget budgets =
  match get_budget ~month:budget.month ~category:budget.category budgets with
  | Some _ -> Error "A budget already exists for this month and category."
  | None -> Ok (budgets @ [budget])

(* Find the original budget using its current month and category.
   Check whether the replacement would collide with another budget. *)
let edit_budget ~month ~category replacement budgets =
  let matches_original = matches_budget ~month ~category in
  if not (List.exists matches_original budgets) then
    Error "Budget not found."
  else
    let conflicts budget =
      not (matches_original budget)
      && matches_budget ~month:replacement.month
           ~category:replacement.category budget
    in
    if List.exists conflicts budgets then
      Error "A budget already exists for this month and category."
    else
      Ok (List.map
        (fun budget -> if matches_original budget then replacement else budget)
        budgets)

(* Remove the matching budget. If the list length stays the same,
   there was no budget with that month and category. *)
let delete_budget ~month ~category budgets =
  let remaining_budgets =
    List.filter
      (fun budget -> not (matches_budget ~month ~category budget))
      budgets
  in
  if List.length remaining_budgets = List.length budgets then
    Error "Budget not found."
  else
    Ok remaining_budgets

(* Add up expenses in the budget's month and category, then calculate
   the remaining amount and spending status. *)
let summarize budget transactions =
  let category = normalize_category budget.category in
  (* Transaction dates use YYYY-MM-DD, so YYYY-MM- matches the month. *)
  let date_prefix = budget.month ^ "-" in
  let add_expense total (transaction : Transaction.transaction) =
    if transaction.transaction_type = Transaction.Expense
       && String.starts_with ~prefix:date_prefix transaction.date
       && normalize_category transaction.category = category
    then total +. transaction.amount
    else total
  in
  let spent = List.fold_left add_expense 0. transactions in
  let remaining = budget.limit -. spent in
  (* Check the limit first because spending at or above it also
     satisfies the 80% warning threshold. *)
  let warning =
    if spent > budget.limit then Over_limit
    else if spent = budget.limit then At_limit
    else if spent >= 0.8 *. budget.limit then Approaching_limit
    else Below_threshold
  in
  { spent; remaining; warning }
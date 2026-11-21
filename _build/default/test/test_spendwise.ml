
open Spendwise.Transaction

let expect_ok = function
  | Ok value -> value
  | Error message -> failwith message

let sample_transaction id amount description category typ =
  create_transaction id amount "2026-11-20"
    description category typ

(* Test transaction creation and adding. *)
let test_add_transaction () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let transactions = expect_ok (add_transaction t1 []) in
  assert (List.length transactions = 1);
  assert (Result.is_error (add_transaction t1 transactions))

(* Test finding and viewing transactions. *)
let test_view_transactions () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let t2 = sample_transaction 2 100. "Salary" "Work" Income in
  let transactions = [t1; t2] in
  assert (List.length (get_transactions transactions) = 2);
  assert (get_transaction 1 transactions = Some t1);
  assert (get_transaction 3 transactions = None)

(* Test editing a transaction. *)
let test_edit_transaction () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let updated =
    sample_transaction 1 40. "Weekly groceries" "Food" Expense
  in
  let transactions = [t1] in
  let result = expect_ok (edit_transaction 1 updated transactions) in
  assert (List.length result = 1);
  assert ((List.hd result).amount = 40.);
  assert ((List.hd result).id = 1);
  assert (Result.is_error (edit_transaction 99 updated transactions))

(* Test deleting a transaction. *)
let test_delete_transaction () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let t2 = sample_transaction 2 100. "Salary" "Work" Income in
  let transactions = [t1; t2] in
  let result = expect_ok (delete_transaction 1 transactions) in
  assert (List.length result = 1);
  assert (get_transaction 1 result = None);
  assert (Result.is_error (delete_transaction 99 transactions))

(* Test individual filters. *)
let test_filters () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let t2 = sample_transaction 2 100. "Salary" "Work" Income in
  let t3 = create_transaction 3 15. "2026-11-19"
      "Lunch" "Food" Expense in
  let transactions = [t1; t2; t3] in

  assert (List.length (filter_by_date "2026-11-20" transactions) = 2);
  assert (List.length (filter_by_category "Food" transactions) = 2);
  assert (List.length (filter_by_type Income transactions) = 1);
  assert (List.length (filter_by_type Expense transactions) = 2);

  let combined =
    filter_transactions ~category:"Food" ~transaction_type:Expense
      transactions
  in
  assert (List.length combined = 2);

  let dated =
    filter_transactions ~date:"2026-11-20" ~category:"Food"
      transactions
  in
  assert (List.length dated = 1)

(* Test searching descriptions. *)
let test_search_transactions () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let t2 = sample_transaction 2 100. "Monthly salary" "Work" Income in
  let transactions = [t1; t2] in

  assert (List.length (search_transactions "grocery" transactions) = 0);
  assert (List.length (search_transactions "grocer" transactions) = 1);
  assert (List.length (search_transactions "SALARY" transactions) = 1);
  assert (List.length (search_transactions "" transactions) = 2)

(* Test input parsing. *)
let test_parsing () =
  assert (parse_amount "25.50" = Ok 25.50);
  assert (Result.is_error (parse_amount "-5"));
  assert (Result.is_error (parse_amount "abc"));
  assert (parse_transaction_type "income" = Ok Income);
  assert (parse_transaction_type "expense" = Ok Expense);
  assert (Result.is_error (parse_transaction_type "invalid"));
  assert (parse_text "Description" "Groceries" = Ok "Groceries");
  assert (Result.is_error (parse_text "Description" ""));
  assert (parse_date "2024-02-29" = Ok "2024-02-29");
  assert (Result.is_error (parse_date "2025-02-29"))

let test_next_transaction_id () =
  let t1 = sample_transaction 1 25.50 "Groceries" "Food" Expense in
  let t2 = sample_transaction 3 100. "Salary" "Work" Income in

  assert (next_transaction_id [] = 1);
  assert (next_transaction_id [t1] = 2);
  assert (next_transaction_id [t1; t2] = 4)

let () =
  test_add_transaction ();
  test_view_transactions ();
  test_edit_transaction ();
  test_delete_transaction ();
  test_filters ();
  test_search_transactions ();
  test_parsing ();
  test_next_transaction_id ();
  print_endline "All SpendWise transaction tests passed!"


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

module Budget = Spendwise.Budget

let november_food_budget limit =
  expect_ok (Budget.create_budget ~month:"2026-11" ~category:"Food" ~limit)

(* Test budget creation and input validation. *)
let test_budget_validation () =
  let budget = november_food_budget 100. in
  assert (Budget.month budget = "2026-11");
  assert (Budget.category budget = "Food");
  assert (Budget.limit budget = 100.);
  assert (Result.is_error
    (Budget.create_budget ~month:"2026-13" ~category:"Food" ~limit:100.));
  assert (Result.is_error
    (Budget.create_budget ~month:"2026-11" ~category:"  " ~limit:100.));
  assert (Result.is_error
    (Budget.create_budget ~month:"2026-11" ~category:"Food" ~limit:0.))

(* Test lookup and uniqueness by month and category. *)
let test_add_and_get_budgets () =
  let november_food = november_food_budget 100. in
  let budgets = expect_ok (Budget.add_budget november_food []) in
  assert (Budget.get_budget ~month:"2026-11" ~category:"food" budgets
    = Some november_food);

  let duplicate =
    expect_ok (Budget.create_budget ~month:"2026-11" ~category:"food" ~limit:150.)
  in
  assert (Result.is_error (Budget.add_budget duplicate budgets));

  let december_food =
    expect_ok (Budget.create_budget ~month:"2026-12" ~category:"Food" ~limit:100.)
  in
  let november_transport =
    expect_ok (Budget.create_budget
      ~month:"2026-11" ~category:"Transport" ~limit:75.)
  in
  let budgets = expect_ok (Budget.add_budget december_food budgets) in
  let budgets = expect_ok (Budget.add_budget november_transport budgets) in
  assert (List.length budgets = 3);
  assert (Budget.get_budget ~month:"2026-12" ~category:"Food" budgets
    = Some december_food);
  assert (Budget.get_budget ~month:"2026-11" ~category:"Transport" budgets
    = Some november_transport)

(* Test updating a limit, rejecting a conflict, and deleting a budget. *)
let test_edit_and_delete_budget () =
  let food = november_food_budget 100. in
  let transport =
    expect_ok (Budget.create_budget
      ~month:"2026-11" ~category:"Transport" ~limit:75.)
  in
  let updated_food = november_food_budget 150. in
  let budgets =
    expect_ok (Budget.edit_budget ~month:"2026-11" ~category:"food"
      updated_food [food; transport])
  in
  assert (Budget.get_budget ~month:"2026-11" ~category:"Food" budgets
    = Some updated_food);
  assert (Result.is_error
    (Budget.edit_budget ~month:"2026-11" ~category:"Food" transport budgets));
  assert (Result.is_error
    (Budget.edit_budget ~month:"2026-12" ~category:"Food" updated_food budgets));

  let remaining =
    expect_ok (Budget.delete_budget ~month:"2026-11" ~category:"food" budgets)
  in
  assert (remaining = [transport]);
  assert (Budget.get_budget ~month:"2026-11" ~category:"Food" remaining = None);
  assert (Result.is_error
    (Budget.delete_budget ~month:"2026-11" ~category:"Food" remaining))

(* Count matching expenses across days, using case-insensitive categories. *)
let test_budget_summary () =
  let budget = november_food_budget 100. in
  let transactions = [
    sample_transaction 1 25. "Groceries" "food" Expense;
    create_transaction 2 25. "2026-11-01" "Lunch" "Food" Expense;
    sample_transaction 3 30. "Income" "Food" Income;
    create_transaction 4 25. "2026-12-01" "Groceries" "Food" Expense;
    sample_transaction 5 20. "Bus pass" "Transport" Expense;
  ] in
  let summary = Budget.summarize budget transactions in
  assert (summary.spent = 50.);
  assert (summary.remaining = 50.);
  assert (summary.warning = Budget.Below_threshold)

(* Check spending below, at, and above each warning boundary. *)
let test_budget_warnings () =
  let budget = november_food_budget 100. in
  let summarize_expense amount =
    Budget.summarize budget
      [sample_transaction 1 amount "Groceries" "Food" Expense]
  in
  let empty = Budget.summarize budget [] in
  assert (empty.spent = 0.);
  assert (empty.remaining = 100.);
  assert (empty.warning = Budget.Below_threshold);
  assert ((summarize_expense 79.99).warning = Budget.Below_threshold);
  assert ((summarize_expense 80.).warning = Budget.Approaching_limit);
  assert ((summarize_expense 100.).warning = Budget.At_limit);
  let over = summarize_expense 110. in
  assert (over.warning = Budget.Over_limit);
  assert (over.remaining = -10.)

let () =
  test_add_transaction ();
  test_view_transactions ();
  test_edit_transaction ();
  test_delete_transaction ();
  test_filters ();
  test_search_transactions ();
  test_parsing ();
  test_next_transaction_id ();
  test_budget_validation ();
  test_add_and_get_budgets ();
  test_edit_and_delete_budget ();
  test_budget_summary ();
  test_budget_warnings ();
  print_endline "All SpendWise transaction tests passed!"

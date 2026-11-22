open Transaction

type period =
  | Weekly
  | Monthly
  | Yearly

type overview = {
  total_income : float;
  total_expenses : float;
  remaining_balance : float;
}

type category_breakdown = {
  category : string;
  amount : float;
  percentage : float;
}

type period_summary = {
  period : period;
  label : string;
  overview : overview;
  expenses_by_category : category_breakdown list;
}

(* Sum transactions matching the given type. *)
let sum_by_type tx_type transactions =
  List.fold_left
    (fun acc t -> if t.transaction_type = tx_type then acc +. t.amount else acc)
    0.0 transactions

(* Compute total income, expenses, and remaining balance. *)
let compute_overview transactions =
  let total_income = sum_by_type Income transactions in
  let total_expenses = sum_by_type Expense transactions in
  let remaining_balance = total_income -. total_expenses in
  { total_income; total_expenses; remaining_balance }

(* Parse YYYY-MM-DD into (year, month, day). *)
let parse_ymd date_str =
  match Transaction.parse_date date_str with
  | Error msg -> Error msg
  | Ok valid_date -> (
      match String.split_on_char '-' valid_date with
      | [ y; m; d ] -> (
          try Ok (int_of_string y, int_of_string m, int_of_string d)
          with Failure _ -> Error "Invalid date format.")
      | _ -> Error "Invalid date format.")

(* Days since epoch for week range checks. *)
let days_since_epoch (y, m, d) =
  let y_adj = if m <= 2 then y - 1 else y in
  let m_adj = if m <= 2 then m + 12 else m in
  let era = (if y_adj >= 0 then y_adj else y_adj - 399) / 400 in
  let yoe = y_adj - (era * 400) in
  let doy = ((153 * (m_adj + (if m_adj > 2 then -3 else 9))) + 2) / 5 + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

(* Filter transactions for a period ending on [anchor_date]. *)
let filter_by_period period ~anchor_date transactions =
  match parse_ymd anchor_date with
  | Error msg -> Error msg
  | Ok ((ay, am, _) as anchor_ymd) -> (
      let anchor_days = days_since_epoch anchor_ymd in
      let keep t =
        match parse_ymd t.date with
        | Error _ -> false
        | Ok ((ty, tm, _) as tx_ymd) -> (
            match period with
            | Weekly ->
                let diff = anchor_days - days_since_epoch tx_ymd in
                diff >= 0 && diff < 7
            | Monthly -> ty = ay && tm = am
            | Yearly -> ty = ay)
      in
      Ok (List.filter keep transactions))

(* Label for the selected summary period. *)
let period_label period (y, m, d) =
  match period with
  | Weekly -> Printf.sprintf "7 days ending %04d-%02d-%02d" y m d
  | Monthly -> Printf.sprintf "%04d-%02d" y m
  | Yearly -> Printf.sprintf "%04d" y

(* Normalize category names for grouping. *)
let normalize_cat cat =
  String.lowercase_ascii (String.trim cat)

(* Add [amount] to [cat] in a running (category * float) list. *)
let rec add_amount cat amount = function
  | [] -> [ (cat, amount) ]
  | (k, v) :: rest ->
      if k = cat then (k, v +. amount) :: rest
      else (k, v) :: add_amount cat amount rest

(* Collect expense totals per category from transactions. *)
let rec collect_expenses acc = function
  | [] -> acc
  | t :: rest ->
      if t.transaction_type = Expense then
        collect_expenses (add_amount (normalize_cat t.category) t.amount acc) rest
      else
        collect_expenses acc rest

(* Group expenses by category, highest amount first. *)
let expense_breakdown transactions =
  let total_exp = sum_by_type Expense transactions in
  let totals = collect_expenses [] transactions in
  let breakdowns =
    List.map
      (fun (category, amount) ->
        let percentage =
          if total_exp > 0.0 then (amount /. total_exp) *. 100.0 else 0.0
        in
        { category; amount; percentage })
      totals
  in
  List.sort (fun a b -> compare b.amount a.amount) breakdowns

(* Summarize totals and category spending for a period. *)
let summarize_period period ~anchor_date transactions =
  match parse_ymd anchor_date with
  | Error msg -> Error msg
  | Ok anchor_ymd -> (
      match filter_by_period period ~anchor_date transactions with
      | Error msg -> Error msg
      | Ok filtered ->
          let overview = compute_overview filtered in
          let expenses_by_category = expense_breakdown filtered in
          let label = period_label period anchor_ymd in
          Ok { period; label; overview; expenses_by_category })

(* Group expenses by YYYY-MM in ascending order. *)
let monthly_spending_trend transactions =
  let rec collect_months acc = function
    | [] -> acc
    | t :: rest ->
        if t.transaction_type = Expense && String.length t.date >= 7 then
          let ym = String.sub t.date 0 7 in
          collect_months (add_amount ym t.amount acc) rest
        else
          collect_months acc rest
  in
  let totals = collect_months [] transactions in
  List.sort (fun (m1, _) (m2, _) -> String.compare m1 m2) totals

(* Summarize budgets for [month] against transactions. *)
let budget_overview ~month budgets transactions =
  let matching =
    List.filter (fun b -> Budget.month b = month) budgets
  in
  List.map
    (fun b -> (Budget.category b, Budget.summarize b transactions))
    matching

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

(** Compute overall income, expenses, and balance from transactions. *)
val compute_overview : Transaction.transaction list -> overview

(** Filter transactions matching a period and anchor date (YYYY-MM-DD). *)
val filter_by_period :
  period -> anchor_date:string -> Transaction.transaction list ->
  (Transaction.transaction list, string) result

(** Summarize spending and income for a period around [anchor_date]. *)
val summarize_period :
  period -> anchor_date:string -> Transaction.transaction list ->
  (period_summary, string) result

(** Breakdown of expense amounts and percentages by category. *)
val expense_breakdown :
  Transaction.transaction list -> category_breakdown list

(** Group expense totals by YYYY-MM month string. *)
val monthly_spending_trend :
  Transaction.transaction list -> (string * float) list

(** Summarize budget usage across all budgets for a given YYYY-MM month. *)
val budget_overview :
  month:string -> Budget.budget list -> Transaction.transaction list ->
  (string * Budget.summary) list

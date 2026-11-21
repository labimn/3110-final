(** A monthly spending limit for one expense category. *)
type budget

type warning =
  | Below_threshold
  | Approaching_limit
  | At_limit
  | Over_limit

type summary = {
  spent : float;
  remaining : float;
  warning : warning;
}

(** [create_budget ~month ~category ~limit] creates a budget.
    [month] must be in YYYY-MM format, [category] must be nonempty,
    and [limit] must be positive and finite. *)
val create_budget :
  month:string -> category:string -> limit:float -> (budget, string) result

val month : budget -> string
val category : budget -> string
val limit : budget -> float

(** [add_budget budget budgets] adds [budget]. Returns an error if a
    budget already exists for the same month and category. Category
    matching is case-insensitive for all budget operations. *)
val add_budget : budget -> budget list -> (budget list, string) result

(** [get_budget ~month ~category budgets] finds a matching budget. *)
val get_budget :
  month:string -> category:string -> budget list -> budget option

(** [edit_budget ~month ~category replacement budgets] replaces the
    matching budget. Returns an error if the original is missing or
    the replacement would conflict with another budget. *)
val edit_budget :
  month:string ->
  category:string ->
  budget ->
  budget list ->
  (budget list, string) result

(** [delete_budget ~month ~category budgets] removes the matching
    budget. Returns an error if it does not exist. *)
val delete_budget :
  month:string -> category:string -> budget list -> (budget list, string) result

(** [summarize budget transactions] counts expenses in the budget's
    month and category. [remaining] is the limit minus spending and
    can be negative. Warnings begin at 80% of the limit. *)
val summarize : budget -> Transaction.transaction list -> summary
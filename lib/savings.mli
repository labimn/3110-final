type goal
(** Savings goals. Lists passed to this module belong to one user; account
    selection and persistence are the storage module's responsibility. *)

type progress = {
  saved : float;
  remaining : float;
  percent : float;
  complete : bool;
}

val create_goal :
  id:int ->
  name:string ->
  target:float ->
  deadline:string option ->
  (goal, string) result
(** IDs are positive. Names are trimmed and nonempty. Targets must be positive,
    finite, and in whole cents, at most $90 trillion. Deadlines, when present,
    are valid YYYY-MM-DD dates. Past deadlines are allowed; a deadline is a
    planning aid, not a lock. *)

val id : goal -> int
val name : goal -> string
val target : goal -> float
val deadline : goal -> string option
val contributions : goal -> float list
val progress : goal -> progress

val add_goal : goal -> goal list -> (goal list, string) result
(** New goals start with no contributions. IDs must be unique within a list. *)

val get_goal : int -> goal list -> goal option
val next_goal_id : goal list -> (int, string) result

val edit_goal :
  int ->
  name:string ->
  target:float ->
  deadline:string option ->
  goal list ->
  (goal list, string) result
(** Editing preserves the ID and contribution history. Targets below the saved
    amount are allowed. Remaining and percent are clamped to 0 and 100. *)

val delete_goal : int -> goal list -> (goal list, string) result

val contribute : int -> amount:float -> goal list -> (goal list, string) result
(** Contributions must be positive and finite; amounts use whole cents and
    totals above $90 trillion are rejected. Contributions above the target are
    allowed. No transaction is created or account balance deducted: allocation
    semantics require team agreement. *)

val add_contribution : goal -> float -> goal
(** Adds a contribution to a goal and returns the updated goal. *)

type transaction_type =
  | Income
  | Expense

type transaction = {
  id : int;
  amount : float;
  date : string;
  description : string;
  category : string;
  transaction_type : transaction_type;
}

val parse_amount : string -> (float, string) result

val parse_transaction_type :
  string -> (transaction_type, string) result

val parse_text : string -> string -> (string, string) result

val parse_date : string -> (string, string) result

val create_transaction :
  int -> float -> string -> string -> string ->
  transaction_type -> transaction

val add_transaction :
  transaction -> transaction list ->
  (transaction list, string) result

val get_transaction :
  int -> transaction list -> transaction option

val get_transactions : transaction list -> transaction list

val edit_transaction :
  int -> transaction -> transaction list ->
  (transaction list, string) result

val delete_transaction :
  int -> transaction list ->
  (transaction list, string) result

val filter_by_date :
  string -> transaction list -> transaction list

val filter_by_category :
  string -> transaction list -> transaction list

val filter_by_type :
  transaction_type -> transaction list -> transaction list

val filter_transactions :
  ?date:string ->
  ?category:string ->
  ?transaction_type:transaction_type ->
  transaction list -> transaction list

val search_transactions :
  string -> transaction list -> transaction list

val next_transaction_id : transaction list -> int
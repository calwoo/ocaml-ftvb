(* chpt 3 exercises *)

let not b =
  match b with
    true -> false
  | false -> true;;

let rec sum_up_to n =
  match n with
    0 -> 0
  | _ -> n + (sum_up_to (n-1));;

let rec power x n =
  match n with
    0 -> 1
  | _ -> x * (power x (n-1));;

let islower c =
  match c with
    'a'..'z' -> true
  | _ -> false;;

let isupper c =
  match c with
    'A'..'Z' -> true
  | _ -> false;;

(* chpt 4 exercises *)

let rec evens l =
  match l with
    [] -> []
  | [a] -> []
  | [a;b] -> [b]
  | _::x::tl -> x :: evens tl;;

let rec count_true l =
  match l with
    [] -> 0
  | true::tl -> 1 + count_true tl
  | false::tl -> count_true tl;;

let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h];;

let palindrome l = l @ rev l;;

let test_palindrome l = l = rev l;;

let rec drop_last l =
  match l with
    [] -> []
  | x::[] -> []
  | x::tl -> x :: drop_last tl;;

let rec drop_last_tail l acc =
  match l with
    [] -> acc
  | x::[] -> acc
  | x::tl -> drop_last_tail tl (acc @ [x]);;

let drop_last_v2 l = drop_last_tail l [];;

let rec member a l =
  match l with
    [] -> false
  | x :: tl -> (a = x) || member a tl;;

let rec make_set l =
  match l with
    [] -> []
  | h::tl -> if member h tl then make_set tl else h :: make_set tl;;

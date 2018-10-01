(* chpt 4 examples *)

let isnil l =
  match l with
    [] -> true
  | _ -> false;;

let rec length l =
  match l with
    [] -> 0
  | _::tl -> 1 + length tl;;

let rec sum l =
  match l with
    [] -> 0
  | h::tl -> h + sum tl;;

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b;;

let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h];;


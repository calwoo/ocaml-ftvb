(* chpt 3 examples *)

let rec factorial n =
  match n with
    1 -> 1
  | _ -> n * factorial (n - 1);;

let isvowel c =
  match c with
    'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | 'u' -> true
  | _ -> false;;

let rec gcd a b =
  match b with
    0 -> a
  | _ -> gcd b (a mod b);;


(* chpt 7 examples *)

let rec take n l =
  match l with
    [] -> if n = 0
          then []
          else raise (Invalid_argument "take")
  | h::tl -> if n < 0
             then raise (Invalid_argument "take") else
               if n = 0 then [] else h :: take (n-1) tl;;

let rec drop n l =
  match l with
    [] -> if n = 0
          then []
          else raise (Invalid_argument "drop")
  | h::tl -> if n < 0
             then raise (Invalid_argument "drop") else
               if n = 0 then l else drop (n-1) tl;;

exception Problem;;

let f x = if x < 0 then raise Problem else 100 / x;;

(* we can declare exceptions and raise them as above
 * or we can just use exception handling
 * ie try..with *)

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0;;

let rec last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _ :: tl -> last tl;;

(* chpt 7 exercises *)

let rec smallest l =
  match l with
    [] -> raise Not_found
  | [x] -> if x < 0 then raise Not_found else x
  | h::x::tl -> if h < 0 then smallest (x::tl) else
                  if x < 0 then smallest (h::tl) else
                    if h < x then smallest (h::tl) else smallest (x::tl);;

let smallest_or_zero l =
  try smallest l with
    Not_found -> 0;;

exception No_roots;;

let rec to_n n =
  if n = 0 then [] else to_n (n-1) @ [n];;
  
let int_sqrt n =
  if n < 0 then raise No_roots else
    List.length (List.filter (fun x -> x >= 0) (List.map (fun x -> n - x*x) (to_n n)));;

let int_sqrt_or_zero n =
  try int_sqrt n with
    No_roots -> 0;;



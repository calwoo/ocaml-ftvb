(* chpt 5 examples *)

(* insertion sort *)
let rec insert x l =
  match l with
    [] -> [x]
  | h::tl -> if x <= h
             then x :: h :: tl
             else h :: (insert x tl);;

let rec sort l =
  match l with
    [] -> []
  | h::tl -> insert h (sort tl);;


(* list helper functions *)
let rec take n l =
  if n = 0 then [] else
    match l with
      h::tl -> h :: take (n-1) tl;;

let rec drop n l =
  match n with
    0 -> l
  | _ -> match l with
           [] -> []
         | h::tl -> drop (n-1) tl;;

(* merge sort *)
let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty -> if hx <= hy
                      then hx :: merge tx (hy::ty)
                      else hy :: merge (hx::tx) ty;;

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ -> let left = take (List.length l / 2) l in
         let right = drop (List.length l / 2) l in
         merge (msort left) (msort right);;


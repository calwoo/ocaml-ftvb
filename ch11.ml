(* chpt 11 examples *)

type 'a tree = Br of 'a * 'a tree * 'a tree
             | Lf
;;

let rec size tr =
  match tr with
    Br (_, l, r) -> 1 + size l + size r
  | Lf -> 0
;;

let rec total tr =
  match tr with
    Br (x, l, r) -> x + total l + total r
  | Lf -> 0
;;

let max x y =
  if x > y then x else y;;

let rec maxdepth tr =
  match tr with
    Br (_, l, r) -> 1 + max (maxdepth l) (maxdepth r)
  | Lf -> 0;;

let rec list_of_tree tr =
  match tr with
    Br (x, l, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)
  | Lf -> [];;

let rec tree_map f tr =
  match tr with
    Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf;;

(* so now we'll implement a binary search tree functionally *)

let rec lookup tr k =
  match tr with
    Lf -> raise Not_found
  | Br ((k',v), l, r) -> if k = k' then v
                         else if k < k' then lookup l k
                         else lookup r k;;
                      
(* to avoid exceptions we use the option type *)
let rec lookup_v2 tr k =
  match tr with
    Lf -> None
  | Br ((k',v), l, r) -> if k = k' then Some v
                         else if k < k' then lookup l k
                         else lookup r k;;

(* insertion into a tree is like the above-- when we reach
 * a leaf, we just insert instead of giving us nothing *)
let rec insert tr k v =
  match tr with
    Lf -> Br((k,v), Lf, Lf)
  | Br ((k',v'), l, r) -> if k = k' then Br((k,v), l, r)
                         else if k < k' then Br((k',v'), insert l k v, r)
                         else Br((k',v'), l, insert r k v);;

(* chpt 11 exercises *)
(* 1: define a function to see if something is in the tree *)
let rec inTree tr k =
  match tr with
    Lf -> false
  | Br(k', l, r) -> (k = k') || (inTree l k) || (inTree r k);;

(* 2: flip tree left to right to become mirror image *)
let rec flipTree tr =
  match tr with
    Lf -> Lf
  | Br(k, l, r) -> Br(k, r, l);;

(* 3: same shape? *)
let rec sameShape atr btr =
  match atr, btr with
    (Lf, Lf) -> true
    (Br(_, l, r), Br(_, l', r')) -> (sameShape l l') && (sameShape r r')
    (_,_) -> false;;

(* 4: build tree representation of dictionary from list representation *)
(* helper function -- filter *)
let rec filter p l =
  match l with
    [] -> []
  | h::tl -> if p h then h :: (filter p tl)
                      else filter p tl;;

let rec tree_of_list d =
  match d with
    [] -> Lf
  | (k,v)::tl -> let lft = filter (fun (k',v') -> k' < k) d in
                  let rght = filter (fun (k',v') -> k' > k) d in
                  Br((k,v), tree_of_list lft, tree_of_list rght);;


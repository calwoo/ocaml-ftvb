(* chpt 6 examples *)

let rec double l =
  match l with
    [] -> []
  | h::tl -> (2*h) :: double tl;;

let rec evens l =
  match l with
    [] -> []
  | h::tl -> (h mod 2 = 0) :: evens tl;;

let rec map f l =
  match l with
    [] -> []
  | h::tl -> (f h) :: map f tl;;

let halve x = x / 2;;

let is_even x = x mod 2 = 0;;

let evens_v2 l =
  map is_even l;;

let evens_v3 l =
  map (fun x -> x mod 2 = 0) l;;

(* mergesort with general comparison operation *)
let greater a b =
  a >= b;;

let rec merge cmp x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty -> if cmp hx hy
                      then hx :: merge cmp tx (hy::ty)
                      else hy :: merge cmp (hx::tx) ty;;

let rec msort cmp l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ -> let left = take (List.length l / 2) l in
         let right = drop (List.length l / 2) l in
         merge cmp (msort cmp left) (msort cmp right);;

(* chp6 exercises *)
let rec calm l =
  match l with
    [] -> []
  | h::tl -> if h = '!' then '.' :: calm tl
             else h :: calm tl;;

let calm_v2 l = map (fun c -> if c = '!' then '.' else c) l;;

let clip i = if i >= 10 then 10 else
               if i <= 1 then 1 else i;;

let cliplist l = map clip l;;

let rec apply f n i = if n = 0 then i else apply f (n-1) (f i);;

let rec filter p l =
  match l with
    [] -> []
  | h::tl -> if p h then h :: filter p tl
             else filter p tl;;

let rec for_all p l =
  match l with
    [] -> true
  | h :: tl -> (p h) && for_all p tl;;

let mapl f ll = map (fun l -> map f l) ll;;











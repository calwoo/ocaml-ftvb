(* chpt 10 examples *)

type color = Red
           | Green
           | Yellow
           | Blue
           | RGB of int * int * int
;;

let col = Blue;;

let cols = [Red; Red; Green; Yellow];;

let colpair = ('R', Red);;

let components c =
  match c with
    Red -> (255,0,0)
  | Green -> (0,255,0)
  | Blue -> (0,0,255)
  | Yellow -> (255,255,0)
  | RGB (r, g, b) -> (r, g, b);;

type 'a option = None | Some of 'a;;

let rec lookup_opt x l =
  match l with
    [] -> None
  | (k,v)::tl -> if x = k then Some v else lookup_opt x tl;;

type 'a sequence = Nil | Cons of 'a * 'a sequence;;

let rec length s =
  match s with
    Nil -> 0
  | Cons (_,tl) -> 1 + length tl;;

let rec append a b =
  match a with
    Nil -> b
  | Cons (h,tl) -> Cons (h, append tl b);;

type expr =
  Num of int
| Add of expr * expr
| Mult of expr * expr
| Subtract of expr * expr
| Divide of expr * expr;;

let rec evaluate e =
  match e with
    Num x -> x
  | Add (e,e') -> (evaluate e) + (evaluate e')
  | Mult (e,e') -> (evaluate e) * (evaluate e')
  | Subtract (e,e') -> (evaluate e) - (evaluate e')
  | Divide (e,e') -> (evaluate e) / (evaluate e');;


(* chpt 10 exercises *)
type rect = Rect of int * int;;

let area (Rect (w,h)) = w*h;;

let rotate (Rect (w,h)) = Rect (h,w);;

let rec take n s =
  match s with
    Nil -> Nil
  | Cons (h,tl) -> if n = 0 then Nil else Cons (h, take (n-1) tl);;

let rec drop n s =
  match s with
    Nil -> Nil
  | Cons (h,tl) -> if n = 0 then s else drop (n-1) tl;;

let rec map f s =
  match s with
    Nil -> Nil
  | Cons (h,tl) -> Cons (f h, map f tl);;





         

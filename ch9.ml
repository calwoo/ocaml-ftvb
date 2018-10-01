(* chpt 9 examples *)

let add x y = x + y;;

let rec mapl f l =
  match l with
    [] -> []
  | h::tl -> List.map f l :: mapl f tl;;

let mapl_v2 f l = List.map (List.map f) l;;

let mapl_v3 f = List.map (List.map f);;

(* chpt 9 exercises *)

(* helper function for first problem *)
let rec member x l =
  match l with
    [] -> false
  | h::tl -> if x = h
             then true
             else member x tl;;

let member_all x ls =
  let member_list = List.map (member x) ls in
  not (member false member_list);;

let (//) a b = b / a;;

let mapll f = List.map (List.map (List.map f));;

let rec take n l =
  match l with
    [] -> []
  | h::tl -> if n = 0 then l else
              h :: take (n-1) tl;;

let truncate n = List.map (take n);;

let list_head l default =
  List.map (fun x -> match x with
                  [] -> default
                | h::_ -> h) l;;

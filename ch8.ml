(* chpt 8 examples *)

let p = (1, 4);;

let q = (1, '1');;

let fst p = match p with (x, _) -> x;;
let snd p = match p with (_, y) -> y;;

let fst_v2 (x, _) = x;;
let snd_v2 (_, y) = y;;

let census = [(1,4); (2,2); (3,2); (4,3); (5,1); (6,2)];;

let y = (1, [2;3;4]);;

let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::tl -> if x = k then v else lookup x tl;;

let rec add k v d =
  match d with
    [] -> [(k,v)]
  | (k',v')::tl -> if k' = k then (k,v) :: tl
                   else (k',v') :: add k v tl;;

let rec remove k d =
  match d with
    [] -> []
  | (k',v')::tl -> if k = k' then tl else (k',v') :: remove k tl;;

let key_exists k d =
  try
    let _ = lookup k d in true
  with
    Not_found -> false;;

(* chpt 8 exercises *)
let key_count d = List.length d;;

let rec replace k v d =
  match d with
    [] -> raise Not_found
  | (k',v')::tl -> if k' = k then (k,v) :: tl
                   else (k',v') :: replace k v tl;;

let rec zip d d' =
  match d, d' with
    [], [] -> []
  | k::t, v::t' -> (k,v) :: zip t t'
  | _ -> raise (Invalid_argument "zip");;

let rec unzip d =
  match d with
    [] -> ([], [])
  | (k,v)::tl -> (k :: fst (unzip tl), v :: snd (unzip tl));;



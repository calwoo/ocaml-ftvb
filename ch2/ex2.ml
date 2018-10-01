(* problems from chpt 2 of 'OCaml from the very beginning' *)


let mult_by_ten x = 10 * x;;

let nonzero x y = x*y > 0;;

let rec pos_sum n =
  if n = 0 then 0 else n + pos_sum (n-1);;

let rec power x n =
  if n = 0 then 1 else x * (power x (n-1));;

(* helper functions from book *)
let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;

let not b = if b then false else true;;

let isconsonant c = not (isvowel c);;

                                  
        

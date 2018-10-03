(* chpt 12 examples *)
(* I/O *)

let print_dict_entry (k,v) =
  print_int k ; print_newline () ; print_string v ; print_newline ();;

(* note that in ocaml I/O, the "do" notation of haskell gets converted to ;
 * and side-effects don't require monadic construction. I think this is because
 * ocaml is strict, and haskell being lazy means we have to be more careful about
 * side effect evaluation? *)

let rec print_dict d =
  match d with
    [] -> ()
  | h::tl -> print_dict_entry h;
             print_dict tl
;;

let rec iter f l =
  match l with
    [] -> ()
  | h::tl -> f h;
             iter f tl
;;

(* then we have *)
let print_dict_v2 = iter print_dict_entry;;

(* to read from the keyboard we have the read functions *)
(* this non-monadic I/O is much more comfy than in hask... *)
let rec read_dict () =
  let i = read_int () in
  if i = 0 then []
  else let name = read_line () in
       (i, name) :: read_dict ()
;;

(* this has exceptions to handle *)
let rec read_dict_v2 () =
  try
    let i = read_int () in
    if i = 0 then []
    else let name = read_line () in
         (i, name) :: read_dict_v2 ()
  with
    Failure "int_of_string" ->
    print_string "This is not a valid integer. Try again. Ha!";
    print_newline();
    read_dict ()
;;

(* reading/writing files *)
let entry_to_channel ch (k,v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'
;;

let dictionary_to_channel ch d =
  iter (entry_to_channel ch) d
;;

(* above, ch is an output channel where the I/O streams originate from *)
(* to get such a channel, we use open_out :: string -> out_channel *)
let dictionary_to_file filename dict =
  let ch = open_out filename in
  dictionary_to_channel ch dict;
  close_out ch
;;

(* this should print a txt file in this directory *)
(* we can try reading this file back in *)
let entry_of_channel ch =
  let number = input_line ch in
  let name = input_line ch in
  (int_of_string number, name)
;;

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in
    e :: dictionary_of_channel ch
  with
    End_of_file -> []
;;

let dictionary_of_file filename =
  let ch = open_in filename in
  let dict = dictionary_of_channel ch in
  close_in ch;
  dict
;;

(* chpt 12 exercises *)
(* 1: function to print a list of integers to screen in same format as
 * OCaml -- ie square brackets and ;'s *)
let rec print_list_ints_inner ls =
  match ls with
    [] -> ()
  | [h] -> print_int h
  | h::tl -> print_int h;
             print_string "; ";
             print_list_ints_inner tl
;;

let print_list_ints ls =
  print_char '[';
  print_list_ints_inner ls;
  print_char ']'
;;

(* 2: function to read three integers from the user, return as a tuple *)
let rec three_ints_to_tuple () =
  try
    let a = read_int () in
    let b = read_int () in
    let c = read_int () in
    (a,b,c)
  with
    Failure "int_of_string" -> print_string "Not a valid integer sequence.";
                               three_ints_to_tuple ()
;;

(* 5: function to count number of lines in a given file *)
let rec count_lines_ch ch =
  try
    input_line ch;
    1 + count_lines_ch ch
  with
    End_of_file -> 0
;;

let count_lines filename =
  let ch = open_in filename in
  count_lines_ch ch
;;

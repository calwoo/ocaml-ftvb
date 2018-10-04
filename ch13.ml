(* chpt 13 notes *)
(* putting things into boxes *)

(* this chapter is about mutable state *)
(* OCaml provides things called references-- these are effectively boxes which
 * we can store values in. we use the function ref :: \alpha -> \alpha ref *)
let x = ref 0;;

(* to extract the value from a ref we use ! *)
let p = !x;;

(* using := updates the reference to a new value *)
x := 50;;

(* function to swap values of references *)
let swap a b =
  let t = !a in
  a := !b;
  b := t
;;

(* this allows us to use imperative programming techniques in OCaml *)
for x = 1 to 5 do print_int x; print_newline () done;;

let smallest_pow2 x =
  let t = ref 1 in
  while !t < x do
    t := !t * 2
  done;
  !t
;;

(* extended example: text statistics *)
(* we have a file saved as gregor.txt that has a selection of text in it *)
(* we want a function to count lines from the file *)
let channel_statistics in_channel =
  let lines = ref 0 in
  try
    while true do
      let line = input_line in_channel in
      lines := !lines + 1
    done
  with
    End_of_file -> print_string "There were ";
                   print_int !lines;
                   print_string " lines.";
                   print_newline ()
;;

let file_statistics filename =
  let channel = open_in filename in
  try
    channel_statistics channel;
    close_in channel
  with
    _ -> close_in channel
;;

(* update the program to count #words, chars, sentences *)
let file_statistics filename =
  let channel = open_in filename in
  try
    channel_statistics channel;
    close_in channel
  with
    _ -> close_in channel
;;
let channel_statistics_v2 in_channel =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  try
    while true do
      let line = input_line in_channel in
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter
        (fun c ->
          match c with
            '.' | '?' | '!' -> sentences := !sentences + 1
            | ' ' -> words := !words + 1
            | _ -> ()) line
    done
  with
    End_of_file -> print_string "There were ";
                   print_int !lines;
                   print_string " lines, making up ";
                   print_int !characters;
                   print_string " characters with ";
                   print_int !words;
                   print_string " words in ";
                   print_int !sentences;
                   print_string " sentences.";
                   print_newline ()
;;

let file_statistics_v2 filename =
  let channel = open_in filename in
  try
    channel_statistics_v2 channel;
    close_in channel
  with
    _ -> close_in channel
;;

(* we want to next add a histogram using arrays *)
let a = [|1;2;3;4;5|];;

(* arrays are mutable lists *)
a.(0);;
a.(4) <- 100;;
a;;

Array.length a;;

Array.make 6 true;;

let print_histogram arr =
  print_string "Character frequencies:";
  print_newline ();
  for x = 0 to 255 do
    if arr.(x) > 0 then
      begin
        print_string "For character '";
        print_char (char_of_int x);
        print_string "'(character number ";
        print_int x;
        print_string ") the count is ";
        print_int arr.(x);
        print_string ".";
        print_newline ()
      end
  done
;;

let channel_statistics_v3 in_channel =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try
    while true do
      let line = input_line in_channel in
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter
        (fun c ->
          match c with
            '.' | '?' | '!' -> sentences := !sentences + 1
            | ' ' -> words := !words + 1
            | _ -> ()) line;
      String.iter (fun c ->
          let i = int_of_char c in
          histogram.(i) <- histogram.(i) + 1) line
    done
  with
    End_of_file -> print_string "There were ";
                   print_int !lines;
                   print_string " lines, making up ";
                   print_int !characters;
                   print_string " characters with ";
                   print_int !words;
                   print_string " words in ";
                   print_int !sentences;
                   print_string " sentences.";
                   print_newline ();
                   print_histogram histogram
;;

let file_statistics_v3 filename =
  let channel = open_in filename in
  try
    channel_statistics_v3 channel;
    close_in channel
  with
    _ -> close_in channel
;;

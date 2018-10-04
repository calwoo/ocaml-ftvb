(* chpt 16 notes *)
(* building bigger programs *)
(* we work with modules *)

(* this is a text statistics module *)

type stats = int * int * int * int;;

let lines (l, _, _, _) = l;;

let characters (_, c, _, _) = c;;

let words (_, _, w, _) = w;;

let sentences (_, _, _, s) = s;;

let stats_from_channel in_channel =
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
    done;
    (0, 0, 0, 0) (* just to make the type agree *)
  with
    End_of_file -> (!lines, !characters, !words, !sentences)
;;

(* read statistics, given a filename. exceptions are not handled *)
let stats_from_file filename =
  let channel = open_in filename in
  let result = stats_from_channel channel in
  close_in channel;
  result
;;


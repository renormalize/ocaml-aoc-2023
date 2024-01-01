(* Read the lines of the file into a string list*)
let lines name = In_channel.input_lines (open_in name)
let is_digit d = d >= '0' && d <= '9'
let to_digit c = Char.code c - Char.code '0'

(* First digit and last digit *)
let fst_lst s =
  let rec first_digit s i =
    if is_digit s.[i] then s.[i] else first_digit s (i + 1)
  in
  let rec last_digit s i =
    if is_digit s.[i] then s.[i] else last_digit s (i - 1)
  in
  (first_digit s 0, last_digit s (String.length s - 1))

let calib_value s =
  let fst, lst = fst_lst s in
  (10 * to_digit fst) + to_digit lst

let digit_words =
  [
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

(* iterate through the string character by character and find the possible numbers*)

let substr s i = String.sub s i (String.length s - i)

let string_begins_with s i =
  List.find_map
    (fun (word, value) ->
      if String.starts_with ~prefix:word (substr s i) then Some value else None)
    digit_words

let match_digit s i =
  if is_digit s.[i] then Some (to_digit s.[i]) else string_begins_with s i

let find_vals line =
  let rec first i =
    match match_digit line i with Some digit -> digit | None -> first (i + 1)
  in
  let rec last i =
    match match_digit line i with Some digit -> digit | None -> last (i - 1)
  in
  let first_digit = first 0 in
  let last_digit = last (String.length line - 1) in
  (first_digit * 10) + last_digit

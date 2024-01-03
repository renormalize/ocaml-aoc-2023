(* read_lines reads each line in the file into a string list*)
let read_lines file = In_channel.input_lines (In_channel.open_text file)
let first_digit line = line.[Str.search_forward (Str.regexp "[0-9]") line 0]

let last_digit line =
  line.[Str.search_backward (Str.regexp "[0-9]") line (String.length line - 1)]

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
let to_digit c = Char.code c - Char.code '0'

let part1_calibration_sum file =
  let calibration_value line =
    (10 * to_digit (first_digit line)) + to_digit (last_digit line)
  in
  let calibration_list file = List.map calibration_value (read_lines file) in
  List.fold_left ( + ) 0 (calibration_list file)

let word_to_digit =
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

let rec check_start line word_map i =
  match word_map with
  | [] -> None
  | h :: t ->
      if
        String.starts_with ~prefix:(fst h)
          (String.sub line i (String.length line - i))
      then Some (snd h)
      else check_start line t i

let rec check_end line word_map i =
  match word_map with
  | [] -> None
  | h :: t ->
      if String.ends_with ~suffix:(fst h) (String.sub line 0 (i + 1)) then
        Some (snd h)
      else check_end line t i

let rec parse_first_digit line i =
  if is_digit line.[i] then to_digit line.[i]
  else
    match check_start line word_to_digit i with
    | None -> parse_first_digit line (i + 1)
    | Some a -> a

let rec parse_last_digit line i =
  if is_digit line.[i] then to_digit line.[i]
  else
    match check_end line word_to_digit i with
    | None -> parse_last_digit line (i - 1)
    | Some a -> a

let vals line =
  (parse_first_digit line 0, parse_last_digit line (String.length line - 1))

let part2_calibration_sum file =
  let calibration_value line = (10 * fst (vals line)) + snd (vals line) in
  let calibration_list file = List.map calibration_value (read_lines file) in
  List.fold_left ( + ) 0 (calibration_list file)

(* Read the lines of the file into a string list*)
let lines name = In_channel.input_lines (open_in name)
let is_digit d = d >= '0' && d <= '9'

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
  (10 * (Char.code fst - Char.code '0')) + (Char.code lst - Char.code '0')

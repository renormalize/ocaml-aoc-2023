let lines file = In_channel.input_lines (In_channel.open_text file)

(*returns the floor of the smaller root and the ceil of the larger root*)
let roots_quadratic_sanitized a b c =
  ( ceil ((-.b -. sqrt ((b *. b) -. (4. *. a *. c))) /. (2. *. a)),
    floor ((-.b +. sqrt ((b *. b) -. (4. *. a *. c))) /. (2. *. a)) )

(*extracts the digits from the input strings*)
let extract_digits line =
  List.map
    (fun x -> float_of_int (int_of_string x))
    (List.tl
       (List.filter
          (fun x -> not (String.equal "" x))
          (String.split_on_char ' ' line)))

(*returns the no of possibilites given the time and the distance to beat*)
let no_possibilities time distance =
  let minus_one = -1. in
  let roots = roots_quadratic_sanitized minus_one time (-.distance) in
  int_of_float (fst roots) - int_of_float (snd roots) - 1

(*solution with the spaces in the input considered*)
let solve_string_with_spaces file =
  let time_and_distance = lines file in
  let times = extract_digits (List.hd time_and_distance) in
  let distances = extract_digits (List.hd (List.tl time_and_distance)) in
  let solutions =
    List.map2
      (fun time distance -> no_possibilities time distance)
      times distances
  in
  List.fold_left ( * ) 1 solutions

(*solution with the spaces in the input not considered*)
let solve_string_without_spaces file =
  let time_and_distance = lines file in
  let time =
    List.hd time_and_distance |> String.split_on_char ':' |> List.tl |> List.hd
    |> Str.global_replace (Str.regexp " ") ""
    |> float_of_string
  in
  let distance =
    List.hd (List.tl time_and_distance)
    |> String.split_on_char ':' |> List.tl |> List.hd
    |> Str.global_replace (Str.regexp " ") ""
    |> int_of_string |> float_of_int
  in
  no_possibilities time distance

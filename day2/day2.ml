let input_line_list file = In_channel.input_lines (In_channel.open_text file)

let validate_trials sets =
  let validate_set set =
    let validate_color s =
      let balls_and_color = String.split_on_char ' ' s in
      let balls, colour =
        (int_of_string (List.nth balls_and_color 0), List.nth balls_and_color 1)
      in
      match colour with
      | "red" -> balls <= 12
      | "green" -> balls <= 13
      | "blue" -> balls <= 14
      | _ -> false
    in
    let ball_list_in_set =
      List.map String.trim (String.split_on_char ',' set)
    in
    List.fold_left ( && ) true (List.map validate_color ball_list_in_set)
  in
  List.fold_left ( && ) true
    (List.map validate_set (String.split_on_char ';' sets))

let is_valid_game s =
  let sets = String.split_on_char ':' s in
  let day, trials =
    (int_of_string (Str.string_after (List.nth sets 0) 5), List.nth sets 1)
  in
  match validate_trials trials with true -> Some day | false -> None

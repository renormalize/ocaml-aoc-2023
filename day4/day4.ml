let lines file = In_channel.input_lines (In_channel.open_text file)

let parse_ints s =
  String.split_on_char ' ' s
  |> List.filter (fun s -> not (String.equal s ""))
  |> List.map (fun s -> int_of_string s)

let winning_and_had card_string =
  let both_number_sets =
    match String.split_on_char ':' card_string with
    | [ _; t ] -> t
    | _ -> failwith "Unexpected input!"
  in
  let winning, had =
    match String.split_on_char '|' both_number_sets with
    | [ h; t ] -> (String.trim h, String.trim t)
    | _ -> failwith "Unexpected input!"
  in
  (parse_ints winning, parse_ints had)

let pow a n =
  let rec pow_rec = function
    | 0 -> 1
    | 1 -> a
    | n ->
        let b = pow_rec (n / 2) in
        b * b * if n mod 2 = 0 then 1 else a
  in
  if n >= 0 then pow_rec n else 0

let no_cards_present (winning, had) =
  List.filter (fun card -> List.mem card winning) had |> List.length

let score_card card_string =
  pow 2 (no_cards_present (winning_and_had card_string) - 1)

let total_points file =
  List.fold_left ( + ) 0 (List.map score_card (lines file))

let create_array string_lines = Array.make (List.length string_lines) 1

(*For a card, find the number of matches*)
(*Add 1 to the corresponding following cards for that specific card*)
(*Since 1 needs to be added for all occurences, add array.(i) to array.(i+j)*)
let solve_card card_string int_array i =
  let score = no_cards_present (winning_and_had card_string) in
  for j = i + 1 to i + score do
    int_array.(j) <- int_array.(j) + int_array.(i)
  done

let sum_no_cards file =
  let the_lines = lines file in
  let array = create_array the_lines in
  let line_array = Array.of_list the_lines in
  for i = 0 to Array.length line_array - 1 do
    (*solve for one card at a time*)
    solve_card line_array.(i) array i
  done;
  Array.fold_left ( + ) 0 array

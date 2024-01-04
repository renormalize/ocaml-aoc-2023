let input_line_list file = In_channel.input_lines (In_channel.open_text file)

type ball = Red of int | Green of int | Blue of int | None

let game_int_and_ball_strings s =
  let game_and_sets = String.split_on_char ':' s in
  let game_int = int_of_string (Str.string_after (List.hd game_and_sets) 5) in
  let ball_strings =
    List.map String.trim
      (Str.split (Str.regexp "[;,]") (List.nth game_and_sets 1))
  in
  (game_int, ball_strings)

let string_to_ball s =
  let color_count_split = String.split_on_char ' ' s in
  let count, color =
    (List.nth color_count_split 0, List.nth color_count_split 1)
  in
  match color with
  | "red" -> Red (int_of_string count)
  | "green" -> Green (int_of_string count)
  | "blue" -> Blue (int_of_string count)
  | _ -> None

let validate_ball ball =
  match ball with
  | Red r -> if r <= 12 then Red r else None
  | Green g -> if g <= 13 then Green g else None
  | Blue b -> if b <= 14 then Blue b else None
  | None -> None

let sum_valid_games game_list =
  let validate_game s =
    let game_int, balls_string = game_int_and_ball_strings s in
    let ball_list = List.map string_to_ball balls_string in
    let valid_balls = List.map validate_ball ball_list in
    let valid_balls_bool =
      List.map (fun x -> match x with None -> false | _ -> true) valid_balls
    in
    if List.fold_left ( && ) true valid_balls_bool then Some game_int else None
  in
  let rec sum_valid_games_helper acc game_list_int_option =
    match game_list_int_option with
    | [] -> acc
    | h :: t -> (
        match h with
        | Some a -> sum_valid_games_helper (acc + a) t
        | None -> sum_valid_games_helper acc t)
  in
  sum_valid_games_helper 0 (List.map validate_game game_list)

let sum_prod_max list =
  let max_each_color s =
    let _, balls_string = game_int_and_ball_strings s in
    let ball_list = List.map string_to_ball balls_string in
    let rec max_balls red green blue valid_balls =
      match valid_balls with
      | [] -> (red, green, blue)
      | Red r :: t ->
          if red < r then max_balls r green blue t
          else max_balls red green blue t
      | Blue b :: t ->
          if blue < b then max_balls red green b t
          else max_balls red green blue t
      | Green g :: t ->
          if green < g then max_balls red g blue t
          else max_balls red green blue t
      | None :: t -> max_balls red green blue t
    in
    max_balls 0 0 0 ball_list
  in
  let prod_max s =
    let r, g, b = max_each_color s in
    r * g * b
  in
  List.fold_left ( + ) 0 (List.map prod_max list)

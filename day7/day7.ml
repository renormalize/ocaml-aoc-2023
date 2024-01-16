let lines file = In_channel.input_lines (In_channel.open_text file)

(*kind -> unique integer value where higher int implies stronger hand
  Five of a kind -> 7
  Four of a kind -> 6
  Full house -> 5
  Three of a kind -> 4
  Two pair -> 3
  One pair -> 2
  High card -> 1
  Using integers for inherent comparability which is used to sort according to strength of hands
*)
(*hand -> integer list where card maps to a unique integer value
  Each non digit card is assigned an integer above 9 based on its strength
  8 -> 8
  9 -> 9
  T -> 10
  J -> 11
  Q -> 12
  K -> 13
  A -> 14
*)

type play = { kind : int; hand : int list; bid : int }

let char_to_card_value = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | a -> Char.code a - Char.code '0'

let int_list_to_kind int_list =
  match List.sort compare int_list with
  | [ a; b; c; d; e ] ->
      if a = b && b = c && c = d && d = e then 7
      else if
        (a = b && b = c && c = d && d <> e)
        || (a <> b && b = c && c = d && d = e)
      then 6
      else if
        (a = b && b = c && c <> d && d = e)
        || (a = b && b <> c && c = d && d = e)
      then 5
      else if
        (a = b && b = c && a <> d && a <> e)
        || (a <> b && b = c && c = d && a <> e)
        || (a <> c && b <> c && c = d && d = e)
      then 4
      else if (a = b && c = d) || (b = c && d = e) || (a = b && d = e) then 3
      else if a = b || b = c || c = d || d = e then 2
      else 1
  | _ -> failwith "Invalid kind!"

let explode s = List.init (String.length s) (String.get s)
let string_to_int_list s = List.map char_to_card_value (explode s)

let line_to_play line =
  match String.split_on_char ' ' line with
  | [ a; b ] ->
      let h = string_to_int_list a in
      { kind = int_list_to_kind h; hand = h; bid = int_of_string b }
  | _ -> failwith "Invalid input!"

let compare_play c d =
  if c.kind > d.kind then 1
  else if c.kind < d.kind then -1
  else
    let rec cmp_list l l' =
      match (l, l') with
      | h :: t, h' :: t' ->
          if h < h' then -1 else if h > h' then 1 else cmp_list t t'
      | _ -> failwith "Invalid lists while comparing!"
    in
    cmp_list c.hand d.hand

let sorted_play_list file =
  lines file |> List.map line_to_play |> List.sort compare_play

let total_winnings file =
  let rec total acc index list =
    match list with
    | h :: t -> total (acc + (index * h.bid)) (index + 1) t
    | [] -> acc
  in
  total 0 1 (sorted_play_list file)

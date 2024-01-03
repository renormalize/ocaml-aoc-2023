let option_list =
  List.map Day2.is_valid_game (Day2.input_line_list "day2/input.txt")

let unpack a b = match b with Some x -> a + x | None -> a
let sum = List.fold_left unpack 0 option_list
let () = Printf.printf "\nSolving Day 2!\n"
let () = Printf.printf "The sum of the Game IDs is: %d\n\n" sum

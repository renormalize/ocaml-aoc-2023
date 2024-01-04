let () = Printf.printf "\nSolving Day 2!\n"
let sum = Day2.sum_valid_games (Day2.input_line_list "day2/input.txt")
let () = Printf.printf "The sum of the Game IDs is: %d\n" sum
let sum_prod = Day2.sum_prod_max (Day2.input_line_list "day2/input.txt")
let () = Printf.printf "The sum of products of max balls is: %d\n\n" sum_prod

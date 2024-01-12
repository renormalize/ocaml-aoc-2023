let () = Printf.printf "\nSolving Day 4!\n"

let () =
  Printf.printf "The sum of the points is: %d\n"
    (Day4.total_points "day4/input.txt")

let () =
  Printf.printf "The sum of the number of cards is: %d\n\n"
    (Day4.sum_no_cards "day4/input.txt")

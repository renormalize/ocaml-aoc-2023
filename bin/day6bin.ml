let () = Printf.printf "\nSolving Day 6!\n"

let () =
  Printf.printf
    "The product of the number of possibilities with spaces considered is: %d\n"
    (Day6.solve_string_with_spaces "day6/input.txt")

let () =
  Printf.printf
    "The product of the number of possibilities with spaces not considered is: \
     %d\n\n"
    (Day6.solve_string_without_spaces "day6/input.txt")

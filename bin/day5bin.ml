let () = Printf.printf "\nSolving Day 5!\n"

let () =
  Printf.printf "The lowest location number is: %d\n"
    (Day5.min_location_from_seeds "day5/input.txt")

let () =
  Printf.printf
    "The lowest location number with ranges as seed input is: %d\n\n"
    (Day5.solve_seed_ranges "day5/input.txt")

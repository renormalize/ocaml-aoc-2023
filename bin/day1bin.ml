let file = "day1/input.txt"
let () = Printf.printf "\nSolving Day 1!\n"

let () =
  Printf.printf "The sum of calibration values with only digits is: %d\n"
    (Day1.part1_calibration_sum file)

let () =
  Printf.printf "The sum of calibration values with digits and words is: %d\n\n"
    (Day1.part2_calibration_sum file)

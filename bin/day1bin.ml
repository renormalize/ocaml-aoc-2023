let () = Printf.printf "Solving Day 1!\n"

let line_list = Day1.lines "day1/input.txt"

let x = List.fold_left (+) 0 (List.map Day1.calib_value line_list)

let () = Printf.printf "Part 1: sum of calibration values is: %d\n" x

let line_list = Day1.lines "day1/input.txt"

let y = List.fold_left (+) 0 (List.map Day1.find_vals line_list)

let () = Printf.printf "Part 2: sum of calibration values is: %d\n" y

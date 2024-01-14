let lines file = In_channel.input_lines (In_channel.open_text file)

(*splits each section of input into its own list*)
let split_string_list lines =
  let rec helper acc list_list lines =
    match lines with
    | [] -> List.rev (List.rev acc :: list_list)
    | h :: t ->
        if String.equal h "" then helper [] (List.rev acc :: list_list) t
        else helper (h :: acc) list_list t
  in
  helper [] [] lines

(*source_dest_strings_to_triple_list takes the list of strings for each mapping and creates the map*)
(*The mapping is held in a triple list, where each triple is (start_source, end_source, dest_start)*)
let source_dest_strings_to_triple_list range_string_list =
  let parse_source_and_dest_string_to_triple range_string =
    let list_vals = String.split_on_char ' ' range_string in
    match list_vals with
    | [ dest_start; source_start; range_length ] ->
        let d_start, s_start, r_length =
          ( int_of_string dest_start,
            int_of_string source_start,
            int_of_string range_length )
        in
        (s_start, s_start + r_length - 1, d_start)
    | _ -> failwith "Invalid input 1"
  in
  List.map parse_source_and_dest_string_to_triple (List.tl range_string_list)

(*mapped_dest_value takes the input source and returns the destination based on the map*)
let mapped_dest_value source source_to_dest_map_list =
  match
    List.find_map
      (fun (source_start, source_end, dest_start) ->
        if source_start <= source && source <= source_end then
          Some (dest_start + source - source_start)
        else None)
      source_to_dest_map_list
  with
  | Some x -> x
  | None -> source

(*create the map from the raw string text for each mapping*)
let create_map_list list_list =
  List.map source_dest_strings_to_triple_list list_list

(*parse out the seed ints from the seed string*)
let parse_seeds string =
  List.filter_map
    (fun x -> if not (String.equal x "") then Some (int_of_string x) else None)
    (List.tl (String.split_on_char ' ' string))

(*find the minimum location from the given seed*)
let min_location_from_seeds file =
  let line_list = lines file in
  let list_list = split_string_list line_list in
  let seeds = parse_seeds (List.hd (List.hd list_list)) in
  let map_list = create_map_list (List.tl list_list) in
  let find_result seed = List.fold_left mapped_dest_value seed map_list in
  List.map find_result seeds |> List.sort compare |> List.hd

(*brute force method of computing the location for each seed in the range and finding min*)
let solve_seed_ranges file =
  let line_list = lines file in
  let list_list = split_string_list line_list in
  let seeds = Array.of_list (parse_seeds (List.hd (List.hd list_list))) in
  let map_list = create_map_list (List.tl list_list) in
  let find_result seed = List.fold_left mapped_dest_value seed map_list in
  let min_val = ref max_int in
  let arr_len = ref (Array.length seeds) in
  let i = ref 0 in
  while !i <= !arr_len - 1 do
    let j = ref seeds.(!i) in
    while !j <= seeds.(!i) + seeds.(!i + 1) - 1 do
      let result_seed = find_result !j in
      if !min_val > result_seed then min_val := result_seed
      else min_val := !min_val;
      j := !j + 1
    done;
    i := !i + 2
  done;
  !min_val

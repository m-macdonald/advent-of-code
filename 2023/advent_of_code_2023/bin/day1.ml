open Advent_of_code_2023

let is_digit c = 
    Char.code c >= Char.code '0' && Char.code c <= Char.code '9'


let rec get_num first last list =
    match list with
    | [] -> Utilities.create_string first last
    | char :: list ->
        match is_digit char with
        | false -> get_num first last list
        | true -> 
            match first with
            | None -> get_num (Some char) last list
            | Some _ ->
                match last with
                | None -> get_num first (Some char) list
                | Some _ -> get_num first (Some char) list

let rec sum_nums acc string_list =
    let first = None in 
    let last = None in
    match string_list with
    | [] -> acc
    | string :: tl  ->
        let chars = Utilities.explode_string string in
        sum_nums ((int_of_string (get_num first last chars)) + acc) tl

let part1 file_name = 
    let input = Utilities.read_file file_name in

    sum_nums 0 input

let part2 file_name =
    let input = Utilities.read_file file_name in

    sum_nums 0 input

let () = 
    Printf.printf "Part 1: %d\n" (part1 "inputs/part1.txt");
    (* Part one and two share the same input *)
    Printf.printf "Part 2: %d\n" (part2 "inputs/part1.txt");

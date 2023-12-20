open Advent_of_code_2023

type digit = { position: int; value: string; }

let part1 file_name =
    let is_digit c = 
        Char.code c >= Char.code '0' && Char.code c <= Char.code '9' in

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
                    | Some _ -> get_num first (Some char) list in

    let rec sum_nums acc string_list =
        let first = None in 
        let last = None in
        match string_list with
        | [] -> acc
        | string :: tl  ->
            let chars = Utilities.explode_string string in
            sum_nums ((int_of_string (get_num first last chars)) + acc) tl in

    let input = Utilities.read_file file_name in

    sum_nums 0 input

let part2 file_name =
    (* TODO: If I'm ever so inclined, I'd like to take the time to do this without regex *)
    let find_digit string test_str =
        let regex = Str.regexp_string test_str in
        try
            let start_pos = Str.search_forward regex string 0 in
            Some start_pos 
        with
        | Not_found -> None in

    let rec get_num first last string valid_digits =
        match valid_digits with 
        | [] -> print_endline ""
        | (test_str, value) :: tl ->
            match find_digit string test_str with
            | None -> 
        | Some found_position -> 
            let found_digit = { position = found_position; value = value } in
            match first, last with
            | None, None -> get_num (Some found_digit) last string tl
            | Some _, None -> get_num first (Some found_digit) string tl
            | Some _, Some last when found_position < last.position -> get_num first (Some found_digit) string tl in
        (* I don't much like this solution but it's the best I've got right now *)
        (* match valid_digits with *)
        (* | [] ->  *)
        (*     match first, last with  *)
        (*     | Some first, Some last -> Utilities.create_string first last *)
        (*     | Some first, None -> Utilities.create_string first first *)
        (*     | _ -> Utilities.create_string ' ' ' ' *)
        (* | (test_str, value) :: tl -> *)
        (*     match find_digit string test_str  with *)
        (*     | None -> get_num first last string tl *)
        (*     | Some found_position -> *)
        (*         let found_digit = { position = found_position; value = value } in *)
        (*         match first, last with *)
        (*         | None, None -> get_num (Some found_digit) last string tl *)
        (*         | Some _, None -> get_num first (Some found_digit) string tl *)
        (*         | Some _, Some last when found_position < last.position -> get_num first (Some found_digit) string tl in *)

    let rec sum_nums acc string_list =
        let first = None in 
        let last = None in
        let valid_digits = [("1", "1"); ("2", "2"); ("3", "3"); ("4", "4"); ("5", "5"); ("6", "6"); ("7", "7"); ("8", "8"); ("9", "9"); 
        ("one", "1"); ("two", "2"); ("three", "3"); ("four", "4"); ("five", "5"); ("six", "6"); ("seven", "7"); ("eight", "8"); ("nine", "9")] in
        match string_list with
        | [] -> acc
        | string :: tl  ->
            sum_nums ((int_of_string (get_num first last string valid_digits)) + acc) tl in

    let input = Utilities.read_file file_name in

    sum_nums 0 input
  
let () = 
    printf.printf "part 1: %d\n" (part1 "inputs/day1.txt");
    (* part one and two share the same input *)
    printf.printf "part 2: %d\n" (part2 "inputs/day1.txt");

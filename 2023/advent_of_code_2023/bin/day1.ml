open Advent_of_code_2023

type t = { position: int; value: char; }

let show digit= 
    match digit with
    | None -> Printf.sprintf "None"
    | Some digit -> Format.sprintf "{ position: %i, value: %c }" digit.position digit.value
(* let part1 file_name = *)
(*     let is_digit c =  *)
(*         Char.code c >= Char.code '0' && Char.code c <= Char.code '9' in *)
(*     let rec get_num first last list = *)
(*         match list with *)
(*         | [] -> Utilities.create_string first last *)
(*         | char :: list -> *)
(*             match is_digit char with *)
(*             | false -> get_num first last list *)
(*             | true -> *)
(*                 match first, last with *)
(*                 | None, None -> get_num (Some char) last list *)
(*                 | Some _, None -> get_num (Some char) last list *)
(*                 | Some _, Some _ -> get_num first (Some char) list in *)
(*     let rec sum_nums acc string_list = *)
(*         let first = None in  *)
(*         let last = None in *)
(*         let  *)
(*         match string_list with *)
(*         | [] -> acc *)
(*         | string :: tl  -> *)
(*             let chars = Utilities.explode_string string *)
(*             sum_nums ((int_of_string (get_num first last chars)) + acc) tl in *)
(*     let input = Utilities.read_file file_name in *)
(*     sum_nums 0 input *)

let part2 file_name =

    let create_string c1 c2 = 
        let buf = Buffer.create 16 in
        let concat c1 c2 = 
            match c1, c2 with
            | Some c1, Some c2 -> Buffer.add_char buf c1.value; Buffer.add_char buf c2.value
            | Some c1, None -> Buffer.add_char buf c1.value; Buffer.add_char buf c1.value
            | _ -> Buffer.add_string buf "Something went wrong" in

        concat c1 c2;
        Buffer.contents buf in

    (* TODO: If I'm ever so inclined, I'd like to take the time to do this without regex *)
    let find_digit string test_str =
        let regex = Str.regexp_string test_str in
        try
            let start_pos = Str.matched_group
            let start_pos = Str.search_forward regex string 0 in
            Some start_pos 
        with
        | Not_found -> Printf.printf "Character did not match %s\n" test_str ; None in

    (* I don't much like this solution but it's the best I've got right now *)
    let rec get_num first last string valid_digits =
        match valid_digits with 
        | [] -> Printf.printf "first: %s\n last: %s\nstring: %s\n\n" (show first) (show last) string; create_string first last
        | (test_str, value) :: tl ->
            match find_digit string test_str with
            | None -> get_num first last string tl
            | Some found_position -> 
                let found_digit = { position = found_position; value = value } in
                Printf.printf "%i %c\n" found_position value;
                match first, last with
                | None, None -> get_num (Some found_digit) last string tl
                | Some first, None when found_position < first.position -> get_num (Some found_digit) last string tl
                | Some first, None when found_position > first.position -> get_num (Some first) (Some found_digit) string tl
                | Some _, Some last when found_position > last.position -> get_num first (Some found_digit) string tl 
                | Some first, Some _ when found_position < first.position -> get_num (Some found_digit) last string tl
                | _ -> get_num first last string tl in

    let rec sum_nums acc string_list =
        let first = None in
        let last = None in
        let valid_digits = [("1", '1'); ("2", '2'); ("3", '3'); ("4", '4'); ("5", '5'); ("6", '6'); ("7", '7'); ("8", '8'); ("9", '9'); 
        ("one", '1'); ("two", '2'); ("three", '3'); ("four", '4'); ("five", '5'); ("six", '6'); ("seven", '7'); ("eight", '8'); ("nine", '9')] in
        match string_list with
        | [] -> acc
        | string :: tl  ->
            sum_nums ((int_of_string (get_num first last string valid_digits)) + acc) tl in

    let input = Utilities.read_file file_name in

    sum_nums 0 input
  
let () = 
    (* Printf.printf "Part 1: %d\n" (part1 "inputs/part1.txt"); *)
    (* Part one and two share the same input *)
    Printf.printf "Part 2: %d\n" (part2 "inputs/part1.txt");

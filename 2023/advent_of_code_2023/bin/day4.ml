open Advent_of_code_2023

let determine_points match_count =
    let rec pow2 number acc = 
        match number with
        | 0 -> acc
        | _ -> pow2 (number - 1) (2 * acc) in

    if match_count <= 0 then
        0
    else if match_count = 1 then
        1
    else
        pow2 (match_count - 1) 1

let parse_numbers number_string = 
    let split = String.split_on_char ' ' number_string in
    let split = List.filter (fun s -> String.trim s <> "") split in
    let trim_and_parse string = int_of_string (String.trim string) in
    List.map trim_and_parse split

let trim_prefix card_string = 
    let index_after_semicolon = (String.index_from card_string 0 ':') + 1 in
    String.sub card_string index_after_semicolon (String.length card_string - index_after_semicolon)

let process_card_string card_string = 
    let card_string = trim_prefix card_string in
    let split_string = String.split_on_char '|' card_string in
    let winning_numbers = parse_numbers (List.nth split_string 0) in
    let candidate_numbers = parse_numbers (List.nth split_string 1) in

    let rec loop candidate_numbers match_count =
        match candidate_numbers with
        | [] -> determine_points match_count
        | candidate_number :: tl -> 
            let is_candidate_a_winner = List.mem candidate_number winning_numbers in
            let match_count = 
                match is_candidate_a_winner with
                | true -> match_count + 1
                | false -> match_count in
            loop tl match_count in
    
    loop candidate_numbers 0 

let rec part1 card_strings acc =
    match card_strings with
    | [] -> acc
    | card_string :: tl ->
        let card_result = process_card_string card_string in
        let acc = acc + card_result in
        part1 tl acc

let () = 
    let file_contents = Utilities.read_file "inputs/day4.txt" in
    
    Printf.printf "Part1: %i" (part1 file_contents 0)

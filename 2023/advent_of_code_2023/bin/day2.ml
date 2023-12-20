open Advent_of_code_2023

type game = { number: int; red_count: int; green_count: int; blue_count: int }

let part1 file_name = 
    let file_contents = Utilities.read_file file_name in
    let rec parse_games games game_total =
        let parse_game game_string =
            (* This is the known index of the end of the "Game " substring that preceeds each line*)
            let index_of_game_substr = 5 in
            let index_of_semicolon = String.index_from game_string 0 ':' in
            let index_of_semicolon_with_padding = index_of_semicolon + 2 in
            let game_number = String.sub game_string index_of_game_substr (index_of_semicolon - index_of_game_substr) in
            let game_number = int_of_string game_number in
            let game_string_length = String.length game_string in
            let draws_substr = String.sub game_string (index_of_semicolon_with_padding) (game_string_length - index_of_semicolon_with_padding) in
            let game_draws = String.split_on_char ';' draws_substr in
            let parse_counts game_draw =
                let split_draws = String.split_on_char ',' game_draw in
                let rec loop split_draws is_game_valid =
                    let determine_color draw = 
                        let reversed_draw_string = Utilities.reverse_string draw in
                        let index_of_first_space = String.index_from reversed_draw_string 0 ' ' in
                        let color = Utilities.reverse_string (String.sub reversed_draw_string 0 index_of_first_space) in
                        color in
                    let draw_value draw =
                        let index_of_first_space = String.index_from draw 0 ' ' in
                        let value_substr = String.sub draw 0 (index_of_first_space) in
                        (* Printf.printf "complete draw string: %s\n" draw; *)
                        (* Printf.printf "value substring: %s\n" value_substr; *)
                        int_of_string value_substr in
                    (* let (red_count, blue_count, green_count) = acc in     *)
                    match split_draws with
                    | [] ->  is_game_valid
                    | hd :: tl ->
                        let draw = String.trim hd in
                        let value = draw_value draw in
                        let color = determine_color draw in
                        match color with
                        | "red" when value > 12 -> loop [] false 
                        | "blue" when value > 14 -> loop [] false
                        | "green" when value > 13-> loop [] false
                        | _ -> loop tl true in

                loop split_draws true in
            let rec loop_game_draws game_draws is_game_valid =
                match game_draws with
                | [] -> is_game_valid
                | hd :: tl ->
                    (* let game_counts = parse_counts hd in *)
                    (* let (red_count, blue_count, green_count) = game_counts in *)
                    (* let (red_total, blue_total, green_total) = acc in *)
                    (* let new_total = (red_total + red_count, blue_total + blue_count, green_total + green_count) in *)
                    (* let (new_red, new_blue, new_green) = new_total in *)
                    (* Printf.printf "red: %i, blue: %i, green: %i\n" new_red new_blue new_green; *)
                    match parse_counts hd with
                    | false -> loop_game_draws [] false
                    | true -> loop_game_draws tl true in

            match loop_game_draws game_draws true with
            | true -> Some game_number
            | _ -> None in

            (* let (red_total, blue_total, green_total) = loop_game_draws game_draws (0, 0, 0) in *)
            (* Printf.printf "Red: %i, Green: %i, Blue: %i\n%s" red_total green_total blue_total game_string; *)
            (* { number = game_number; red_count = red_total; green_count = green_total; blue_count = blue_total } in *)
            (* Printf.printf "%s\n" game_string in *)
            

        match games with
        | [] -> game_total
        | hd :: tl ->
            match parse_game hd with
            | Some game_number -> parse_games tl (game_total + game_number)
            | None -> parse_games tl game_total in
            (* let game = parse_game hd in *)
            (* if game.red_count > 12 || game.green_count > 13 || game.blue_count > 14 then *)
            (*     parse_games tl game_total *)
            (* else *)
            (*     parse_games tl (game_total + game.number) in *)
    
    parse_games file_contents 0

let () = 
    Printf.printf "part 1: %d\n" (part1 "inputs/day2-part1.txt");
    (* part one and two share the same input *)
    (* printf.printf "part 2: %d\n" (part2 "inputs/part1.txt"); *)

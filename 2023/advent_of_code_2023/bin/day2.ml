open Advent_of_code_2023

type game = { number: int; is_game_valid: bool; red_min: int; green_min: int; blue_min: int }
let red = "red"
let blue = "blue"
let green = "green"

let day2 file_name = 
    let file_contents = Utilities.read_file file_name in
    let rec parse_games games game_total game_power =
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
            let parse_counts game_draw game =
                let split_draws = String.split_on_char ',' game_draw in
                let rec loop split_draws game =
                    let determine_color draw = 
                        let reversed_draw_string = Utilities.reverse_string draw in
                        let index_of_first_space = String.index_from reversed_draw_string 0 ' ' in
                        let color = Utilities.reverse_string (String.sub reversed_draw_string 0 index_of_first_space) in
                        color in
                    let draw_value draw =
                        let index_of_first_space = String.index_from draw 0 ' ' in
                        let value_substr = String.sub draw 0 (index_of_first_space) in
                        int_of_string value_substr in
                    match split_draws with
                    | [] -> game
                    | hd :: tl ->
                        let draw = String.trim hd in
                        let value = draw_value draw in
                        let color = determine_color draw in
                        let is_game_valid =
                            if game.is_game_valid then
                                if  (color = red && value > 12) ||
                                    (color = blue && value > 14) || 
                                    (color = green && value > 13) then
                                    false
                                else
                                    true
                            else
                                false in   
                        let red_min = if value > game.red_min && color = red then value else game.red_min in
                        let blue_min = if value > game.blue_min && color = blue then value else game.blue_min in
                        let green_min = if value > game.green_min && color = green then value else game.green_min in
                        
                        loop tl { number = game_number; is_game_valid; red_min; blue_min; green_min } in

            loop split_draws game in
            let rec loop_game_draws game_draws game =
                match game_draws with
                | [] -> game
                | hd :: tl ->
                    let game = parse_counts hd game in
                    loop_game_draws tl game in

            loop_game_draws game_draws { number = 0; is_game_valid = true; red_min = 0; green_min = 0; blue_min = 0 } in

        match games with
        | [] -> (game_total, game_power)
        | hd :: tl ->
            let game = parse_game hd in
            let game_total = 
                match game.is_game_valid with
                | true -> game_total + game.number
                | false -> game_total in
            let power = game.red_min * game.blue_min * game.green_min in
            let game_power = game_power + power in
            parse_games tl game_total game_power in

    parse_games file_contents 0 0

let () = 
    let (game_total, power_total) = day2 "inputs/day2.txt" in
    Printf.printf "part 1: %d\n" game_total;
    (* part one and two share the same input *)
    Printf.printf "part 2: %d\n" power_total
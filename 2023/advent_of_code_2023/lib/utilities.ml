let read_file file_name : string list =
    let ic = open_in file_name in
    
    (* Defines a function that uses the in_channel created above and accesses the next line within. If there is no line ("with" clause) it returns a None instead *)
    let try_read() = 
        try Some (input_line ic) with End_of_file -> None in
    (* Recursive function that will loop until the try_read function returns None *)
    let rec loop acc = match try_read () with
        (* :: notation appends to the front of the list *)
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []

let explode_string s = List.init (String.length s) (String.get s)

let create_string c1 c2 = 
    let buf = Buffer.create 16 in
    match c1 with
    | None -> Buffer.add_string buf String.empty; Buffer.contents buf
    | Some c1 -> 
        match c2 with
        | None -> Buffer.add_char buf c1; Buffer.add_char buf c1; Buffer.contents buf
        | Some c2 -> 
            Buffer.add_char buf c1;
            Buffer.add_char buf c2;
            Buffer.contents buf

type token = 
    Identifier of string 
    | String of string
    | EOL 
    | ParenL 
    | ParenR 
    | CurlyL 
    | CurlyR 
    | Semicolon
    | Whitespace

let string_of_token token = match token with
    | Identifier id -> Printf.sprintf "Identifier[%s]" id
    | String str -> Printf.sprintf "String[\"%s\"]" str
    | EOL -> "EOL" 
    | ParenL -> "ParenL" 
    | ParenR -> "ParenR" 
    | CurlyL -> "CurlyL" 
    | CurlyR -> "CurlyR" 
    | Semicolon -> "Semicolon"
    | Whitespace -> "Whitespace"

let rec until cond tokens = match tokens with
| [] -> []
| hd :: tl when not (cond hd) -> until cond tl
| tokens -> tokens

let tokenize_file filename =
    let stringify chars = let chars = List.rev chars in String.init (List.length chars) (List.nth chars) in
    let rec tokenize_identifier chars tokens = match tokens with
    | [] -> [Identifier (stringify chars); EOL]
    | ('a'..'z' | 'A'..'Z' | '_' | '0'..'1') as hd :: tl -> tokenize_identifier (hd :: chars) tl
    | list -> Identifier (stringify chars) :: tokenize_line list
    and tokenize_string chars tokens = match tokens with
    (* TODO: handle string escaping *)
    | '"' :: tl -> String (stringify chars) :: tokenize_line tl
    | hd :: tl -> tokenize_string (hd :: chars) tl
    | [] -> failwith "Found EOL or EOF before end of string"
    and tokenize_line line = match line with
    | [] -> [EOL]
    | ('a'..'z' | 'A'..'Z' | '_') as hd :: tl -> tokenize_identifier [hd] tl
    | '"' :: tl -> tokenize_string [] tl
    | ' ' :: tl -> Whitespace :: tokenize_line tl
    | ';' :: tl -> Semicolon :: tokenize_line tl
    | '(' :: tl -> ParenL :: tokenize_line tl
    | ')' :: tl -> ParenR :: tokenize_line tl
    | '{' :: tl -> CurlyL :: tokenize_line tl
    | '}' :: tl -> CurlyR :: tokenize_line tl
    | _ -> failwith "Unknown char" in
    let tokenize_from_channel channel = 
        let line = input_line channel in
        tokenize_line (List.init (String.length line) (String.get line)) in
    let rec read_file channel = try 
        let line = tokenize_from_channel channel in
        line @ (read_file channel) with
    | End_of_file -> close_in channel; [] in
    read_file (open_in filename)

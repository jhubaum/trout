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

type location = {
    line : int;
    column : int;
}

type located_token = {
    token : token;
    location : location;
}

let string_of_token token = match token with
    | Identifier id -> Printf.sprintf "Identifier[%s]" id
    | String str -> Printf.sprintf "String[\"%s\"]" (String.escaped str)
    | EOL -> "EOL" 
    | ParenL -> "ParenL" 
    | ParenR -> "ParenR" 
    | CurlyL -> "CurlyL" 
    | CurlyR -> "CurlyR" 
    | Semicolon -> "Semicolon"
    | Whitespace -> "Whitespace"

let string_of_located_token { token = token; location = loc } = 
    Printf.sprintf "%d,%d: %s" loc.line loc.column (string_of_token token) 


let rec until cond tokens = match tokens with
| [] -> []
| hd :: tl when not (cond hd) -> until cond tl
| tokens -> tokens


type token_error = 
    | UnknownToken of string * location
    | UnknownEscapedChar of string * location
    | UnfinishedString of location


let describe_token_error error = match error with
    | UnknownToken (s, loc) -> Printf.printf "%d,%d: Unknown token '%s'\n" loc.line loc.column s
    | UnknownEscapedChar (s, loc) -> Printf.printf "%d,%d: Unknown escaped character '%s'\n" loc.line loc.column s
    | UnfinishedString loc -> Printf.printf "%d,%d: Unfinished string\n" loc.line loc.column

exception InternalException of token_error

type state = ParseIdentifier of (location * char list) 
    | ParseString of { loc: location; chars: char list; escape_next: bool} 
    | ParseToken


let tokenize_line line_number line = 
    let state = ref ParseToken in 
    let to_string chars = String.init (List.length chars) (List.nth @@ List.rev chars) in
    let finish_identifier loc name = state := ParseToken; { location=loc; token=(Identifier name)} in
    let is_from_identifier c = match c with
        | ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') -> true
        | _ -> false in
    let peek_char c = match !state with
        | ParseIdentifier (loc, chars) -> if not (is_from_identifier c)
            then Some (finish_identifier loc (to_string chars)) 
            else None
        | _ -> None in
    let consume_char i c = 
        let i = i + 1 in
        let loc = { line = line_number; column = i } in
        let tok t = Some { token = t; location = loc } in
        match !state with
        | ParseToken -> begin match c with
            | ('a'..'z' | 'A'..'Z' | '_') -> state := ParseIdentifier (loc, [c]); None
            | '"' -> state := ParseString { loc=loc; chars=[]; escape_next=false }; None
            | ' ' -> tok Whitespace
            | ';' -> tok Semicolon
            | '(' -> tok ParenL
            | ')' -> tok ParenR
            | '{' -> tok CurlyL
            | '}' -> tok CurlyR
            | _ -> raise (InternalException (UnknownToken (String.make 1 c, loc)))
            end
        | ParseIdentifier (loc, chars) -> begin match c with
            | c when is_from_identifier c -> state := ParseIdentifier (loc, c :: chars); None
            | _ -> assert false (* handled by peek_char *)
            end
        | ParseString { loc = l; chars = chars; escape_next = true } -> 
            let insert_escaped c = state := ParseString { loc = l; chars = c :: chars; escape_next = false }; None in
            begin match c with
            | 'n' -> insert_escaped '\n'
            | c -> raise (InternalException (UnknownEscapedChar (String.make 1 c, loc)))
            end
        | ParseString { loc = l; chars = chars; escape_next = false } -> begin match c with
            | '"' -> state := ParseToken; Some {location=l; token=String (to_string chars)}
            | '\\' -> state := ParseString { loc = l; chars = chars; escape_next = true }; None
            | c -> state := ParseString { loc = l; chars = c :: chars; escape_next = false }; None
            end in
    let match_char i c = 
        let peeked = peek_char c in
        let consumed = consume_char i c in
        peeked, consumed in
    let tokens = List.fold_left begin fun acc elems -> match elems with
        | Some a, Some b -> b :: a :: acc
        | Some a, None -> a :: acc
        | None, Some b -> b :: acc
        | None, None -> acc
    end [] (List.mapi match_char line) in
    let eol = { location = { line = line_number; column = List.length line }; token = EOL } in
    List.rev begin match !state with 
        | ParseToken -> eol :: tokens
        | ParseIdentifier (loc, chars) -> eol :: (finish_identifier loc (to_string chars)) :: tokens
        | ParseString { loc = l; _} -> raise (InternalException (UnfinishedString l))
    end

let tokenize_file filename =
    let tokenize_from_channel line_num channel = 
        let line = input_line channel in
        tokenize_line line_num (List.init (String.length line) (String.get line)) in
    let rec read_file line_num channel = try 
        let line = tokenize_from_channel line_num channel in
        line @ (read_file (line_num+1) channel) with
    | End_of_file -> close_in channel; [] in
    try 
        Ok (read_file 1 (open_in filename))
    with
    | InternalException e -> Error e

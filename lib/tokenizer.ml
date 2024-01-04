type token = 
    Identifier of string 
    | String of string
    | Integer of int
    | EOL 
    | ParenL 
    | ParenR 
    | CurlyL 
    | CurlyR 
    | Semicolon
    | Whitespace
    | Comma
    | Dot
    | Colon

let string_of_token token = match token with
    | Identifier id -> Printf.sprintf "Identifier[%s]" id
    | String str -> Printf.sprintf "String[\"%s\"]" (String.escaped str)
    | Integer n -> Printf.sprintf "Integer[%d]" n
    | EOL -> "EOL" 
    | ParenL -> "ParenL" 
    | ParenR -> "ParenR" 
    | CurlyL -> "CurlyL" 
    | CurlyR -> "CurlyR" 
    | Semicolon -> "Semicolon"
    | Whitespace -> "Whitespace"
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Colon -> "Colon"

type located_token = {
    token : token;
    location : Common.location;
}

let string_of_located_token { token = token; location = loc } = 
    Printf.sprintf "%d,%d: %s" loc.line loc.column (string_of_token token) 

type token_error = 
    | UnknownChar of string * Common.location
    | UnknownEscapedChar of string * Common.location
    | UnfinishedString of Common.location

let describe_token_error error = match error with
    | UnknownChar (s, loc) -> Printf.printf "%d,%d: Unknown char '%s'\n" loc.line loc.column s
    | UnknownEscapedChar (s, loc) -> Printf.printf "%d,%d: Unknown escaped character '%s'\n" loc.line loc.column s
    | UnfinishedString loc -> Printf.printf "%d,%d: Unfinished string\n" loc.line loc.column

(* A stateful iterator over a file to make tokenization easier *)
(* TODO: Does it have to be stateful? With the next_token function, it's much easier to make it stateless *)
module CharIterator = struct
    (* The data for the iterator. If the channel is None, the file has been fully read *)
    type t = { channel: in_channel option; line_index: int; column_index: int; rem_line: char list }

    (* Read the next line from the channel. Only used internally *)
    let _next_line it = match !it.channel with
    | None -> ()
    | Some c -> 
        try
            let line = (input_line c) ^ "\n" in
            it := { channel = Some c; line_index = !it.line_index+1; column_index=1; rem_line = List.init (String.length line) (String.get line) }
        with
        | End_of_file -> close_in c; it := { !it with channel = None }

    (* TODO: Handle non-existent files by catching 'Sys_error' exception *)
    let create filename = let it = ref { channel = Some (open_in filename); line_index = 0; column_index = 0; rem_line = [] } in
        _next_line it; it

    let peek it = match it.rem_line with
    | [] -> None
    | c :: _ -> Some c

    let pop it = match !it.channel with
    | None -> None
    | Some _ -> begin match !it.rem_line with
        | c :: [] -> _next_line it; Some c
        | hd :: tl -> it := { !it with column_index = 1 + !it.column_index; rem_line = tl }; Some hd
        | [] -> assert false (* Should only be the case when channel is None*)
        end

    let location it : Common.location = { line = it.line_index; column = it.column_index }
end

let next_token iterator = 
    let to_string chars = String.init (List.length chars) (List.nth @@ List.rev chars) in
    let to_digit c = (int_of_char c) - (int_of_char '0') in
    let rec get_identifier chars = match CharIterator.peek !iterator with
    | None -> Identifier (to_string chars)
    | Some ('a'..'z' | 'A'..'Z' | '_' | '0'..'9' as c) -> let _ = CharIterator.pop iterator in get_identifier (c :: chars)
    | _ -> Identifier (to_string chars) in
    let rec get_number n = match CharIterator.peek !iterator with
    | None -> n
    | Some ('0'..'9' as c) -> let _ = CharIterator.pop iterator in get_number 10 * n + (to_digit c)
    | Some _ -> n in
    let rec get_string escape_next chars = 
        let loc = CharIterator.location !iterator in
        let escaped c = match c with
        | 'n' -> Ok '\n'
        | c -> Error (UnknownEscapedChar (String.make 1 c, loc)) in
        let char = CharIterator.pop iterator in
        match char with
        | None -> Error (UnfinishedString loc)
        | Some c -> if escape_next then match escaped c with
            | Ok c -> get_string false (c :: chars)
            | Error err -> Error err
            else match c with
            | '"' -> Ok (String (to_string chars))
            | '\\' -> get_string true chars
            | '\n' -> Error (UnfinishedString loc)
            | c -> get_string false (c :: chars) in
    let location = CharIterator.location !iterator in
    let token = match CharIterator.pop iterator with
    | None -> None
    | Some ('a'..'z' | 'A'..'Z' | '_' as c) -> Some (Ok (get_identifier [c]))
    | Some ('0'..'9' as c) -> Some (Ok (Integer (get_number (to_digit c))))
    | Some '"' -> Some (get_string false [])
    | Some ' ' -> Some (Ok Whitespace)
    | Some '\n' -> Some (Ok EOL)
    | Some ';' -> Some (Ok Semicolon)
    | Some ':' -> Some (Ok Colon)
    | Some '(' -> Some (Ok ParenL)
    | Some ')' -> Some (Ok ParenR)
    | Some '{' -> Some (Ok CurlyL)
    | Some '}' -> Some (Ok CurlyR)
    | Some ',' -> Some (Ok Comma)
    | Some '.' -> Some (Ok Dot)
    | Some c -> Some (Error (UnknownChar (String.make 1 c, location))) in
    match token with
    | None -> None
    | Some (Ok tok) -> Some (Ok { location = location; token = tok })
    | Some (Error err) -> Some (Error err)
    
let tokenize_file filename =
    (* TODO: Replace aux function by returning a token iterator? *)
    let rec aux iterator = let next = next_token iterator in match next with
    | None -> Ok []
    | Some (Error err) -> Error err
    | Some (Ok token) -> begin match aux iterator with
        | Error err -> Error err
        | Ok tokens -> Ok (token :: tokens)
        end in
    let iterator = CharIterator.create filename in 
    aux iterator

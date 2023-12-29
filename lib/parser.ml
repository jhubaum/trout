open Common;;
open Tokenizer;;

type value = 
    Literal of string | 
    Variable of string |
    Meta of string |
    Call of function_call | 
    (* TODO: Implement parsing for this *)
    StructInit of struct_init |
    MemberAccess of value * string
and function_call = { name: string; args: value list; location: location }
and struct_init = { struct_name: string; members: (string * value) list; }

type statement = function_call

type function_def = { name: string; scope: statement list; location: location; params: string list}

type mod_statement = function_def
type _mod = mod_statement list

let skip_whitespace = until (fun t -> t.token != Whitespace && t.token != EOL)

type parser_error = UnexpectedToken of located_token |
    TokenizerError of token_error |
    UnexpectedEndOfFile |
    InvalidMetaCall of location |
    InvalidFunctionCall of location

exception InternalParserError of parser_error

let describe_error error = match error with
    | TokenizerError e -> describe_token_error e
    | UnexpectedToken { token = token; location = location } -> 
        Printf.printf "%s: Unexpected token %s\n" (string_of_location location) (string_of_token token)
    | UnexpectedEndOfFile -> Printf.printf "Unexpected end of file\n"
    | InvalidMetaCall l -> Printf.printf "%s: Invalid call to meta function" (string_of_location l)
    | InvalidFunctionCall l -> Printf.printf "%s: Called object is not callable" (string_of_location l)

let match_or_fail (expected : token) (tokens : located_token list) = match skip_whitespace tokens with
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile
    | hd :: tl -> if hd.token = expected 
        then tl 
        else raise @@ InternalParserError (UnexpectedToken hd)

let match_chain tokens chain = 
    List.fold_left (fun tokens expected -> match_or_fail expected tokens) tokens chain

(* TODO: Refactor to make tokens hold a state, so I don't need to return all tokens
   in all calls and can just use (Tokens.peek tokens) and (Tokens.pop tokens) *)
let parse_list tokens parse_item =
    let tokens = match_or_fail ParenL tokens in
    let rec aux tokens = match skip_whitespace tokens with
        | {token=Comma;_} :: tl -> let item, tokens = parse_item (skip_whitespace tl) in
            let items, tokens = aux tokens in
            item :: items, tokens
        | {token=ParenR;_} :: tl -> [], tl
        | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
        | [] -> raise @@ InternalParserError UnexpectedEndOfFile in
    let parse tokens = match skip_whitespace tokens with
        | {token=ParenR;_} :: tl -> [], tl
        | tl -> let item, tokens = parse_item (skip_whitespace tl) in
            let items, tokens = aux tokens in
            item :: items, tokens in
    parse tokens

let parse_identifier tokens = match skip_whitespace tokens with
    | { token=Identifier name;_} :: tl -> name, tl
    | token :: _ -> raise @@ InternalParserError (UnexpectedToken token)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile

let rec parse_value tokens = 
    let rec follow_identifier location value tokens = match skip_whitespace tokens with
    | {token=Dot;_} :: {token=Identifier name;_} :: tl -> follow_identifier location (MemberAccess (value, name)) tl
    | {token=ParenL;_} as hd :: tl -> 
        begin match value with
        | Variable name -> 
            let args, tokens = parse_list (hd :: tl) parse_value in
            follow_identifier location (Call { name = name; args = args; location = location}) tokens
        | _ -> raise @@ InternalParserError (InvalidFunctionCall location)
        end
    | _ -> value, tokens in
    match skip_whitespace tokens with
    | {token=String name; _} :: tl -> Literal name, tl
    | {token=Identifier "meta"; location} :: tl ->
        let args, tokens = parse_list tl parse_identifier in
        if List.length args <> 1 then
            raise @@ InternalParserError (InvalidMetaCall location)
        else
            follow_identifier location (Meta (List.hd args)) tokens
    | {token=Identifier name; location} :: tl -> follow_identifier location (Variable name) tl
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile

let parse_scope tokens = 
    let tokens = match_or_fail CurlyL tokens in
        let parse_function_call tokens = match skip_whitespace tokens with
        | {token=Identifier name;location=location} :: tl -> let args, tokens = parse_list tl parse_value in
            { name = name; args = args; location = location }, tokens
        | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
        | [] -> raise @@ InternalParserError UnexpectedEndOfFile in
    let rec aux scope tokens = match skip_whitespace tokens with
        | {token=CurlyR;_} :: tl -> List.rev scope, tl
        | {token=Semicolon;_} :: tl -> aux scope tl
        | tokens -> let statement, tokens = parse_function_call tokens in
            aux (statement :: scope) tokens in
    aux [] tokens

let parse_function tokens = match skip_whitespace tokens with
    | {token=Identifier name;location=location} :: tl -> 
            let params, tokens = parse_list tl (fun tokens -> match skip_whitespace tokens with
                | {token=Identifier name;_} :: tl -> name, tl
                | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
                | [] -> raise @@ InternalParserError UnexpectedEndOfFile) in
            let scope, tokens = parse_scope tokens in
        { name = name; scope = scope; location=location; params = params}, tokens
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile


let parse_module tokens = 
    let rec aux tokens = match skip_whitespace tokens with
    | [] -> []
    | {token=Identifier "fn";_} :: tl -> let func, tokens =  parse_function tl in
        func :: aux tokens
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd) in
    try Ok (aux tokens) with
    | InternalParserError e -> Error e


let parse_file filename = match tokenize_file filename with
    | Ok tokens -> parse_module tokens
    | Error e -> Error (TokenizerError e)

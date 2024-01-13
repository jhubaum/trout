open Common;;
open Tokenizer;;

type literal = String of string | Integer of int | Bool of bool

type value = 
    Literal of literal |
    Variable of string |
    Meta of string |
    Call of function_call | 
    (* TODO: Implement parsing for this *)
    StructInit of struct_init |
    MemberAccess of value * string |
    MatchStatement of match_statement
and function_call = { name: string; args: value list; location: location }
and struct_init = { struct_name: string; members: (string * value) list; }
and match_statement = { match_location: location; on_value: value; rows: match_statement_row list }
and match_statement_row = { (*row_location: location; *) matched_type: string; identifier: string; value: value }

type statement = FunctionCall of function_call | Match of match_statement

type type_constraint = TypeId of string | SumType of (type_constraint list)
type param_def = { param_name: string; type_constraint: type_constraint option }
type function_def = { name: string; scope: statement list; location: location; params: param_def list}

type mod_statement = function_def
type _mod = mod_statement list

let rec until cond tokens = match tokens with
| [] -> []
| hd :: tl when not (cond hd) -> until cond tl
| tokens -> tokens

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
    | {token=String name; _} :: tl -> Literal (String name), tl
    | {token=Integer i; _} :: tl -> Literal (Integer i), tl
    | {token=Identifier "false"; _} :: tl -> Literal (Bool false), tl
    | {token=Identifier "true"; _} :: tl -> Literal (Bool true), tl
    | {token=Identifier "meta"; location} :: tl ->
        let args, tokens = parse_list tl parse_identifier in
        if List.length args <> 1 then
            raise @@ InternalParserError (InvalidMetaCall location)
        else
            follow_identifier location (Meta (List.hd args)) tokens
    | {token=Identifier name; location} :: tl -> follow_identifier location (Variable name) tl
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile

let parse_type_constraint tokens = 
    let parse_type_id tokens = match skip_whitespace tokens with
    | {token=Identifier name;_} :: tl -> (TypeId name), tl
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile in
    let rec aux type_constraints tokens = 
        let type_constraint, tokens = parse_type_id tokens in
        match skip_whitespace tokens with
        | {token=Bar;_} :: tl -> aux (type_constraint :: type_constraints) tl
        | tokens -> if (List.length type_constraints) = 0 then type_constraint, tokens else SumType (type_constraint :: type_constraints), tokens in
    aux [] tokens

let parse_param tokens = 
    let name, tokens = parse_identifier tokens in
    match skip_whitespace tokens with
    | {token=Colon;_} :: tl -> 
        let type_constraint, tokens = parse_type_constraint tl in
        { param_name=name; type_constraint = Some type_constraint}, tokens
    | _ -> { param_name=name; type_constraint = None }, tokens



let parse_match_statement location tokens = 
    let parse_match_row tokens = 
        let type_id, tokens = parse_identifier tokens in
        let name, tokens = parse_identifier tokens in
        match skip_whitespace tokens with
        | {token=ArrowRight;_} :: tl -> 
                let value, tokens = parse_value tl in
                { matched_type = type_id; identifier=name; value=value }, tokens
        | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
        | [] -> raise @@ InternalParserError UnexpectedEndOfFile in
    let parse_scope tokens = 
        let rec aux tokens = match skip_whitespace tokens with
        | {token=Comma;_} :: tl -> aux tl
        | {token=CurlyR;_} :: tl -> [], tl
        | tokens -> 
                let row, tokens = parse_match_row tokens in 
                let rows, tokens = aux tokens in
                row :: rows, tokens in
        match skip_whitespace tokens with
        | {token=CurlyL;_} :: tl -> aux tl
        | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
        | [] -> raise @@ InternalParserError UnexpectedEndOfFile in
    let value, tokens = parse_value tokens in
    let rows, tokens = parse_scope tokens in
    Match { match_location=location; on_value=value; rows=rows }, tokens

let parse_scope tokens = 
    let tokens = match_or_fail CurlyL tokens in
        let parse_function_call tokens = match skip_whitespace tokens with
        | {token=Identifier "match";location=location} :: tl -> parse_match_statement location tl
        | {token=Identifier name;location=location} :: tl -> let args, tokens = parse_list tl parse_value in
            FunctionCall { name = name; args = args; location = location }, tokens
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
            let params, tokens = parse_list tl parse_param in
            let scope, tokens = parse_scope tokens in
        { name = name; scope = scope; location=location; params = params}, tokens
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd)
    | [] -> raise @@ InternalParserError UnexpectedEndOfFile

let skip_comments tokens = 
    let rec skip_singleline_comment tokens = match tokens with
    | [] -> []
    | {token=EOL;_} :: tl -> tl
    | _ :: tl -> skip_singleline_comment tl in
    let rec aux depth tokens = match tokens with
        | [] -> Ok []
        | {token=StartMultilineComment;_} :: tl -> aux (depth+1) tl
        | {token=StartSingleLineComment;_} :: tl -> aux depth (skip_singleline_comment tl)
        | {token=EndMultilineComment;_} as hd :: tl -> 
                if depth = 0 
                then Error (UnexpectedToken hd) 
                else aux (depth-1) tl
        | hd :: tl -> begin match aux depth tl with
            | Error err -> Error err
            | Ok tokens -> if depth = 0 then Ok (hd :: tokens) else Ok (tokens)
            end in
    aux 0 tokens

let parse_module tokens = 
    let rec aux tokens = match skip_whitespace tokens with
    | [] -> []
    | {token=Identifier "fn";_} :: tl -> let func, tokens =  parse_function tl in
        func :: aux tokens
    | hd :: _ -> raise @@ InternalParserError (UnexpectedToken hd) in
    match skip_comments tokens with
    | Error err -> Error err
    | Ok tokens -> begin try Ok (aux tokens) with
        | InternalParserError e -> Error e
        end

let parse_file filename = match tokenize_file filename with
    | Ok tokens -> parse_module tokens
    | Error e -> Error (TokenizerError e)

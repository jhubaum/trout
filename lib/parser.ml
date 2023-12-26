open Tokenizer;;

type value = string
type function_call = { name: string; args: value list }

type statement = function_call

type function_def = { name: string; scope: statement list }

(* TODO: Rename *)
type mod_statement = function_def
type _mod = mod_statement list

let skip_whitespace = until (fun t -> t != Whitespace && t != EOL)

let match_or_fail expected tokens = match skip_whitespace tokens with
    | [] -> failwith "Unexpected end of file"
    | hd :: tl -> if hd = expected 
        then tl 
        else failwith (Printf.sprintf "Unexpected token %s" (string_of_token hd))

let match_chain tokens chain = 
    List.fold_left (fun tokens expected -> match_or_fail expected tokens) tokens chain

(* TODO: Switch to using result type and proper error. Add locations to tokens and make one error unexpected token *)
let parse_scope tokens = 
    let tokens = match_or_fail CurlyL tokens in
    let parse_arg_list tokens = 
        let tokens = match_or_fail ParenL tokens in
        let rec aux tokens = match skip_whitespace tokens with
        | ParenR :: tl -> [], tl
        | String name :: tl -> let args, tokens = aux tl in
            name :: args, tokens
        | hd :: _ -> failwith (Printf.sprintf "Unexpected token %s in arg list" (string_of_token hd))
        | [] -> failwith "Unexpected end of file in arg list"
        in aux tokens in
    let parse_function_call tokens = match skip_whitespace tokens with
        | Identifier name :: tl -> let args, tokens = parse_arg_list tl in
            { name = name; args = args }, tokens
        | hd :: _ -> failwith (Printf.sprintf "Unexpected token %s in scope" (string_of_token hd))
        | [] -> failwith "Unexpected end of file in scope" in
    let rec aux scope tokens = match skip_whitespace tokens with
        | CurlyR :: tl -> List.rev scope, tl
        | Semicolon :: tl -> aux scope tl
        | tokens -> let statement, tokens = parse_function_call tokens in
            aux (statement :: scope) tokens in
    aux [] tokens

let parse_function tokens = match skip_whitespace tokens with 
    | Identifier name :: tl -> let scope, tokens = parse_scope (match_chain tl [ParenL; ParenR]) in
        { name = name; scope = scope }, tokens
    | _ -> failwith "Unexpected token after fn"


let parse_module tokens = 
    let rec aux tokens = match skip_whitespace tokens with
    | [] -> []
    | Identifier "fn" :: tl -> let func, tokens =  parse_function tl in
        func :: aux tokens
    | hd :: _ -> failwith (Printf.sprintf "Unexpected token %s in module" (string_of_token hd)) in
    aux tokens


(* quick hack to ignore location. TODO: fix properly *)
let parse_file filename = parse_module (List.map (fun { token=token; _} -> token) (tokenize_file filename))

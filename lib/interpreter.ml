type error = UnknownFunction of Parser.function_call |
             ArgumentCountMismatch of (Parser.function_call * Parser.function_def) |
             ArgumentTypeMismatch of (int * Parser.function_call * Parser.function_def) |
             UnknownIdentifier of (string * Common.location)

let describe_error _ = 
    (* TODO: Implement *)
    print_endline "An error occurred"

type type_name = string

(* a function call that calls an existing function with valid arguments *)
type checked_function_call = {
    function_index: int; (* refers to the function_def in the Parser._mod *)
    args: (Parser.value * type_name) list;
}

type types_in_scope = (string (* var name *) * string (* type name *)) list

let type_for_identifier (scope : types_in_scope) identifier location = 
    match List.find_opt (fun (var_name, _) -> var_name = identifier) scope with
    | None -> Error (UnknownIdentifier (identifier, location))
    | Some (_, type_name) -> Ok (type_name)

let create_scope (call : checked_function_call) (func : Parser.function_def) = 
    List.combine (List.map (fun (p : Parser.param_def) -> p.param_name) func.params) (List.map snd call.args)

let rec map_result f l = match l with
| [] -> Ok []
| hd :: tl -> begin match f hd with
    | Error err -> Error err
    | Ok v -> begin match map_result f tl with
        | Error err -> Error err
        | Ok l -> Ok (v :: l)
        end
    end

let rec fold_result f acc l = match l with
| [] -> Ok acc
| hd :: tl -> begin match f acc hd with
    | Error err -> Error err
    | Ok acc -> fold_result f acc tl
    end

(* TODO: Return a list of errors? *)
(* TODO: This is an unreadable mess. Can I use monads to make this more readable? *)
let verify_function_calls (_mod : Parser._mod) : (checked_function_call list, error) result= 
    let find_function_index name = 
        let aux i (def : Parser.function_def) = if def.name = name then Some (i, def) else None in
        List.find_mapi aux _mod in
    let get_type_list (cur_scope : types_in_scope) location (args : Parser.value list) =
        let aux (value : Parser.value) = match value with
        | Parser.StringLiteral _ -> Ok "string"
        | Parser.IntegerLiteral _ -> Ok "int"
        | Parser.Variable name -> type_for_identifier cur_scope name location
        | _ -> failwith "Unsupported case in `find_type_for_value`" in
        map_result aux args in
    let check_signature_match (cur_scope : types_in_scope) (call : Parser.function_call) (func : Parser.function_def) = 
        let type_is_valid (arg : type_name) (type_constraint : Parser.type_constraint option) = match arg, type_constraint with
        | _, None -> true
        | "string", Some Parser.String -> true
        | "int", Some Parser.Integer -> true
        | _, _ -> false in
        let rec aux i (args : type_name list) (params : Parser.param_def list) = 
            match args, params with
            | [], [] -> Ok ()
            | [], _ :: _ -> Error (ArgumentCountMismatch (call, func))
            | _ :: _, [] -> Error (ArgumentCountMismatch (call, func))
            | arg :: args, param :: params -> if type_is_valid arg param.type_constraint then
                    aux (i+1) args params
                else
                    Error (ArgumentTypeMismatch (i, call, func)) in
        match get_type_list cur_scope call.location call.args with
        | Error err -> Error err
        | Ok args -> Result.map (fun () -> args) (aux 1 args func.params) in
    let rec verify_and_collect_calls 
        (cur_scope : types_in_scope) 
        (function_calls : checked_function_call list) 
        (call : Parser.function_call)
            : (checked_function_call list, error) result = 
        let recurse_function_call (call : checked_function_call) (func : Parser.function_def)
            : (checked_function_call list, error) result = match List.find_opt ((=) call) function_calls with
            | Some _ -> Ok function_calls
            | None -> 
                let scope = create_scope call func in
                fold_result (verify_and_collect_calls scope) (call :: function_calls) func.scope in
        match find_function_index call.name with
        | None -> Error (UnknownFunction call)
        | Some (index, def) -> begin match check_signature_match cur_scope call def with
            | Error err -> Error err
            | Ok type_list -> 
                let call = { function_index = index; args = List.combine call.args type_list; } in
                recurse_function_call call def 
            end in
    verify_and_collect_calls [] [] { name = "main"; args = []; location = { line = 0; column = 0 } }

(* Execute the meta-program and decompile all higher-level language features into a C-compatible version *)
let unwrap_module (_mod : Parser._mod) = 
    verify_function_calls _mod


let eval_value value = match value with
    | Parser.StringLiteral s -> s
    | _ -> failwith "Not implemented"

let rec eval_func (_mod : Parser._mod) name args =  match name with
    | "print" -> List.iter (fun value -> print_string @@ eval_value value) args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (fun (call : Parser.function_call) -> eval_func _mod call.name call.args) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod "main" []

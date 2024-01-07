type type_name = string
type variable = { name: string; type_id: type_name }
type active_scope = variable list


(* a function call that calls an existing function with valid arguments *)
type typed_function_call = {
    function_index: int; (* refers to the function_def in the Parser._mod *)
    args: (Parser.value * type_name) list;
}

type types_in_scope = (string (* var name *) * string (* type name *)) list

type error = UnknownFunction of Parser.function_call |
            (* Using typed_function_call here isn't very useful. TODO: Replace with Parser.function_call *)
             ArgumentCountMismatch of (typed_function_call * Parser.function_def) |
             ArgumentTypeMismatch of (int * typed_function_call * Parser.function_def) |
             UnknownIdentifier of (string * Common.location) |
             NoMainFunction

let describe_error error = match error with
    | UnknownFunction call -> Printf.printf "%s: Unknown function `%s`\n" (Common.string_of_location call.location) call.name 
    | ArgumentCountMismatch (_, def) -> Printf.printf "Argument count mismatch for call of function `%s`\n" def.name
    | ArgumentTypeMismatch (_, _, def) -> Printf.printf "Argument type mismatch for call of function `%s`\n" def.name
    | UnknownIdentifier (name, location) -> Printf.printf "%s: Unknwown identifier %s\n" (Common.string_of_location location) name
    | NoMainFunction -> print_endline "No main function found\n"

let type_for_identifier (scope : types_in_scope) identifier location = 
    match List.find_opt (fun (var_name, _) -> var_name = identifier) scope with
    | None -> Error (UnknownIdentifier (identifier, location))
    | Some (_, type_name) -> Ok (type_name)


let create_scope (call : typed_function_call) (func : Parser.function_def) = 
    let aux (arg, (param : Parser.param_def)) = { name = param.param_name; type_id = snd arg; } in
    List.map aux (List.combine call.args func.params)

(* map a function returning a result to a result list, propagating the first error *)
(* TODO: Return all errors instead? *)
let rec list_map_result f l = match l with
| [] -> Ok []
| hd :: tl -> begin match f hd, list_map_result f tl with
    | Ok hd, Ok tl -> Ok (hd :: tl)
    | Error err, _ -> Error err
    | _, Error err -> Error err
    end

(* TODO: Replace with: instantiate_typed_function_def *)
let check_typed_function_call (def : Parser.function_def) call =
    let types_match typed_value (param : Parser.param_def) = match snd typed_value, param.type_constraint with
    | _, None -> true
    | "string", Some Parser.String -> true
    | "int", Some Parser.Integer -> true
    | _, _ -> false in
    let rec aux i args (params : Parser.param_def list) = match args, params with
    | [], [] -> Ok call
    | [], _ :: _ -> Error (ArgumentCountMismatch (call, def))
    | _ :: _, [] -> Error (ArgumentCountMismatch (call, def))
    | arg :: args, param :: params -> if types_match arg param then
            aux (i+1) args params
        else
            Error (ArgumentTypeMismatch (i, call, def)) in
    aux 0 call.args def.params

let create_typed_function_call index (def : Parser.function_def) (call : Parser.function_call) scope = 
    let type_for_identifier name = 
        Option.to_result ~none:(UnknownIdentifier (name, call.location)) (List.find_opt (fun var -> var.name = name) scope) in
    let value_with_type (value : Parser.value) = match value with
    | Parser.StringLiteral _ -> Ok (value, "string")
    | Parser.IntegerLiteral _ -> Ok (value, "int")
    | Parser.Variable name -> Result.map (fun var -> value, var.type_id) (type_for_identifier name)
    | _ -> failwith "Unsupported case in `create_checked_function_call" in
    match list_map_result value_with_type call.args with
    | Error err -> Error err
    | Ok typed_args -> check_typed_function_call def { function_index = index; args = typed_args }

module ModuleBuilder = struct
    type t = { _mod: Parser._mod; functions: typed_function_call list }

    let create _mod = { _mod = _mod; functions = [] }

    let insert_function t scope (call : Parser.function_call) = 
        let definitions = List.filter (fun (_, (def : Parser.function_def)) -> def.name = call.name) (List.mapi (fun i def -> i, def) !t._mod) in
        let return_if_new call = match List.find_opt ((=) call) !t.functions with
            | None -> t := { !t with functions = call :: !t.functions }; Ok(Some call)
            | Some _ -> Ok None in
        let try_insert i def = 
            let call = create_typed_function_call i def call scope in 
            Result.join (Result.map return_if_new call) in
        let map_result_err f res = match res with
        | Ok res -> Ok res
        | Error err -> f err in
        let rec aux definitions err = match definitions with
        | [] -> Error err
        | (i, def) :: tl -> map_result_err (fun err -> aux tl err) (try_insert i def) in
        if call.name = "print" then 
            Ok None 
        else aux definitions (UnknownFunction call)

    let get t = t.functions
end

(* Execute the meta-program and decompile all higher-level language features into a C-compatible version *)
let unwrap_module (_mod : Parser._mod) = 
    let builder = ref (ModuleBuilder.create _mod) in
    let rec inspect_function scope (func : Parser.function_def) = 
        let recurse call = match ModuleBuilder.insert_function builder scope call with
        | Error err -> Error err
        | Ok (Some call) -> 
            let def = List.nth _mod call.function_index in
            inspect_function (create_scope call def) def
        | v -> v in
        let rec aux scope = match scope with
        | [] -> Ok(None)
        | hd :: tl -> Result.join (Result.map (fun _ -> aux tl) (recurse hd)) in
        aux func.scope in
    match List.find_opt (fun (def : Parser.function_def) -> def.name = "main") _mod with
    | None -> Error NoMainFunction
    | Some main -> Result.map (fun _ -> ModuleBuilder.get !builder) (inspect_function [] main)


let eval_value value = match value with
    | Parser.StringLiteral s -> s
    | _ -> failwith "Not implemented"

let rec eval_func (_mod : Parser._mod) name args =  match name with
    | "print" -> List.iter (fun value -> print_string @@ eval_value value) args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (fun (call : Parser.function_call) -> eval_func _mod call.name call.args) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod "main" []

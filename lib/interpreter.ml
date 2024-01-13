type error = UnknownFunction of string * Common.location |
             ArgumentCountMismatch of (Parser.function_call * Parser.function_def) |
             ArgumentTypeMismatch of (int * Parser.function_call * Parser.function_def) |
             UnknownIdentifier of (string * Common.location) |
             NoMainFunction

let describe_error error = match error with
    | UnknownFunction (name, location) -> Printf.printf "%s: Unknown function `%s`\n" (Common.string_of_location location) name 
    | ArgumentCountMismatch (_, def) -> Printf.printf "Argument count mismatch for call of function `%s`\n" def.name
    | ArgumentTypeMismatch (_, _, def) -> Printf.printf "Argument type mismatch for call of function `%s`\n" def.name
    | UnknownIdentifier (name, location) -> Printf.printf "%s: Unknwown identifier %s\n" (Common.string_of_location location) name
    | NoMainFunction -> print_endline "No main function found\n"

type type_name = string
type variable = { name: string; type_id: type_name }
type active_scope = variable list

let type_for_identifier (scope : active_scope) identifier location = 
    match List.find_opt (fun { name;_} -> name = identifier) scope with
    | None -> Error (UnknownIdentifier (identifier, location))
    | Some { type_id;_ } -> Ok (type_id)

let type_of_literal (l : Parser.literal ) = match l with
| Parser.String _ -> "string"
| Parser.Integer _ -> "int"
| Parser.Bool _ -> "bool"

let print_literal (l : Parser.literal option) = match l with
| Some Parser.String s -> print_string s
| Some Parser.Integer i -> print_int i
| Some Parser.Bool b -> if b then print_string "true" else print_string "false"
| None -> failwith "Tried to print None"

let create_scope (func : Parser.function_def) arg_types = 
    let aux (arg, (param : Parser.param_def)) = { name = param.param_name; type_id = arg; } in
    List.map aux (List.combine arg_types func.params)

(* map a function returning a result to a result list, propagating the first error *)
(* TODO: Return all errors instead? *)
let rec map_result_list f l = match l with
| [] -> Ok []
| hd :: tl -> begin match f hd, map_result_list f tl with
    | Ok hd, Ok tl -> Ok (hd :: tl)
    | Error err, _ -> Error err
    | _, Error err -> Error err
    end

let rec iter_result_list f l = match l with
| [] -> Ok ()
| hd :: tl -> match f hd with
    | Error err -> Error err
    | Ok () -> iter_result_list f tl

(* Return an error if no function matches. If a template function matches, return its definition if it needs to be instantiated and None otherwise*)
let find_matching_function (_mod : Parser._mod) (call : Parser.function_call) arg_types = 
    let matches_definition (def : Parser.function_def) = 
        let rec fulfils_type_constraint type_id (type_constraint : Parser.type_constraint) = match type_constraint with
        | Parser.TypeId name -> type_id = name
        | Parser.SumType constraints -> List.exists (fulfils_type_constraint type_id) constraints in
        let aux ((param : Parser.param_def), type_id) =  match param.type_constraint with
        | Some c -> fulfils_type_constraint type_id c
        | None -> true in
        try
            def.name = call.name && List.for_all aux (List.combine def.params arg_types)
        with
            | Invalid_argument _ -> false in
    match List.find_opt matches_definition _mod with
    | None -> Error (UnknownFunction (call.name, call.location))
    | Some def -> Ok def

(*
module TypeCheckedAST = struct
    type function_call = {
        name: string;
        args: (Parser.value * type_name) list;
    }
    type statement = FunctionCall of function_call | Match of Parser.match_statement

    type function_def = {
        name: string;
        params: variable list;
        scope: statement list;
    }
    type builder = { _mod: Parser._mod; functions: function_def list }

    let create_builder _mod = { _mod = _mod; functions = [] }

    let rec convert_function t (def: Parser.function_def) arg_types = 
        let params = create_scope def arg_types in
        let type_of_value location (value : Parser.value) = match value with
        | Parser.Literal l -> Ok begin match l with
            | Parser.String _ -> "string"
            | Parser.Integer _ -> "int"
            | Parser.Bool _ -> "bool"
            end
        | Parser.Variable name -> type_for_identifier params name location
        | _ -> failwith "unsupported case in `type_of_value`" in
        let convert_call (call : Parser.function_call) = 
            let create_call arg_types = { name = call.name; args = List.combine call.args arg_types } in
            let check_definition arg_types = match find_matching_function !t call.name call.location arg_types with
            | Ok None -> Ok (create_call arg_types)
            | Ok (Some def) -> Result.map (fun () -> create_call arg_types) (convert_function t def arg_types)
            | Error err -> Error err in
            match map_result_list (type_of_value call.location) call.args with
            | Error err -> Error err
            | Ok arg_types -> check_definition arg_types in
        let convert_statement (statement : Parser.statement) = match statement with
        | Parser.FunctionCall call -> Result.map (fun call -> FunctionCall call) (convert_call call)
        (* TODO: How should this be actually handeled? *)
        | Parser.Match m -> Ok (Match m) in
        (* TODO: Make sure that function has not already been inserted *)
        match map_result_list convert_statement def.scope with
        | Error err -> Error err
        | Ok scope -> t := { !t with functions = { name = def.name; params = params; scope = scope } :: !t.functions }; Ok()

    let get t = t.functions

    let print (_mod : function_def list) = 
        let print_with_indent indent strings = 
            print_string (String.make indent ' ');
            List.iter print_string strings;
            print_newline () in
      let list_to_string elem_to_string list = 
        let rec aux list = match list with
        | [] -> ""
        | [x] -> elem_to_string x
        | x::xs -> (elem_to_string x) ^ ", " ^ (aux xs) in
        "(" ^ (aux list) ^ ")" in
      let literal_to_string (l: Parser.literal) = match l with
      | Parser.String s -> "\"" ^ String.escaped s ^ "\""
      | Parser.Integer i -> string_of_int i
      | Parser.Bool b -> string_of_bool b in
      let rec match_statement_to_string (match_statement: Parser.match_statement) = 
          "match " ^ (value_to_string match_statement.on_value)
      and value_to_string value = match value with
        | Parser.Literal l -> literal_to_string l
        (* TODO: Implement proper print statement *)
        | Parser.MatchStatement m -> match_statement_to_string m
        | Parser.Variable v -> v
        | Parser.Meta value -> "meta(" ^ value ^ ")"
        | Parser.Call call -> call.name ^ (list_to_string value_to_string call.args)
        | Parser.MemberAccess (obj, member) -> (value_to_string obj) ^ "." ^ member
        | Parser.StructInit s -> 
                s.struct_name ^ "{ " ^ (list_to_string (fun (name, value) -> name ^ ": " ^ (value_to_string value) ^ "; ") s.members) ^ "}" in
        let print_call indent (call : function_call) = 
            print_with_indent indent [
                call.name;
                "(";
                String.concat ", " (List.map (fun (value, _) -> value_to_string value) call.args);
                ")";
            ] in
        let print_statement indent statement = match statement with
        | Match m -> print_endline (match_statement_to_string m)
        | FunctionCall call -> print_call indent call in
        let print_function (func : function_def) = 
            let string_of_var (var : variable) = Printf.sprintf "%s: %s" var.name var.type_id in
            Printf.printf "%s (%s) {\n" func.name (String.concat ", " (List.map string_of_var func.params));
            List.iter (print_statement 4) func.scope;
            print_endline "}\n\n" in
        List.iter print_function _mod
end

(* Execute the meta-program and decompile all higher-level language features into a C-compatible version *)
let unwrap_module (_mod : Parser._mod) = 
    let builder = ref (TypeCheckedAST.create_builder _mod) in
    let types_for_non_template_function (func: Parser.function_def) = 
        let is_template_param (param: Parser.param_def) = match param.type_constraint with
        | None -> true
        | Some (Parser.SumType _) -> true
        | _ -> false in 
        let get_type (param: Parser.param_def) = match Option.get param.type_constraint with 
        | Parser.TypeId name -> name
        | Parser.SumType _ -> failwith "Sum type in types_for_non_template_function" in
        if List.exists is_template_param func.params then None
        else Some (List.map get_type func.params) in
    let convert_non_template_function (func: Parser.function_def) = match types_for_non_template_function func with
    | None -> Ok ()
    | Some arg_types -> TypeCheckedAST.convert_function builder func arg_types in
    let conversion_result = map_result_list convert_non_template_function _mod in
    Result.map (fun _ -> TypeCheckedAST.get !builder) conversion_result

*)

let unwrap_module _ = failwith "Needs some fixing"
module TypeCheckedAST = struct
    let print _ = failwith "Needs some fixing"
end


module ModuleInterpreter = struct
    (* TODO: Start using a new struct instead of variable that also covers void (e.g for function calls)
    and contains the type *)
    type variable = string * Parser.literal
    type scope = variable list
    type t = { _mod: Parser._mod; scope_stack: scope list }

    let create _mod = { _mod = _mod; scope_stack = [] }

    let scope_val t name = List.find_opt (fun (n, _) -> n = name) (List.hd t.scope_stack)

    let type_of_value t location (value : Parser.value) = match value with 
    | Parser.Literal l -> Ok(type_of_literal l)
    | Parser.Variable name -> Option.to_result ~none:(UnknownIdentifier (name, location)) (Option.map (fun (_, l) -> type_of_literal l) (scope_val t name))
    | _ -> failwith "Not implemented case in ModuleInterpreter::type_of_value"

    let insert_variable t name value = 
        let rec replace variables = match variables with
        | [] -> [name, value]
        | (n, _) :: tl when n = name -> (name, value) :: tl
        | _ :: tl -> replace tl in
        let replace_in_stack stack = match stack with
        | [] -> failwith "logic error in insert_variable"
        | hd :: tl -> (replace hd) :: tl in
        { t with scope_stack = replace_in_stack t.scope_stack }
 
    let rec eval_value t location (value : Parser.value) = match value with
    | Parser.Literal l -> Ok (Some l)
    | Parser.Variable name -> Option.to_result ~none:(UnknownIdentifier (name, location)) (Option.map (fun (_, l) -> Some l) (scope_val t name))
    | Parser.Call call -> eval_function_call t call
    | _ -> failwith "Not implemented case in ModuleInterpreter::type_of_value"
    and push_scope t (def : Parser.function_def) (call : Parser.function_call) = 
        let aux ((param : Parser.param_def), (value : Parser.value)) = 
            let value = eval_value t call.location value in
            (* Unwrapping result here because we've done a type check earlier already if this function is called correctly *)
            param.param_name, Option.get (Result.get_ok value) in
        let scope = List.map aux (List.combine def.params call.args) in
        { t with scope_stack = scope :: t.scope_stack }
    and eval_function_call t (call : Parser.function_call) =
        let eval_function (def : Parser.function_def) (call : Parser.function_call) = 
            let t = push_scope t def call in
            let aux (statement : Parser.statement) = match statement with
            | Parser.FunctionCall call -> eval_function_call t call
            | Parser.Match match_statement -> eval_match_statement t match_statement in
            Result.map (fun _ -> None) (map_result_list aux def.scope) in
        match call.name with
        | "print" -> Result.map (fun _ -> print_newline (); None) (iter_result_list (print_value t call.location) call.args)
        | _ -> 
            let arg_types = map_result_list (type_of_value t call.location) call.args in
            match Result.join (Result.map (find_matching_function t._mod call) arg_types) with
            | Error err -> Error err
            | Ok def -> eval_function def call
    and eval_match_statement t match_statement = 
        match type_of_value t match_statement.match_location match_statement.on_value with
        | Error err -> Error err
        | Ok type_id ->
            let row = List.find_opt (fun (row : Parser.match_statement_row) -> row.matched_type = type_id) match_statement.rows in
            (* TODO: Check beforehand that match expression covers all types *)
            match row with
            | None -> failwith "match expression didn't cover value"
            | Some row ->
                let value = Result.get_ok (eval_value t match_statement.match_location match_statement.on_value) in
                let t = insert_variable t row.identifier (Option.get value) in
                eval_value t match_statement.match_location (row.value)
    and print_value t location value = 
        let value = eval_value t location value in
        Result.map (fun value -> print_literal value) value
end

let run _mod = 
    let interpreter = ModuleInterpreter.create _mod in
    let res = ModuleInterpreter.eval_function_call interpreter { name="main"; args = []; location={line=0; column=0}} in
    match res with
    | Ok _ -> ()
    | Error err -> describe_error err

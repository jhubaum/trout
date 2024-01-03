type error = UnknownFunction

exception InterpreterError of error

type metacall_description = { function_name: string; meta_param_indices: int list }

let transform_value transformer (_mod : Parser._mod) = 
    let rec transform value = match value with
    | Parser.Call call -> let args = List.map transform call.args in 
        transformer @@ Parser.Call { call with args = args }
    | Parser.MemberAccess (value, name) -> transformer @@ Parser.MemberAccess (transformer value, name)
    | Parser.StructInit s -> let members = List.map (fun (name, value) -> name, transformer value) s.members in
        transformer @@ Parser.StructInit { s with members = members }
    | value -> transformer value in
    let transform_statement (statement : Parser.statement) = 
        (* A hack to make the transformer work on statements in a function. TODO: Figure out how to do this properly *)
        match transform @@ Parser.Call statement with
        | Parser.Call call -> call
        | _ -> failwith "Logic error in transform_value. Returned value isn't a call" in
    let transform_function (func : Parser.function_def) = { func with scope = List.map transform_statement func.scope } in
    List.map transform_function _mod

let find_metacall_descriptions (_mod : Parser._mod) = 
    let meta_args (statement : Parser.mod_statement) = 
        let rec aux (value : Parser.value) = match value with
        | Parser.Meta value -> [value]
        | Parser.Call call -> List.flatten (List.map aux call.args)
        | Parser.MemberAccess (value, _) -> aux value
        | _ -> [] in
        let aux2 margs (call : Parser.function_call) = List.fold_left (fun acc i -> (aux i) @ acc) margs call.args in
        List.fold_left aux2 [] statement.scope in
    let find_param_index (func : Parser.function_def) param_name =
        let rec aux (params : Parser.param_def list) index = match params with
        (* TODO: One possibility for this failure is a meta call on a global object. Handle this *)
        (* TODO: Add location to this error and replace exception with result type *)
        | [] -> raise @@ InterpreterError UnknownFunction
        | hd :: _ when hd.param_name = param_name -> index
        | _ :: tl -> aux tl (index+1) in
        aux func.params 0 in
    let aux (statement : Parser.mod_statement) = match meta_args statement with
    | [] -> None
    | meta_params -> Some { function_name = statement.name; meta_param_indices = List.map (find_param_index statement) meta_params } in
    List.filter_map aux _mod

let find_metacall_instantiations _mod desc = 
    let instantiations = ref [] in
    (* This will produce erroneous code if objects are passed through several functions. TODO: Fix *)
    let track_call (call : Parser.function_call) = 
        let instantiation = List.map (List.nth call.args) desc.meta_param_indices in
        instantiations := instantiation :: !instantiations in
    (* This will fail if meta function is called over several layers *)
    let rec iter_call (call : Parser.function_call) = 
        if call.name = desc.function_name then 
            track_call call 
        else List.iter begin fun value -> match value with | Parser.Call call -> iter_call call | _ -> () end call.args in
    let find_definition value = 
        let rec aux definitions name = match definitions with
        | [] -> failwith "Logic error in find_definition"
        | (hd : Parser.function_def) :: _ when hd.name = name -> hd
        | _ :: tl -> aux tl name in
        match value with
        | Parser.Variable name -> aux _mod name
        | _ -> failwith "find_definition on invalid value" in
    List.iter (fun (mod_statement : Parser.mod_statement) -> List.iter iter_call mod_statement.scope) _mod;
    desc, List.map (fun inst -> List.map find_definition inst) !instantiations

let meta_call_transform nth_to_string args (desc : metacall_description) =
    let name_params = List.map nth_to_string desc.meta_param_indices in
    let filtered_args = List.filteri (fun i _ -> List.for_all ((<>) i) desc.meta_param_indices) args in
    desc.function_name ^ "__" ^ (String.concat "_" name_params), filtered_args

let instantiate_if_matching metacall_desc instantiations _mod (func : Parser.function_def) =
    let nth_arg_name (instantiations : (Parser.param_def * Parser.function_def) list) i = 
        let (param : Parser.param_def) = List.nth func.params i in
        let rec aux (instantiations : (Parser.param_def * Parser.function_def) list) = match instantiations with
        | [] -> failwith "Logic error in `instantiate_if_matching:arg_name_for_index`"
        | hd :: _ when (fst hd).param_name = param.param_name -> (snd hd).name
        | _ :: tl -> aux tl in
        aux instantiations in
    let instantiate instantiation =
        let def_to_struct_value (def : Parser.function_def) = 
            Parser.StructInit { struct_name = "Function"; members = ["name", Parser.StringLiteral def.name] } in
        let meta_param = List.combine (List.map (List.nth func.params) metacall_desc.meta_param_indices) instantiation in
        let find_and_replace (name : string) =
            let rec aux params = match params with
            | [] -> failwith "Logic error in instantiate_if_matching"
            | (hd : Parser.param_def * Parser.function_def) :: _ when (fst hd).param_name = name -> snd hd
            | _ :: tl -> aux tl in
            aux meta_param in
        let rec replace_value value = match value with
            | Parser.Meta name -> let def = find_and_replace name in
                def_to_struct_value def
            | Parser.MemberAccess (value, name) -> Parser.MemberAccess ((replace_value value), name)
            | Parser.Call call -> Parser.Call { call with args = List.map replace_value call.args }
            | value -> value in
        let replace_call (call : Parser.function_call) = { call with args = List.map replace_value call.args } in
        let name, params = meta_call_transform (nth_arg_name meta_param) func.params metacall_desc in
        { 
            func with name = name; params = params; scope = List.map replace_call func.scope;
        } in
    if func.name = metacall_desc.function_name then
        List.map instantiate instantiations
    else
        [func]

let replace_meta_calls_transformer (meta_calls : metacall_description list) (value : Parser.value) =
    let nth_name (call : Parser.function_call) i = match List.nth call.args i with
    | Parser.Variable name -> name
    | _ -> failwith "Unsupported arg value in `replace_meta_calls_transformer`" in
    let matches meta_call (call : Parser.function_call) = meta_call.function_name = call.name in
    let rec aux meta_calls call = match meta_calls with
    | [] -> value
    | hd :: _ when matches hd call -> let name, args = meta_call_transform (nth_name call) call.args hd in
        Parser.Call { call with name = name; args=args }
    | _ :: tl -> aux tl call in
    match value with
    | Parser.Call call -> aux meta_calls call
    | value -> value

let simplify_struct_access_transformer (value : Parser.value) =
    let rec aux members name = match members with
    | [] -> failwith "No such named member in `simplify_struct_access_transformer`"
    | (n, value) :: _ when n=name -> value
    | _ :: tl -> aux tl name in
    match value with
    | Parser.MemberAccess (Parser.StructInit s, name) -> aux s.members name
    | value -> value

let unwrap_meta_calls (_mod : Parser._mod) =
    let meta_calls = find_metacall_descriptions _mod in
    let with_instantiations = List.map (find_metacall_instantiations _mod) meta_calls in
    let insert_instantiation (_mod : Parser._mod) desc instantiations =
        List.flatten (List.map (instantiate_if_matching desc instantiations _mod) _mod) in
    let rec aux (_mod : Parser._mod) instantiations = match instantiations with
    | [] -> _mod
    | (desc, instantiations) :: tl -> aux (insert_instantiation _mod desc instantiations) tl in
    let rec transformations _mod transformers = match transformers with
    | [] -> _mod
    | hd :: tl -> transformations (transform_value hd _mod) tl in
    transformations (aux _mod with_instantiations) [
        (replace_meta_calls_transformer meta_calls);
        simplify_struct_access_transformer
    ]

let eval_value value = match value with
    | Parser.StringLiteral s -> s
    | _ -> failwith "Not implemented"

let rec eval_func (_mod : Parser._mod) name args =  match name with
    | "print" -> List.iter (fun value -> print_string @@ eval_value value) args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (fun (call : Parser.function_call) -> eval_func _mod call.name call.args) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod "main" []

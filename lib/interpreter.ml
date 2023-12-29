type error = UnknownFunction

exception InterpreterError of error

type metacall_description = { function_name: string; meta_param_indices: int list }

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
        let rec aux params index = match params with
        (* TODO: One possibility for this failure is a meta call on a global object. Handle this *)
        (* TODO: Add location to this error and replace exception with result type *)
        | [] -> raise @@ InterpreterError UnknownFunction
        | hd :: _ when hd = param_name -> index
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

let instantiate_if_matching metacall_desc instantiations _mod (func : Parser.function_def) =
    let instantiate instantiation =
        let def_to_struct_value (def : Parser.function_def) = 
            Parser.StructInit { struct_name = "Function"; members = ["name", Parser.Literal def.name] } in
        let meta_param = List.combine (List.map (List.nth func.params) metacall_desc.meta_param_indices) instantiation in
        let find_and_replace name =
            let rec aux params = match params with
            | [] -> failwith "Logic error in instantiate_if_matching"
            | hd :: _ when (fst hd) = name -> snd hd
            | _ :: tl -> aux tl in
            aux meta_param in
        let rec replace_value value = match value with
            | Parser.Meta name -> let def = find_and_replace name in
                def_to_struct_value def
            | Parser.MemberAccess (value, name) -> Parser.MemberAccess ((replace_value value), name)
            | Parser.Call call -> Parser.Call { call with args = List.map replace_value call.args }
            | value -> value in
        let replace_call (call : Parser.function_call) = { call with args = List.map replace_value call.args } in
        { 
            func with
            name = func.name ^ "__" ^ (String.concat "_" (List.map (fun (_, (def : Parser.function_def)) -> def.name) meta_param)); 
            scope = List.map replace_call func.scope;
        } in
    if func.name = metacall_desc.function_name then
        List.map instantiate instantiations
    else
        [func]

let unwrap_meta_calls (_mod : Parser._mod) =
    let meta_calls = find_metacall_descriptions _mod in
    let with_instantiations = List.map (find_metacall_instantiations _mod) meta_calls in
    let insert_instantiation (_mod : Parser._mod) desc instantiations =
        List.flatten (List.map (instantiate_if_matching desc instantiations _mod) _mod) in
    let rec aux (_mod : Parser._mod) instantiations = match instantiations with
    | [] -> _mod
    | (desc, instantiations) :: tl -> aux (insert_instantiation _mod desc instantiations) tl in
    aux _mod with_instantiations

let eval_value value = match value with
    | Parser.Literal s -> s
    | _ -> failwith "Not implemented"

let rec eval_func (_mod : Parser._mod) name args =  match name with
    | "print" -> List.iter (fun value -> print_string @@ eval_value value) args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (fun (call : Parser.function_call) -> eval_func _mod call.name call.args) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod "main" []

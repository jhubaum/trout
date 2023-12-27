let eval_value value = match value with
    | Parser.Literal s -> s
    | _ -> failwith "Not implemented"

let rec eval_func (_mod : Parser._mod) name args =  match name with
    | "print" -> List.iter (fun value -> print_string @@ eval_value value) args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (fun (call : Parser.function_call) -> eval_func _mod call.name call.args) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod "main" []

let rec eval_func _mod (func : Parser.function_call) = match func.name with
    | "print" -> List.iter print_string func.args
    | func -> match List.find_opt (fun (def : Parser.mod_statement) -> def.name = func) _mod with
        | Some func -> List.iter (eval_func _mod) func.scope
        | None -> failwith ("Function " ^ func ^ " not found")


let run _mod = eval_func _mod { name = "main"; args = [] }

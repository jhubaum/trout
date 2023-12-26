open Parser;;

let string_of_module _mod =
    let indent_with len = String.make len ' ' in
    let string_of_function_call indent (call : function_call) = 
        (indent_with indent) ^ call.name ^ "(" ^ (String.concat ", " call.args) ^ ")" in
    let string_of_scope scope indent = 
        "{" ^ (String.concat "\n" (List.map (string_of_function_call (indent+2)) scope)) ^ "}" in
    let string_of_func_def func = func.name ^ (string_of_scope func.scope 2) in
    List.fold_left (fun s func -> s ^ (string_of_func_def func) ^ "\n") "" _mod



let () = 
    let _mod = Parser.parse_file "test.trout" in
    print_endline @@ string_of_module _mod;

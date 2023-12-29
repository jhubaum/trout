open Parser;;

let print_with_indent indent strings = 
  print_string (String.make indent ' ');
  List.iter print_string strings;
  print_newline ()


let print_module _mod = 
  let list_to_string elem_to_string list = 
    let rec aux list = match list with
    | [] -> ""
    | [x] -> elem_to_string x
    | x::xs -> (elem_to_string x) ^ ", " ^ (aux xs) in
    "(" ^ (aux list) ^ ")" in
  let rec value_to_string value = match value with
    | Literal s -> "\"" ^ String.escaped s ^ "\""
    | Variable v -> v
    | Meta value -> "meta(" ^ (value_to_string value) ^ ")"
    | Call call -> call.name ^ (list_to_string value_to_string call.args)
    | MemberAccess (obj, member) -> (value_to_string obj) ^ "." ^ member in
  let args_list_to_string = list_to_string value_to_string in
  let print_scope indent scope = 
    let print_statement (statement : statement) = 
      print_with_indent indent [statement.name; args_list_to_string statement.args; ";"] in
    List.iter print_statement scope in
  let print_function indent func = 
    print_with_indent indent ["fn "; func.name; list_to_string (fun s -> s) func.params; " {"];
    print_scope (indent+4) func.scope;
    print_with_indent indent ["}\n"] in
  List.iter (print_function 0) _mod

type options = { filename: string; unwrap: bool; }

let parse_args =
  let input_file = ref ""  in 
  let flag = ref false in
  let usage_msg = "Usage: trout [options] filename" in
  let speclist =
    [
      ("--unwrap", Arg.Set flag, "Print the unwrapped code for the given file")
    ] in
  Arg.parse speclist (fun s -> input_file := s) usage_msg;
  { filename = !input_file; unwrap = !flag }

let () =
  let args = parse_args in
  match Parser.parse_file args.filename with
  | Error e -> Parser.describe_error e
  | Ok _mod -> if args.unwrap then print_module _mod else Interpreter.run _mod;

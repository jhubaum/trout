open Parser;;

let print_with_indent indent strings = 
  print_string (String.make indent ' ');
  List.iter print_string strings;
  print_newline ()


let print_module _mod = 
  let value_to_string = String.escaped in
  let args_list_to_string args = "(" ^ (String.concat ", " (List.map value_to_string args)) ^ ")" in
  let print_scope indent scope = 
    let print_statement (statement : statement) = 
      print_with_indent indent [statement.name; args_list_to_string statement.args; ";"] in
    List.iter print_statement scope in
  let print_function indent func = 
    print_with_indent indent ["fn "; func.name; "() {"];
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
  let _mod = Parser.parse_file args.filename in
  if args.unwrap then print_module _mod else Interpreter.run _mod;

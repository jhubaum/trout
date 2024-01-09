let unwrap_module (_mod : Parser._mod) = match (Interpreter.unwrap_module _mod) with
  | Error err -> Interpreter.describe_error err
  | Ok _mod -> Interpreter.TypeCheckedAST.print _mod

type options = { filename: string; unwrap: bool; }

let parse_args =
  let input_file = ref ""  in 
  let flag = ref false in
  let usage_msg = "Usage: trout [options] filename" in
  let speclist =
    [
      ("--unwrap", Arg.Set flag, "Unwrap the code into a C-like version, executing the meta-program and decompiling all higher-level language features")
    ] in
  Arg.parse speclist (fun s -> input_file := s) usage_msg;
  { filename = !input_file; unwrap = !flag }

let () =
  let args = parse_args in
  match Parser.parse_file args.filename with
  | Error e -> Parser.describe_error e
  | Ok _mod -> if args.unwrap then unwrap_module _mod else Interpreter.run _mod;

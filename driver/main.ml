let dump_ast ast =
  Format.printf "%s@." (Ast.show ast)

let dump_typedast tast =
  Format.printf "%s@." (Typed_ast.show tast)

let main anonymous =
  try
    let inc = open_in anonymous in
    let lexbuf = Lexing.from_channel inc in
    let ast = Parser.main Lexer.token lexbuf in
    begin if !Flags.print_ast then dump_ast ast end;
    close_in inc;
  with
  | _ as exn -> begin
      let (x, y, z) = ! Lexer.loc in
      Printf.printf "error: line %d, %d %d %d\n" (! Lexer.line) x y z;
      raise exn
    end

let usage =
  "loop"

let () =
  Arg.parse Options.spec main usage

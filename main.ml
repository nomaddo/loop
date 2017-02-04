let _ =
  if not (Array.length Sys.argv = 2) then exit 1
  else
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    Ast.sexp_of_t result
    |> Sexplib.Sexp.output stdout;
    print_endline "";
    flush stdout;
    exit 0

let main () =
  if not (Array.length Sys.argv = 2) then exit 1
  else try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    Ast.sexp_of_t result
    |> Sexplib.Sexp.output stdout;
    print_endline "";
    flush stdout;
    exit 0
    with
    | _ -> begin
        let (x, y, z) = ! Lexer.loc in
        Printf.printf "error: line %d, %d %d %d\n" (! Lexer.line) x y z;
        exit 1
      end

let _ = main ()

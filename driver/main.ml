open Batteries

let dump_ast ast =
  Format.printf "%s@." (Ast.show ast)

let dump_tast tast =
  Format.printf "%s@." (Typed_ast.show (fun fmt e -> ()) tast)

let dump_ila top =
  Format.printf "%a@." Dump.dump top

let dump_ilb top =
  Format.printf "%a@." Ilb_dump.dump top

let typ anonymous =
  let mod_name =
    Filename.basename anonymous
    |> Filename.chop_extension in
  let inc = open_in anonymous in
  let lexbuf = Lexing.from_channel inc in
  let ast = Parser.main Lexer.token lexbuf in
  begin if !Flags.print_ast then dump_ast ast end;
  let intf, tast = Typing.implementation mod_name ast in
  begin if !Flags.print_tast then dump_tast tast end;
  intf, tast

let main anonymous =
  let mod_name =
    Filename.basename anonymous
    |> Filename.chop_extension in
  (* try *)
    let inc = open_in anonymous in
    let lexbuf = Lexing.from_channel inc in
    let ast = Parser.main Lexer.token lexbuf in
    begin if !Flags.print_ast then dump_ast ast end;
    let intf, tast = Typing.implementation mod_name ast in
    begin if !Flags.print_tast then dump_tast tast end;
    let top = Ir_main.transl mod_name intf tast in
    begin if !Flags.print_ila then dump_ila top end;
    Ila_check.check top;
    let btop = Asmgen.transl top in
    begin if !Flags.print_ilb then dump_ilb btop end;
    close_in inc
  (* with *)
  (* | _ as exn -> begin *)
  (*     let (x, y, z) = ! Lexer.loc in *)
  (*     Printf.printf "error: line %d, %d %d %d\n" (! Lexer.line) x y z; *)
  (*     raise exn *)
  (*   end *)

let usage =
  "loop"

let () =
  Arg.parse Options.spec main usage

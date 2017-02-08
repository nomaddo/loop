let add l e =
  l := e :: !l

let ast =
  ("--ast", Arg.Unit (fun () -> Flags.print_ast := true), "")

let tast =
  ("--tast", Arg.Unit (fun () -> Flags.print_tast := true), "")

let intf =
  ("-I", Arg.String (fun s -> add Flags.search_path s), "")

let spec =
  [ ast; intf ]

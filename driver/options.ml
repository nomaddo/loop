(* TODO 本に書いてしまったので細かくデバッグオプションについて書く *)

let add l e =
  l := e :: !l

let on  r () = r := true
let off r () = r := false

let ast =
  ("--ast", Arg.Unit (on Flags.print_ast), "")

let tast =
  ("--tast", Arg.Unit (on Flags.print_tast), "")

let ila =
  ("--ila", Arg.Unit (on Flags.print_ila), "")

let ilb =
  ("--ilb", Arg.Unit (on Flags.print_ilb), "")

let intf =
  ("-I", Arg.String (fun s -> add Flags.search_path s), "")

let dflag =
  ("-d", Arg.Unit (on Flags.dflag), "")

let opt =
  ("-O", Arg.Unit (on Flags.opt), "")

let noopt =
  ("-O0", Arg.Unit (off Flags.opt), "")

let ra =
  ("--show-vars", Arg.Unit (on Flags.show_valid_vars), "")

let spec =
  [ ast; tast; ila; ilb; intf; dflag; opt; noopt; ra ]

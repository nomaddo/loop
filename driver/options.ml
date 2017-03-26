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

let show_vars =
  ("--show-vars", Arg.Unit (on Flags.show_valid_vars), "")

let show_stack_layout =
  ("--show-stack-layout", Arg.Unit (on Flags.show_stack), "")

let show_dyn_arrays =
  ("--show-dyn-arrays", Arg.Unit (on Flags.show_dyn_arrays), "")

let toilb =
  ("--toilb", Arg.Unit (on Flags.toilb), "")

let sa =
  ("--sa", Arg.Unit (on Flags.sa), "")

let ra =
  ("--sa", Arg.Unit (on Flags.ra), "")

let spec =
  [ ast; tast; ila; ilb; intf; dflag; opt; noopt; show_vars;
    toilb; sa; ra; show_stack_layout; show_dyn_arrays ]

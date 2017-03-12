let add l e =
  l := e :: !l

let on  r () = r := true
let off r () = r := false

let ast =
  ("--ast", Arg.Unit (on Flags.print_ast), "")

let tast =
  ("--tast", Arg.Unit (on Flags.print_tast), "")

let intf =
  ("-I", Arg.String (fun s -> add Flags.search_path s), "")

let dflag =
  ("-d", Arg.Unit (on Flags.dflag), "")

let opt =
  ("-O", Arg.Unit (on Flags.opt), "")

let noopt =
  ("-O0", Arg.Unit (off Flags.opt), "")

let spec =
  [ ast; intf; dflag; opt; noopt ]

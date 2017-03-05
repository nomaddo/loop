open Ir

let transl mod_name intf tast =
  let top = Transl.implementation mod_name intf tast in
  List.iter Simplify.func top.funcs;
  top

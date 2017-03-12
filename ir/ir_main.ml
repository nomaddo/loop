open Ir

let transl mod_name intf tast =
  let top = Transl.implementation mod_name intf tast in
  if !Flags.opt then
    List.iter (fun func ->
      while Simplify.func func do
        ()
      done) top.funcs;
  top

(* register allocation
   allocate, call, callmを全部消して
   属性にする
*)

let transl_func

let transl {Ir.memories; funcs} =
  let funcs = List.map transl_funcs funcs in
  { memories; funcs }

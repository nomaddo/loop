(* register allocation
   allocate, call, callmを全部消して
   属性にする
*)

let transl_func func =


let transl {Ir.memories; funcs} =
  let funcs = List.map (transl_funcs memories) funcs in
  { memories; funcs }

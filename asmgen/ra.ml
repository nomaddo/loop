(* register allocation
   tvの属性にレジスタ割付した結果を書き込む
*)

let transl_func func =
let transl {Ir.memories; funcs} =
  let funcs = List.map (transl_funcs memories) funcs in
  { memories; funcs }

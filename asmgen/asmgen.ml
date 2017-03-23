let transl top =
  let top = Toilb.transl top in
  Format.printf "%a@." Ilb_dump.dump top;
  Sa.transl top;
  List.iter (fun func ->
    while Ilb_simplify.remove_constant_move func do
      Ilb_simplify.remove_redundant_instr func
    done) top.Ir.funcs;
  Ra.transl top;
  top

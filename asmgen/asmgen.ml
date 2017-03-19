let transl top =
  let top = Toilb.transl top in
  Format.printf "%a@." Ilb_dump.dump top;
  Sa.transl top;
  List.iter (fun func ->
    while Ilb_simplify.func func do
      ()
    done) top.funcs;
  top

let transl top =
  let top = Toilb.transl top in
  Format.printf "%a@." Ilb_dump.dump top;
  Sa.transl top;
  top

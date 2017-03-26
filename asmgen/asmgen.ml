let transl top =
  let top = Toilb.transl top in
  Etc.dmsg Flags.toilb (fun () ->
      Format.printf "%a@." Ilb_dump.dump top;
  );
  Sa.transl top;
  Etc.dmsg Flags.sa (fun () ->
      Format.printf "%a@." Ilb_dump.dump top;
  );
  begin if !Flags.opt then
    List.iter (fun func ->
      while Ilb_simplify.remove_constant_move func do
        Ilb_simplify.remove_redundant_instr func
      done) top.Ir.funcs
  end;
  Ra.transl top;
  Etc.dmsg Flags.ra (fun () ->
      Format.printf "%a@." Ilb_dump.dump top;
  );
  top

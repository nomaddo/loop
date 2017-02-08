let gen_sym =
  let cnt = ref 0 in
  fun () -> begin
      let ret = !cnt in
      incr cnt;
      ret
    end

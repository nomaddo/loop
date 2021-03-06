let (++) = (@@)

let not_implemented_yet () =
  failwith "not implemented yet!"

let fold_rev f acc l =
  List.fold_left (fun (acc, x) d -> (acc, f acc d :: x)) (acc, []) l
  |> (fun (acc, l) -> acc, List.rev l)

let fold_rev2 f acc l1 l2 =
  List.fold_left2 (fun (acc, x) y z -> (acc, f acc y z :: x)) (acc, []) l1 l2
  |> (fun (acc, l) -> acc, List.rev l)

let str_cnt = ref 0

let cnt () =
  let n = !str_cnt in
  incr str_cnt; n

let flag_on r =
  r := true

let flag_off r =
  r := false

let dmsg flag f =
  if !flag then f () else ()

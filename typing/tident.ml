type ident = {name: string; id: int}
[@@deriving show]

type path =
  | Tident of ident
  | Tpath  of ident * path
[@@deriving show]

let make_ident s =
  { name = s; id = Btypes.gen_sym () }

let ident tpath =
  match tpath with
  | Tident ident -> ident
  | _ -> failwith "ident_path"

let path s =
  let id = make_ident s in
  Tident id

let recreate_ident ident =
  { ident with id = Btypes.gen_sym () }

let rec recreate_path = function
  | Tident ident -> Tident (recreate_ident ident)
  | Tpath (ident, p) ->
     Tpath (recreate_ident ident, recreate_path p)

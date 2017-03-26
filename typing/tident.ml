type ident = {name: string; id: int}
[@@deriving show]

type path =
  | Tident of ident
[@@deriving show]

let make_ident s =
  { name = s; id = Btypes.gen_sym () }

let ident tpath =
  match tpath with
  | Tident ident -> ident

let get_name tpath = (ident tpath).name

let path s =
  let id = make_ident s in
  Tident id

let recreate_ident ident =
  { ident with id = Btypes.gen_sym () }

let rec recreate_path = function
  | Tident ident -> Tident (recreate_ident ident)

let rec make_label ?(is_top=true) modname tpath =
  match tpath with
  | Tident id ->
      if is_top then
        Format.sprintf "%s" id.name
      else id.name

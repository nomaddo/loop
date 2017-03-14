open Typ

type opcore =
  | Iconst of int
  | Rconst of string
  | Memory of int
  | Var    of Tident.path      (* variables *)
  | Tv     of int              (* temporary variables *)
  | Sp                         (* stack pointer, ilbにのみ登場する *)
[@@deriving show]

type operand =
  { opcore: opcore; typ: typ;
    mutable attrs: operand_attr list }

and operand_attr =
  | Tpath of Tident.path
  | Ind
  | Bct
[@@deriving show]

let new_operand ?(attrs=[]) opcore typ =
  { opcore; typ; attrs }

let count, reset =
  let r = ref 0 in
  ((fun () -> incr r; !r), (fun () -> r := 0))

let new_tv ?(attrs=[]) ?(opt=None) typ =
    { opcore = Tv (count ()); typ;
      attrs = attrs @ match opt with None -> []
                                   | Some tpath -> [Tpath tpath]}

let new_var =
  fun ?(attrs=[]) typ ->
    let tpath = Tident.path ("_" ^ string_of_int (count ())) in
    { opcore = Var tpath; typ; attrs; }

let new_name ?(attrs=[]) tpath typ =
  new_operand ~attrs (Var tpath) typ

let is_constant op = match op.opcore with
  | Iconst _ | Rconst _ -> true
  | _ -> false

open Typ

type opcore =
  | Iconst of int
  | Rconst of string
  | Memory of int
  | Var    of Tident.path      (* variables *)
  | Tv     of int              (* temporary variables *)
[@@deriving show]

type operand =
  { opcore: opcore; typ: typ;
    mutable attrs: operand_attr list }

and operand_attr =
  | Ind
  | Bct
  | Upper                       (* Var only *)
  | Lower                       (* Var only *)
[@@deriving show]

let new_operand ?(attrs=[]) opcore typ =
  { opcore; typ; attrs }

let count, reset =
  let r = ref 0 in
  ((fun () -> incr r; !r), (fun () -> r := 0))

let new_tv typ =
    { opcore = Tv (count ()); typ; attrs = [] }

let new_var =
  fun ?(attrs=[]) typ ->
    let tpath = Tident.path ("@" ^ string_of_int (count ())) in
    { opcore = Var tpath; typ; attrs; }

let new_name ?(attrs=[]) tpath typ =
  new_operand ~attrs (Var tpath) typ

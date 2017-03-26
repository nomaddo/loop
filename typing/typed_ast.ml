type direction = Ast.direction
[@@deriving show]

type 'a expr_core =
  | Var    of Tident.path
  | Iconst of int
  | Rconst of string
  | Call   of Tident.path *  'a expr list
  | Aref   of Tident.path *  'a expr list

and 'a expr = { expr_core: 'a expr_core; expr_typ: 'a typ; expr_intf: 'a }

and 'a typ =
  | Void
  | Int
  | Real
  | Array of 'a typ *  'a expr
  | Lambda of 'a typ list *  'a typ
[@@deriving show]

type 'a decl =
  | Decl   of 'a typ * Tident.path *  'a expr option
  | If     of 'a expr *  'a decl list *  'a decl list option
  | Assign of Tident.path *  'a expr
  | Astore of Tident.path *  'a expr list *  'a expr
  | For    of Tident.path *  'a expr * direction *  'a expr *  'a expr option *  'a decl list
  | While  of 'a expr *  'a decl list
  | Call   of Tident.path *  'a expr list * 'a typ
  | Return of 'a expr option
[@@deriving show]

type  'a top_decl =
  | Fundef     of  'a typ * Tident.path * (Tident.path * 'a typ) list *  'a decl list
  | Global_var of  'a typ * Tident.path *  'a expr option
  | Prim       of  'a typ * Tident.path * string
[@@deriving show]

type 'a t = 'a top_decl list
[@@deriving show]

let ret_typ = function
  | Lambda (_, ret) -> ret
  | _ -> failwith "ret_typ"

let new_expr expr_core expr_typ intf =
  {expr_core; expr_typ; expr_intf=intf}

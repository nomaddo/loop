type direction = Ast.direction
[@@deriving show]

type  expr_core =
  | Var    of Tident.path
  | Iconst of int
  | Rconst of string
  | Call   of Tident.path *  expr list
  | Aref   of Tident.path *  expr list

and expr = { expr_core: expr_core; expr_typ: typ }
[@@deriving show]

and typ =
  | Void
  | Int
  | Real
  | Array of  typ *  expr
  | Lambda of  typ list *  typ
[@@deriving show]

type  decl =
  | Decl   of  typ * Tident.path *  expr option
  | If     of  expr *  decl list *  decl list option
  | Assign of Tident.path *  expr
  | Astore of Tident.path *  expr list *  expr
  | For    of Tident.path *  expr * direction *  expr *  expr option *  decl list
  | While  of  expr *  decl list
  | Call   of Tident.path *  expr list * typ
  | Return of  expr
[@@deriving show]

type  top_decl =
  | Fundef     of  typ * Tident.path * (Tident.path * typ) list *  decl list
  | Global_var of  typ * Tident.path *  expr option
  | Prim       of  typ * Tident.path * string
[@@deriving show]

type t = top_decl list
[@@deriving show]

let ret_typ = function
  | Lambda (_, ret) -> ret
  | _ -> failwith "ret_typ"

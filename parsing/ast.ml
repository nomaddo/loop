type direction =
   | To
   | Downto
[@@deriving show]

type  expr =
  | Var    of Pident.path
  | Iconst of int
  | Rconst of string
  | Call   of Pident.path *  expr list
  | Aref   of Pident.path *  expr list
[@@deriving show]

type  typ =
  | Void
  | Int
  | Real
  | Array of  typ *  expr
  | Lambda of  typ list *  typ
[@@deriving show]

type  decl =
  | Decl   of typ * Pident.path *  expr option
  | If     of expr *  decl list *  decl list option
  | Assign of Pident.path *  expr
  | Astore of Pident.path *  expr list *  expr
  | For    of Pident.path *  expr * direction *  expr *  expr option *  decl list
  | While  of expr *  decl list
  | Call   of Pident.path *  expr list
  | Return of expr option
[@@deriving show]

type  top_decl =
  | Fundef     of typ * Pident.path * (string * typ) list *  decl list
  | Global_var of typ * Pident.path *  expr option
  | Prim       of typ * Pident.path * string
[@@deriving show]

type t = top_decl list
[@@deriving show]

open Sexplib.Std

type direction =
   | To
   | Downto
[@@deriving sexp]

type typ =
  | Void
  | Int
  | Real
  | Array of typ * expr option
  | Lambda of typ list * typ

and expr =
  | Var    of Pident.path
  | Iconst of int
  | Rconst of string
  | Call   of Pident.path * expr list
  | Aref   of Pident.path * expr list
[@@deriving sexp]

type decl =
  | Decl   of typ * Pident.path * expr option
  | If     of expr * decl list * decl list option
  | Assign of Pident.path * expr
  | Astore of Pident.path * expr list * expr
  | For    of Pident.path * expr * direction * expr * expr option * decl list
  | While  of expr * decl list
  | Call   of Pident.path * expr list
  | Return of expr
[@@deriving sexp]

type top_decl =
  | Fundef of typ * Pident.path * (typ * string) list * decl list
  | Global_var of typ * Pident.path * expr option
  | Prim   of typ * Pident.path * string
[@@deriving sexp]

type t = top_decl list
[@@deriving sexp]

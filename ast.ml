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

and expr =
  | Var    of Pident.path
  | Iconst of int
  | Rconst of string
  | Call   of Pident.path * expr list
  | Aref   of Pident.path * expr list
[@@deriving sexp]

type decl =
  | Decl   of typ * string * expr option
  | If     of expr * decl list * decl list option
  | Assign of Pident.path * expr
  | Astore of Pident.path * expr list * expr
  | For    of string * expr * direction * expr * expr option * decl list
  | While  of expr * decl list
  | Call   of Pident.path * expr list
[@@deriving sexp]

type top_decl =
  | Fundef of typ * string * (typ * string) list * decl list
  | Global_var of typ * string * expr option
[@@deriving sexp]

type t = top_decl list
[@@deriving sexp]

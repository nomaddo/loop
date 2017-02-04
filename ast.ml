open Sexplib.Std

type typ =
  | Void
  | Int
  | Real
  | Array of typ
[@@deriving sexp]

type decl =
  | Decl of typ * string
[@@deriving sexp]

type top_decl =
  | Fundef of typ * string * (typ * string) list * decl list
[@@deriving sexp]

type t = top_decl list
[@@deriving sexp]

type expr =
  | Iconst of int

[@@deriving sexp]

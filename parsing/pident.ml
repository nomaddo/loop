open Sexplib.Std

type ident = string
[@@deriving sexp]

type path =
  | Pident of ident
  | Ppath  of ident * path
[@@deriving sexp]

type ident = string
[@@deriving show]

type path =
  | Pident of ident
[@@deriving show]

let ident = function
  | Pident ident -> ident

type ident = string
[@@deriving show]

type path =
  | Pident of ident
  | Ppath  of ident * path
[@@deriving show]

let ident = function
  | Pident ident -> ident
  | _            -> failwith "ident"

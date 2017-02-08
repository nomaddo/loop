type ident = {name: string; id: int}
[@@deriving show]

type path =
  | Tident of ident
  | Tpath  of ident * path
[@@deriving show]

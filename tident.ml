type ident = {name: string; id: int}

type path =
  | Tident of ident
  | Tpath  of ident * ident

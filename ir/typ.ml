type typ =
  | I4
  | R8
[@@deriving show]

type funtyp =
  { ret_typ: typ option;
    args   : typ list
  }

let sizeof typ =
  match typ with
  | I4 -> 4
  | R8 -> 8

open Typ

type opcore =
  | Iconst of int
  | Rconst of string
  | Var    of Tident.path      (* variables *)
  | Tv     of int              (* temporary variables *)
  | Sp                         (* stack pointer, ilbにのみ登場する *)
  | Fp                         (* frame pointer, ilbにのみ登場する *)
[@@deriving show]

(* operandを表すデータ構造
   同じ変数は同じデータを共有しているためattrを買い換えると書き換わっちゃう

   raではレジスタ情報を書き込む前にコピーしてから書き込む *)
type operand =
  { opcore: opcore; typ: typ; id: int;
    mutable operand_attrs: operand_attr list }

and operand_attr =
  | Tpath of Tident.path
  | Ind
  | Bct
  | Arg of int
  | Reg of Typ.typ * int
[@@deriving show]

let new_operand ?(attrs=[]) opcore typ =
  { opcore; typ; operand_attrs=attrs; id = Etc.cnt () }

let count, reset =
  let r = ref 0 in
  ((fun () -> incr r; !r), (fun () -> r := 0))

let new_tv ?(attrs=[]) ?(opt=None) typ =
    { opcore = Tv (count ()); typ; id = Etc.cnt ();
      operand_attrs = attrs @ match opt with None -> []
                                   | Some tpath -> [Tpath tpath]}

let hash = Hashtbl.create 10

let new_var ?(attrs=[]) tpath typ =
  try
    Hashtbl.find hash tpath
  with Not_found ->
    let op = new_operand ~attrs (Var tpath) typ in
    Hashtbl.add hash tpath op;
    op

let is_constant op = match op.opcore with
  | Iconst _ | Rconst _ -> true
  | _ -> false

let is_zero op =
  match op.opcore with
  | Iconst i -> i = 0
  | _ -> false

let copy op =
  { opcore = op.opcore; id = Etc.cnt ();
    typ = op.typ;
    operand_attrs = List.map (fun x -> x) op.operand_attrs }

let get_reg instr =
  let regs = List.filter
      (function Reg _ -> true | _ -> false) instr.operand_attrs in
  match regs with
  | [Reg (opt, n)] -> (opt, n)
  | _ -> failwith "get_reg"

let is_marked instr =
  let regs = List.filter
      (function Reg _ -> true | _ -> false) instr.operand_attrs in
  match regs with
  | [] -> false
  | [x] -> true
  | _ -> failwith "is_marked"

let dummy typ = new_operand (Tv (-1)) typ

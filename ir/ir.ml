open Batteries

type typ =
  | I4
  | R4
[@@deriving show]

type opcore =
  | Iconst of int
  | Rconst of string
  | Memory of int
  | Var    of Tident.path      (* string is just comment *)
  | Tv     of int              (* temporary variable *)
[@@deriving show]

type operand =
  { opcore: opcore; typ: typ }
[@@deriving show]

type instr =
  | Add   of operand * operand * operand
  | Sub   of operand * operand * operand
  | Mul   of operand * operand * operand
  | Div   of operand * operand * operand
  | Str   of operand * operand
  | Ld    of operand * operand
  | Conv  of operand * operand
  | Mov   of operand * operand
  | Ble   of operand * operand * basic_block
  | Blt   of operand * operand * basic_block
  | Bge   of operand * operand * basic_block
  | Bgt   of operand * operand * basic_block
  | Beq   of operand * operand * basic_block
  | Bnq   of operand * operand * basic_block
  | Mle   of operand * operand * operand * operand
  | Mlt   of operand * operand * operand * operand
  | Mge   of operand * operand * operand * operand
  | Mgt   of operand * operand * operand * operand
  | Meq   of operand * operand * operand * operand
  | Mne   of operand * operand * operand * operand
  | Call  of func * operand list
  | Ret   of operand
  | Alloc of operand * operand

and basic_block = {
  mutable instrs : instr list;
  mutable next   : basic_block option;
  mutable succs  : basic_block list;
  mutable preds  : basic_block list;
}

and func = {
  mutable label_name: string;
  mutable args: operand list;
  mutable entry: basic_block;
}

and toplevel = {
  mutable funcs: func list;
  mutable memories: string list;
}
[@@deriving show]

let true_op = { opcore = Iconst 0; typ = I4 }
let false_op = { opcore = Iconst 1; typ = I4 }

let add op [x; y] = [Add (op, x, y)]
let sub op [x; y] = [Sub (op, x, y)]
let mul op [x; y] = [Mul (op, x, y)]
let div op [x; y] = [Div (op, x, y)]
let mle op [x; y] =
  [Mle (op, x, y, true_op); Mgt (op, x, y, false_op)]
let mlt op [x; y] =
  [Mlt (op, x, y, true_op); Mge (op, x, y, false_op)]
let mge op [x; y] =
  [Mge (op, x, y, true_op); Mlt (op, x, y, false_op)]
let mgt op [x; y] =
  [Mgt (op, x, y, true_op); Mle (op, x, y, false_op)]
let meq op [x; y] =
  [Meq (op, x, y, true_op); Mne (op, x, y, false_op)]
let mne op [x; y] =
  [Mne (op, x, y, true_op); Meq (op, x, y, false_op)]
let conv op [x] = [Conv (op, x)]

let new_bc () =
  { instrs = []; next = None; succs = []; preds = [] }

let tv_count =
  let r = ref 0 in
  fun () -> incr r; !r

let new_tv typ =
    { opcore = Tv (tv_count ()); typ }

let new_var =
  let c = ref 0 in
  fun typ ->
    incr c;
    let tpath = Tident.path ("@" ^ string_of_int !c) in
    { opcore = Var tpath; typ }

let concat_bc p n =
  p.next <- Some n;

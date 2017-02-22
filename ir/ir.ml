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
  { opcore: opcore; typ: typ; mutable attrs: string list }
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
  | Bne   of operand * operand * basic_block
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
  id : int;
  mutable instrs : instr list;
  mutable next   : basic_block option;
  mutable succs  : basic_block list;
  mutable preds  : basic_block list;
}

and func = {
  mutable label_name: string;
  mutable args: operand list;
  mutable entry: basic_block;
  mutable all_bc: basic_block list;
}

and toplevel = {
  mutable funcs: func list;
  mutable memories: string list;
}
[@@deriving show]

let rec dump fmt {funcs; memories} =
  List.iter (dump_func fmt) funcs

and dump_func fmt {label_name; args; entry; all_bc} =
  Format.fprintf fmt "%s: %a@.%a@." label_name dump_args args dump_bcs all_bc

and dump_args fmt l =
  Format.fprintf fmt "{";
  List.iter (Format.fprintf fmt "%a," dump_operand) l;
  Format.fprintf fmt "}"

and dump_operand fmt op =
  match op.opcore with
  | Iconst i -> Format.fprintf fmt "%d" i
  | Rconst s -> Format.fprintf fmt "%s" s
  | Memory i -> Format.fprintf fmt "M_%d" i
  | Var    tpath -> Format.fprintf fmt "$%s(%a)"
        (Tident.show_path tpath) dump_typ op.typ
  | Tv     i -> Format.fprintf fmt "@%d(%a)" i dump_typ op.typ

and dump_typ fmt = function
  | I4 -> Format.fprintf fmt "I4"
  | R4 -> Format.fprintf fmt "R4"

and dump_bcs fmt bcs =
  List.iter (dump_bc fmt) bcs

and dump_bc fmt bc =
  List.iter (dump_instr fmt) bc.instrs

and dump_instr fmt instr =
  let d = dump_operand in
  match instr with
  | Add (x, y, z) -> Format.fprintf fmt "add %a, %a, %a@." d x d y d z
  | Sub (x, y, z) -> Format.fprintf fmt "sub %a, %a, %a@." d x d y d z
  | Mul (x, y, z) -> Format.fprintf fmt "mul %a, %a, %a@." d x d y d z
  | Div (x, y, z) -> Format.fprintf fmt "div %a, %a, %a@." d x d y d z
  | Str (x, y) -> Format.fprintf fmt "store %a, %a@." d x d y
  | Ld (x, y) -> Format.fprintf fmt "load %a, %a@." d x d y
  | Conv (x, y) -> Format.fprintf fmt "conv %a, %a@." d x d y
  | Mov (x, y) -> Format.fprintf fmt "mov %a, %a@." d x d y
  | Ble (x, y, b) -> Format.fprintf fmt "ble %a, %a, {%d}@." d x d y b.id
  | Blt (x, y, b) -> Format.fprintf fmt "blt %a, %a, {%d}@." d x d y b.id
  | Bge (x, y, b) -> Format.fprintf fmt "bge %a, %a, {%d}@." d x d y b.id
  | Bgt (x, y, b) -> Format.fprintf fmt "bgt %a, %a, {%d}@." d x d y b.id
  | Beq (x, y, b) -> Format.fprintf fmt "beq %a, %a, {%d}@." d x d y b.id
  | Bne (x, y, b) -> Format.fprintf fmt "bne %a, %a, {%d}@." d x d y b.id
  | Mle (x, y, z, a) -> Format.fprintf fmt "mle %a, %a, %a, %a@." d x d y d z d z
  | Mlt (x, y, z, a) -> Format.fprintf fmt "mlt %a, %a, %a, %a@." d x d y d z d z
  | Mge (x, y, z, a) -> Format.fprintf fmt "mge %a, %a, %a, %a@." d x d y d z d z
  | Mgt (x, y, z, a) -> Format.fprintf fmt "mgt %a, %a, %a, %a@." d x d y d z d z
  | Meq (x, y, z, a) -> Format.fprintf fmt "meq %a, %a, %a, %a@." d x d y d z d z
  | Mne (x, y, z, a) -> Format.fprintf fmt "mne %a, %a, %a, %a@." d x d y d z d z
  | Call (f, ops) -> Format.fprintf fmt "call %s, %a@." f.label_name (fun fmt l ->
      List.iter (d fmt) l) ops
  | Ret x -> Format.fprintf fmt "ret %a@." d x
  | Alloc (x, y) -> Format.fprintf fmt "alloc %a, %a@." d x d y

let all_bc = ref []
let bc_count = ref 0

let new_operand ?(attrs=[]) opcore typ =
  { opcore; typ; attrs }

let new_bc () =
  let bc = { instrs = []; next = None;
             succs = []; preds = []; id = !bc_count } in
  all_bc := bc :: !all_bc;
  incr bc_count;
  bc

let tv_count =
  let r = ref 0 in
  fun () -> incr r; !r

let new_tv typ =
    { opcore = Tv (tv_count ()); typ; attrs = [] }

let new_var =
  let c = ref 0 in
  fun ?(attrs=[]) typ ->
    incr c;
    let tpath = Tident.path ("@" ^ string_of_int !c) in
    { opcore = Var tpath; typ; attrs; }

let concat_bc p n =
  p.next <- Some n

let true_op = new_operand (Iconst 0) I4
let false_op = new_operand (Iconst 1) I4

let add op [x; y] = [Add (op, x, y)]
let sub op [x; y] = [Sub (op, x, y)]
let mul op [x; y] = [Mul (op, x, y)]
let div op [x; y] = [Div (op, x, y)]
let mle op [x; y] =
  [Mle (op, true_op, x, y); Mgt (op, false_op, x, y)]
let mlt op [x; y] =
  [Mlt (op, true_op, x, y); Mge (op, false_op, x, y)]
let mge op [x; y] =
  [Mge (op, true_op, x, y); Mlt (op, false_op, x, y)]
let mgt op [x; y] =
  [Mgt (op, true_op, x, y); Mle (op, false_op, x, y)]
let meq op [x; y] =
  [Meq (op, true_op, x, y); Mne (op, false_op, x, y)]
let mne op [x; y] =
  [Mne (op, true_op, x, y); Meq (op, false_op, x, y)]
let conv op [x] = [Conv (op, x)]

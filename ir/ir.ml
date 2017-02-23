open Batteries
open Typ
open Operand
open Etc

type basic_block = {
  id: int;
  mutable instrs : instr list;

  (* 次のbasic_block *)
  mutable next   : basic_block option;

  (* ブランチなど次に行きうるbasic_block *)
  mutable succs  : basic_block list;

  (* 前になりうるbasic_block *)
  mutable preds  : basic_block list;

  mutable attrs  : basic_block_attr list;

  mutable loop : loop_info;
}

and basic_block_attr =
  | Line of int

and instr_core =
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
  | Call  of Tident.path * operand list
  | Callm of operand * Tident.path * operand list
  | Ret   of operand
  | Alloc of operand * operand

and instr = {
  mutable instr_core : instr_core;
  mutable belongs    : basic_block;
}

and loop_info = {
  id: int;

  (* 誘導変数のリスト、terminateで必ず更新される *)
  mutable ind_vars   : operand list;

  (* 回避分岐を行うブロック *)
  mutable pre_initial: basic_block option;

  (* 変数の初期化をするブロック *)
  mutable initial    : basic_block option;

  (* 実際の処理を始めるブロック *)
  mutable entrance   : basic_block;

  (* 終了条件を行うブロック、ここからentranceへ分岐が出る *)
  mutable terminate  : basic_block;

  (* ループ終了後必ずここを通る *)
  mutable epilogue   : basic_block;

  (* 親のループ *)
  mutable parent     : loop_info option;

  (* 子のループ *)
  mutable child      : loop_info list;

  mutable attrs      : loop_info_attr list
}

and loop_info_attr =
  | While (* whileの場合pre_initial, initial, ind_varsがない *)
  | For
  | Total

and func = {
  mutable label_name: string;
  mutable args: operand list;
  mutable entry: basic_block;
  mutable all_bc: basic_block list;
  mutable loops: loop_info list;
}

and toplevel = {
  mutable funcs: func list;
  mutable memories: string list;
}
[@@deriving show]

module Dump = struct
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
  | Var    tpath -> Format.fprintf fmt "$%a(%a)"
        dump_tpath tpath dump_typ op.typ
  | Tv     i -> Format.fprintf fmt "@%d(%a)" i dump_typ op.typ

and dump_tpath fmt = function
  | Tident.Tident id -> Format.fprintf fmt "%s(%d)" id.name id.id
  | Tident.Tpath (id, path) ->
      Format.fprintf fmt "%s."id.name;
      dump_tpath fmt path

and dump_typ fmt = function
  | I4 -> Format.fprintf fmt "I4"
  | R4 -> Format.fprintf fmt "R4"

and dump_bcs fmt bcs =
  List.iter (dump_bc fmt) bcs

and dump_bc fmt bc =
  Format.fprintf fmt "--- block (%d)@." bc.id;
  List.iter (dump_instr fmt) bc.instrs;
  Format.fprintf fmt "--- next block (%s)@.@."
    (match bc.next with None -> "none" | Some b -> string_of_int b.id)

and dump_instr fmt instr =
  let d = dump_operand in
  match instr.instr_core with
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
  | Callm (x, tpath, ops) -> Format.fprintf fmt "callm %a %a, %a@." d x
        dump_tpath tpath  (fun fmt l -> List.iter (d fmt) l) ops
  | Call (tpath, ops) -> Format.fprintf fmt "call %a, %a@." dump_tpath tpath
        (fun fmt l -> List.iter (d fmt) l) ops
  | Ret x -> Format.fprintf fmt "ret %a@." d x
  | Alloc (x, y) -> Format.fprintf fmt "alloc %a, %a@." d x d y
end

let true_op = new_operand (Iconst 0) I4
let false_op = new_operand (Iconst 1) I4

module Instr = struct
  type t = instr

  let new_instr core bc =
    { instr_core = core; belongs = bc }

  let copy_instr bc {instr_core; belongs} =
     { instr_core; belongs = bc }
end

module Bc = struct
  let all_bc = ref []

  let new_bc ?(attrs=[]) loop =
    let bc = { instrs = []; next = None; succs = []; loop;
               preds = []; attrs; id = Etc.cnt (); } in
    all_bc := bc :: !all_bc;
    bc

  let concat_bc p n =
    p.next <- Some n

end

module Loop_info = struct

  let dummy_bc = Obj.magic 1
  let cnt = ref 0

  let total_loop () = {
      id = -100;
      ind_vars = [];
      pre_initial = None;
      initial = None;
      entrance = dummy_bc;
      terminate = dummy_bc;
      epilogue = dummy_bc;
      parent = None;
      child = [];
      attrs = [Total]
  }

  let make_loop ~pre ~init parent =
    let loop_info =
      { pre_initial = Some dummy_bc; initial = Some dummy_bc;
        entrance = dummy_bc; terminate = dummy_bc; epilogue = dummy_bc; ind_vars = [];
        parent = Some parent; child = []; attrs = []; id = Etc.cnt () } in
    let pre_initial = Bc.new_bc loop_info in
    let initial     = Bc.new_bc loop_info in
    let entrance    = Bc.new_bc loop_info in
    let terminate   = Bc.new_bc loop_info in
    let epilogue    = Bc.new_bc loop_info in
    loop_info.pre_initial <- if pre then Some pre_initial else None;
    loop_info.initial <- if init then Some initial else None;
    loop_info.entrance <- entrance;
    loop_info.terminate <- terminate;
    loop_info.epilogue <- epilogue;
    loop_info
end

open Instr

let add bc op [x; y] = [new_instr ++ Add (op, x, y) ++ bc]
let sub bc op [x; y] = [new_instr ++ Sub (op, x, y) ++ bc]
let mul bc op [x; y] = [new_instr ++ Mul (op, x, y) ++ bc]
let div bc op [x; y] = [new_instr ++ Div (op, x, y) ++ bc]
let mle bc op [x; y] = [new_instr ++ Mle (op, true_op, x, y) ++ bc;
                        new_instr ++ Mgt (op, false_op, x, y) ++ bc]
let mlt bc op [x; y] = [new_instr ++ Mlt (op, true_op, x, y) ++ bc;
                        new_instr ++ Mge (op, false_op, x, y) ++ bc]
let mge bc op [x; y] = [new_instr ++ Mge (op, true_op, x, y) ++ bc;
                        new_instr ++ Mlt (op, false_op, x, y) ++ bc]
let mgt bc op [x; y] = [new_instr ++ Mgt (op, true_op, x, y) ++ bc;
                        new_instr ++ Mle (op, false_op, x, y) ++ bc]
let meq bc op [x; y] = [new_instr ++ Meq (op, true_op, x, y) ++ bc;
                        new_instr ++ Mne (op, false_op, x, y) ++ bc]
let mne bc op [x; y] = [new_instr ++ Mne (op, true_op, x, y) ++ bc;
                        new_instr ++ Meq (op, false_op, x, y) ++ bc]
let conv bc op [x] = [new_instr ++ Conv (op, x) ++ bc]

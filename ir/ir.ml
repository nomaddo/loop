open Batteries
open Typ
open Operand
open Etc

type 'a basic_block = {
  id: int;
  mutable instrs : 'a instr list;

  (* 次のbasic_block *)
  mutable next   : 'a basic_block option;

  (* ブランチなど次に行きうるbasic_block *)
  mutable succs  : 'a basic_block list;

  (* 前になりうるbasic_block *)
  mutable preds  : 'a basic_block list;

  mutable attrs  : basic_block_attr list;

  mutable loop : 'a loop_info;
}

and basic_block_attr =
  | Line of int

and index_mode =
  | Base_offset of { base: operand; offset: operand }

and ila =
  | Add   of operand * operand * operand
  | Sub   of operand * operand * operand
  | Mul   of operand * operand * operand
  | Div   of operand * operand * operand
  | Str   of index_mode * operand
  | Ld    of operand * index_mode
  | Conv  of operand * operand
  | Mov   of operand * operand
  | Ble   of operand * operand * ila basic_block
  | Blt   of operand * operand * ila basic_block
  | Bge   of operand * operand * ila basic_block
  | Bgt   of operand * operand * ila basic_block
  | Beq   of operand * operand * ila basic_block
  | Bne   of operand * operand * ila basic_block
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

and 'a instr = {
  mutable instr_core : 'a;
  mutable belongs    : 'a basic_block;
}

and 'a loop_info = {
  id: int;

  (* 誘導変数のリスト、terminateで必ず更新される *)
  mutable ind_vars   : operand list;

  (* 回避分岐を行うブロック *)
  mutable pre_initial: 'a basic_block option;

  (* 変数の初期化をするブロック *)
  mutable initial    : 'a basic_block option;

  (* 実際の処理を始めるブロック *)
  mutable entrance   : 'a basic_block;

  (* 終了条件を行うブロック、ここからentranceへ分岐が出る *)
  mutable terminate  : 'a basic_block;

  (* ループ終了後必ずここを通る *)
  mutable epilogue   : 'a basic_block;

  (* 親のループ *)
  mutable parent     : 'a loop_info option;

  (* 子のループ *)
  mutable child      : 'a loop_info list;

  mutable attrs      : loop_info_attr list
}

and loop_info_attr =
  | While (* whileの場合pre_initial, initial, ind_varsがない *)
  | For
  | Total

and 'a func = {
  mutable label_name: string;
  mutable args: operand list;
  mutable entry: 'a basic_block;
  mutable all_bc: 'a basic_block list;
  mutable loops: 'a loop_info list;
}

and 'a toplevel = {
  mutable funcs: 'a func list;
  mutable memories: memory list;
}

and memory = {
  name  : Tident.path;
  shape : int
}
[@@deriving show]

let true_op = new_operand (Iconst 0) I4
let false_op = new_operand (Iconst 1) I4

module Instr = struct
  type t = ila instr

  let new_instr core bc =
    { instr_core = core; belongs = bc }

  let copy_instr bc {instr_core; belongs} =
     { instr_core; belongs = bc }
end

module Bc = struct
  let all_bc : ila basic_block list ref = ref []
  let clear_bc () = all_bc := []

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

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

  (* 最適化でノードに印をつけるために使う
     使いたい人が自分でcleanして使う
  *)
  mutable traverse_attr: int
}

and basic_block_attr =
  | Line of int

and index_mode =
  | Base_offset of { base: operand; offset: operand }
  | Operand     of operand

and ila =
  | Add    of operand * operand * operand
  | Sub    of operand * operand * operand
  | Mul    of operand * operand * operand
  | Div    of operand * operand * operand
  | Str    of index_mode * operand
  | Ld     of operand * index_mode
  | Conv   of operand * operand
  | Mov    of operand * operand
  | Branch of br_kind * operand * operand * ila basic_block
  | Bmov   of br_kind * operand * operand * operand * operand
  | Call   of Tident.path * operand list
  | Callm  of operand * Tident.path * operand list
  | Ret    of operand
  | Alloc  of operand * operand
  | Dealloc of operand * operand

and br_kind =
  | Le | Lt | Ge | Gt | Eq | Ne

and 'a instr = {
  mutable instr_core : 'a;
  mutable instr_attrs : instr_attr list
}

and instr_attr =
  | Vars of Operand.operand Set.Map.t

and 'a loop_info = {
  id: int;

  (* 誘導変数のリスト、terminateで必ず更新される *)
  mutable ind_vars   : operand list;

  (* 回避分岐を行うブロック *)
  mutable pre_initial: 'a basic_block option;

  (* 変数の初期化をするブロック *)
  mutable initial    : 'a basic_block option;

  (* 実際の処理を始めるブロック *)
  mutable entrance   : 'a basic_block option;

  (* 終了条件を行うブロック、ここからentrance,epilogueへ分岐が出る *)
  mutable terminate  : 'a basic_block option;

  (* ループ終了後必ずここを通る *)
  mutable epilogue   : 'a basic_block option;

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
  mutable loops: 'a loop_info list;
  (* mutable dealloc: 'a instr list *)
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

let true_op = new_operand (Iconst 1) I4
let false_op = new_operand (Iconst 0) I4

module Instr = struct
  type t = ila instr

  let new_instr core =
    { instr_core = core; instr_attrs = [] }

  let copy_instr bc {instr_core; } =
    { instr_core; instr_attrs = [] }

  let delete bc instr =
    let instrs =
      List.fold_left (fun acc x ->
        if x == instr then acc else x :: acc) [] bc.instrs
      |> List.rev in
    bc.instrs <- instrs

  let replace bc old new_ =
    let new_instrs =
      List.fold_left (fun acc x ->
        if x == old then new_ :: acc else x :: acc) [] bc.instrs
      |> List.rev in
    bc.instrs <- new_instrs

  let replace_instrs bc old news =
    let rec f = function
      | x :: xs ->
          if x = old then news @ xs else x :: f xs
      | [] -> news in
    let new_instrs = f bc.instrs in
    bc.instrs <- new_instrs

  let append_last bc instr =
    bc.instrs <- bc.instrs @ [instr]

  let append_first bc instr =
    bc.instrs <- instr :: bc.instrs

  let new_branch k x y bc = new_instr ++ Branch (k, x, y, bc)
  let new_bmov k op1 op2 op3 op4 =
    new_instr ++ Bmov (k, op1, op2, op3, op4)

  let is_branch ila = match ila.instr_core with
    | Branch _ -> true
    | _ -> false

  let get_distination ila = match ila.instr_core with
    | Branch (_, _, _, bc) -> bc
    | _ -> raise Not_found

  let find_branch_instr bc =
    try Some (List.find is_branch bc.instrs) with
    | Not_found -> None

  let is_ret instr =
    match instr.instr_core with
    | Ret _ -> true | _ -> false

  let include_ret bc =
    try ignore (List.find is_ret bc.instrs); true
    with Not_found -> false
end

module Bc = struct
  let new_bc ?(attrs=[]) loop =
    let bc = { instrs = []; next = None; succs = []; loop = loop;
               preds = []; attrs; id = Etc.cnt (); traverse_attr = 0} in
    bc

  let concat_bc p n =
    p.next <- Some n

end

module Loop_info = struct
  let empty_loop id =
    {
      id = id;
      ind_vars = [];
      pre_initial = None;
      initial = None;
      entrance = None;
      terminate = None;
      epilogue = None;
      parent = None;
      child = [];
      attrs = [Total]
    }

  let dummy_loop = Obj.magic (empty_loop (-1))
  let is_dummy_loop (loop: 'a loop_info) = loop.id = -1

  let total_loop =
    let r = ref 0 in
    (fun () ->
      incr r;
      Obj.magic (empty_loop (-100 - !r)))


  let new_loop () = empty_loop ++ Etc.cnt ()
end

open Instr

let add op [x; y] = [new_instr ++ Add (op, x, y) ]
let sub op [x; y] = [new_instr ++ Sub (op, x, y) ]
let mul op [x; y] = [new_instr ++ Mul (op, x, y) ]
let div op [x; y] = [new_instr ++ Div (op, x, y) ]
let mle op [x; y] = [new_bmov Le op true_op  x  y ;
                     new_bmov Gt op false_op x  y ]
let mlt op [x; y] = [new_bmov Lt op true_op  x  y ;
                     new_bmov Ge op false_op x  y ]
let mge op [x; y] = [new_bmov Ge op true_op  x  y ;
                     new_bmov Lt op false_op x  y ]
let mgt op [x; y] = [new_bmov Gt op true_op  x  y ;
                     new_bmov Le op false_op x  y ]
let meq op [x; y] = [new_bmov Eq op true_op  x  y ;
                     new_bmov Ne op false_op x  y ]
let mne op [x; y] = [new_bmov Ne op true_op  x  y ;
                     new_bmov Eq op false_op x  y ]
let conv op [x] = [new_instr ++ Conv (op, x)]

(* machine dependent *)
let int_size = 4
let double_size = 8
let addr_size = 4

let addr_size_op = Operand.new_operand (Iconst addr_size) I4

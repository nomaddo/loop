open Batteries
open Operand

type ilb =
  | Add    of operand * operand * operand
  | Sub    of operand * operand * operand
  | Mul    of operand * operand * operand
  | Div    of operand * operand * operand
  | Str    of Ir.index_mode * operand
  | Ldr    of operand * Ir.index_mode (* 仮想命令 movにも *)
  | Mov    of operand * operand
  | Cmp    of operand * operand
  | Branch of Ir.br_kind * ilb Ir.basic_block
  | Bmov   of Ir.br_kind * operand * operand
  | Bl     of Tident.path
  | B      of Tident.path

  (* 疑似命令 *)
  | Ret    of operand option
  | Conv   of operand * operand (* 命令と一対一対応なので最後までこの形 *)
  | Call   of operand option * Tident.path * operand list
  | Alloc  of operand * operand
  | Dealloc  of operand * operand

let is_branch instr = match instr.Ir.instr_core with
  | Branch (k, b) -> true
  | _ -> false

let find_branch_instr bc =
  try Some (List.find is_branch bc.Ir.instrs) with
  | Not_found -> None

let get_distination instr = match instr.Ir.instr_core with
  | Branch (k, b) -> b
  | _ -> assert false

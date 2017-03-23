(* register allocation
   tvの属性にレジスタ割付した結果を書き込む
   call, callmをb, blに変える
*)

open Batteries
open Etc
open Ir
open Ilb

let copy = Operand.copy
let is_marked = Operand.is_marked
let is_constant = Operand.is_constant

let r_num = 12

type regs =
  { r_reg: (Operand.operand, int) Map.t;
    d_reg: (Operand.operand, int) Map.t;
  }

let create_regs r_reg d_reg =
  {r_reg; d_reg}

let update_r regs r_reg =
  {r_reg; d_reg=regs.d_reg}

let update_d regs d_reg =
  {r_reg = regs.r_reg; d_reg}

let transl_bc regs bc = regs
  (* List.iter (fun instr -> match instr.instr_core with *)
  (* ) bc.instrs *)

let transl_func regs func =
  Ir_util.fold 1100 transl_bc (Map.empty, Map.empty) func.Ir.entry

let transl top =
  List.iter (fun func ->
    transl_func top.Ir.memories func |> ignore) top.Ir.funcs

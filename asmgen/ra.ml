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

let transl_func memories func =
  Ilb_simplify.set_var_attr Set.empty func.entry

let transl top =
  List.iter (fun func ->
    transl_func top.Ir.memories func |> ignore) top.Ir.funcs

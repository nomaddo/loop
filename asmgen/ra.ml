(* register allocation
   tvの属性にレジスタ割付した結果を書き込む
   call, callmをb, blに変える
*)

open Batteries
open Etc
open Ir
open Ilb

(* 定数のmovの除去

*)

open Batteries
open Etc
open Operand
open Ir

let constant_folding x y =
  match x.opcore, y.opcore with
  | Iconst i, Iconst j ->
      assert (x.typ == y.typ);
      new_operand ++ Iconst (i + j) ++ x.typ
  | Rconst s, Rconst t ->
      assert (x.typ == y.typ);
      new_operand ++ Rconst (float_of_string s +. float_of_string t
                             |> string_of_float) ++ x.typ
  | _ -> assert false

let try_replace hash op =
  try Hashtbl.find hash op with Not_found -> op

let compare_op k op1 op2 =
  let cmpi k i =
    match k with
    | Le -> i <= 0
    | Lt -> i < 0
    | Ge -> i >= 0
    | Gt -> i > 0
    | Eq -> i = 0
    | Ne -> i != 0 in
  let cmpr k s =
    let f = float_of_string s in
    match k with
    | Le -> f <= 0.
    | Lt -> f < 0.
    | Ge -> f >= 0.
    | Gt -> f > 0.
    | Eq -> f = 0.
    | Ne -> f != 0. in
  let op = constant_folding op1 op2 in
  match op.opcore with
  | Iconst i -> cmpi k i
  | Rconst s -> cmpr k s
  | _ -> failwith "compare_op"

let shrink k op1 op2 bc dist =
  if compare_op k op1 op2 then
    bc.next <- Some dist

let simplify_bc bc =
  List.fold_left (fun (hash, instrs) instr ->
    match instr.instr_core with
    | Add (op1, op2, op3)
    | Sub (op1, op2, op3)
    | Mul (op1, op2, op3)
    | Div (op1, op2, op3) ->
        Hashtbl.remove hash op1;
        let op2_ = try_replace hash op2 in
        let op3_ = try_replace hash op3 in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ in
          (hash, Instr.new_instr ++ Mov (op1, op2) ++ instr.belongs :: instrs)
        else (hash, instr :: instrs)
    | Mov (op1, op2) ->
        if Operand.is_constant op2 then begin
          Hashtbl.add hash op1 op2;
          (hash, instrs) end
        else begin
          Hashtbl.remove hash op1;
          Hashtbl.add hash op1 op2;
          (hash, instr :: instrs)
        end
    | Str   (index_mode, op) ->
        let instr =
          Instr.new_instr
          ++ Str (index_mode, try_replace hash op)
          ++ instr.belongs in
        (hash, instr :: instrs)
    | Ld _ -> (hash, instr :: instrs)
    | Conv  (op1 , op2) ->
        let instr =
          Instr.new_instr
          ++ Conv (op1, try_replace hash op2)
          ++ instr.belongs in
        (hash, instr :: instrs)
    | Branch   (k, op1, op2, dist) ->
        if is_constant op1 && is_constant op2 then begin
          shrink k op1 op2 bc dist;
          (hash, instrs)
        end else
        let instr =
          Instr.new_branch k
          ++ try_replace hash op1
          ++ try_replace hash op2
          ++ bc ++ instr.belongs in
        (hash, instr :: instrs)
    | Bmov (k, op1 , op2, op3 , op4) ->
        Hashtbl.remove hash op1;
        let instr =
          Instr.new_bmov k
          ++ try_replace hash op1
          ++ try_replace hash op2
          ++ try_replace hash op3
          ++ try_replace hash op4 ++ bc in
        (hash, instr :: instrs)
    | Call  (tpath , ops) ->
        let instr =
          Instr.new_instr ++ Call (tpath, List.map (try_replace hash) ops) ++ bc in
        (hash, instr :: instrs)
    | Callm (op , tpath , ops) ->
        Hashtbl.remove hash op;
        let instr =
          Instr.new_instr ++ Callm (op, tpath, List.map (try_replace hash) ops) ++ bc in
        (hash, instr :: instrs)
    | Ret op ->
        let instr =
          Instr.new_instr ++ Ret (try_replace hash op) ++ bc in
        (hash, instr :: instrs)
    | Alloc (op1, op2) ->
        let instr =
          Instr.new_instr ++ Alloc (op1, try_replace hash op2) ++ bc in
        (hash, instr :: instrs)) (Hashtbl.create 100, []) bc.instrs
  |> snd |> List.rev

let func {Ir.entry} =
  Ir_util.iter 1 (fun bc -> bc.instrs <- simplify_bc bc) entry

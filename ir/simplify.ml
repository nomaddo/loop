(* 定数のmovの除去

*)

open Batteries
open Etc
open Operand
open Ir

let new_instr = Instr.new_instr

let flag = ref false

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

let try_replace map op =
  try let op_ = Map.find op map in
    flag_on flag; op_
  with Not_found -> op

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

let simplify_index map index_mode =
  match index_mode with
  | Base_offset { base;  offset; } ->
      Base_offset  { base;  offset = try_replace map offset; }

let simplify_bc map bc =
  List.fold_left (fun (map, instrs) instr ->
    match instr.instr_core with
    | Add (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Add (op1, op2_, op3_) in
          (map, instr :: instrs)

    | Sub (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Sub (op1, op2_, op3_) in
          (map, instr :: instrs)

    | Mul (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ in
          (map, new_instr ++ Mov (op1, op2)  :: instrs)
        else
          let instr = new_instr ++ Mul (op1, op2_, op3_) in
          (map, instr :: instrs)
    | Div (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Div (op1, op2_, op3_) in
          (map, instr :: instrs)
    | Mov (op1, op2) ->
        if Operand.is_constant op2 then begin
        let map = Map.add op1 op2 map in
          (map, instrs) end
        else begin
          let map = Map.add op1 op2 map in
          (map, instr :: instrs)
        end
    | Str   (index_mode, op) ->
        let instr =
          new_instr
          ++ Str (simplify_index map index_mode, try_replace map op) in
        (map, instr :: instrs)
    | Ld (op, index_mode) ->
        let instr =
          new_instr
          ++ Ld (op, simplify_index map index_mode) in
        let map = Map.remove op map in
        (map, instr :: instrs)
    | Conv  (op1 , op2) ->
        let instr =
          new_instr
          ++ Conv (op1, try_replace map op2) in
        let map = Map.remove op1 map in
        (map, instr :: instrs)
    | Branch   (k, op1, op2, dist) ->
        if is_constant op1 && is_constant op2 then begin
          shrink k op1 op2 bc dist;
          (map, instrs)
        end else
        let instr =
          Instr.new_branch k
          ++ try_replace map op1
          ++ try_replace map op2
          ++ dist in
        (map, instr :: instrs)
    | Bmov (k, op1 , op2, op3 , op4) ->
        let map = Map.remove op1 map in
        let instr =
          Instr.new_bmov k
          ++ try_replace map op1
          ++ try_replace map op2
          ++ try_replace map op3
          ++ try_replace map op4 in
        (map, instr :: instrs)
    | Call  (tpath , ops) ->
        let instr =
          new_instr ++ Call (tpath, List.map (try_replace map) ops) in
        (map, instr :: instrs)
    | Callm (op , tpath , ops) ->
        let map = Map.remove op map in
        let instr =
          new_instr ++ Callm (op, tpath, List.map (try_replace map) ops) in
        (map, instr :: instrs)
    | Ret op ->
        let instr =
          new_instr ++ Ret (try_replace map op) in
        (map, instr :: instrs)
    | Alloc (op1, op2) ->
        let instr =
          new_instr ++ Alloc (op1, try_replace map op2) in
        (map, instr :: instrs)) (map, []) bc.instrs
  |> (fun (map, instrs) -> map, List.rev instrs)

let dump_map map =
  Format.printf "dump_map: begin@.";
  Map.foldi (fun k v () -> Format.printf "%a -> %a@." Dump.dump_operand k Dump.dump_operand v) map ();
  Format.printf "dump_map: end@."

let func {Ir.entry} =
  flag_off flag;
  let map = Map.empty in
  let map = Ir_util.fold (entry.traverse_attr + 1) (fun map bc ->
      let map, instrs = simplify_bc map bc in
      bc.instrs <- instrs; map) map entry in
  ignore map;
  (* dump_map map; *)
  !flag

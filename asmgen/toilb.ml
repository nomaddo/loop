open Ir

module BcMap = Hashtbl.Make (struct
    type t = Ir.ila Ir.basic_block
    let equal (s: t) (t: t) = s.id == t.id
    let hash (t:t) = t.id
end)

let access_with_int hash bc = BcMap.find hash bc
let insert_with_int hash key value = BcMap.add hash key value

let transl_instr hash memories instr =
  let cores = match instr.instr_core with
  | Add    (op1, op2, op3)         -> [Ilb.Add (op1, op2, op3)]
  | Sub    (op1, op2, op3)         -> [Ilb.Sub (op1, op2, op3)]
  | Mul    (op1, op2, op3)         -> [Ilb.Mul (op1, op2, op3)]
  | Div    (op1, op2, op3)         -> [Ilb.Div (op1, op2, op3)]
  | Str    (index_mode, op)        -> [Ilb.Str (index_mode, op)]
  | Ld     (op, index_mode)        -> [Ilb.Ldr (op, index_mode)]
  | Conv   (op1, op2)              -> [Ilb.Conv (op1, op2)]
  | Mov    (op1, op2)              -> [Ilb.Ldr (op1, Ir.Operand op2)]
  | Branch (k, op1, op2, bc)       -> [Ilb.Cmp (op1, op2);
                                       Ilb.Branch (k, access_with_int hash bc)]
  | Bmov   (k, op1, op2, op3, op4) -> [Ilb.Cmp (op1, op2); Ilb.Bmov (k, op3, op4)]
  | Call   (tpath , ops)           -> [Ilb.Call (tpath , ops)]
  | Callm  (op, tpath, ops)        -> [Ilb.Callm  (op, tpath, ops)]
  | Ret    op                      -> [Ilb.Ret op]
  | Alloc  (op1, op2)              -> [Ilb.Alloc  (op1, op2)] in
  List.map Instr.new_instr cores

let rec transl_bc hash memories bc =
  let instrs = List.map (transl_instr hash memories) bc.instrs |> List.flatten in
  let bbc = access_with_int hash bc in
  bbc.instrs <- instrs

let transl_func memories {Ir.label_name; args; entry} =
  let hash = BcMap.create 100 in
  Ir_util.reset_traverse_attr entry;
  Ir_util.iter 100 (fun bc -> insert_with_int hash bc
      (Bc.new_bc Loop_info.dummy_loop)) entry;
  let new_bc = BcMap.find hash entry in
  Ir_util.iter 200 (transl_bc hash memories) entry;
  {Ir.label_name; args; entry = new_bc; loops = []}

let transl {Ir.funcs; memories} =
  {memories; funcs = List.map (transl_func memories) funcs}

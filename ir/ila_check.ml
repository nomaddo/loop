open Ir
open Operand

let rec check {funcs; memories} =
  List.iter check_func funcs

and check_func {label_name; args; entry; loops} =
  Ir_util.iter 10 check_bc entry

and check_bc {instrs} = check_instrs instrs

and check_instrs = function
  | [] -> ()
  | [x] -> check_instr ~last:true x
  | x::xs ->
      check_instr ~last:false x;
      check_instrs xs

and check_instr ~last instr = match instr.instr_core with
  | Ir.Add   (op1, op2, op3)
  | Sub   (op1, op2 , op3)
  | Mul   (op1, op2, op3)
  | Div   (op1, op2, op3) ->
      if not (op1.typ = op2.typ)
      then begin
        Format.printf "type mismatch@. %a, and %a@."
          Dump.dump_operand op1 Dump.dump_operand op2;
        failwith "check_instr: type mismatch binop1" end
      else if not (op2.typ = op3.typ) then begin
        Format.printf "type mismatch@. %a, and %a@."
          Dump.dump_operand op2 Dump.dump_operand op3;
        failwith "check_instr: type mismatch binop2" end
  | Str   (index_mode, op)
  | Ld    (op , index_mode) ->
      begin match index_mode with
      | Base_offset {base; offset} ->
          if base.typ != Ir.addr_size_op.typ then begin
            Format.printf "check_instr: %a is expected %a"
              Dump.dump_operand base Dump.dump_typ addr_size_op.typ;
            failwith "check_instr: type mismatch  memory" end
          else if offset.typ != addr_size_op.typ then begin
            Format.printf "check_instr: %a is expected %a"
              Dump.dump_operand base Dump.dump_typ addr_size_op.typ;
            failwith "check_instr: type mismatch memory" end
      end
  | Conv  (op1 , op2) -> ()
  | Mov   (op1 , op2) ->
      (* Operand.VarはMovできない、メモリを表している *)
      let checkop ~dist op = match op.opcore with
        | Var _ ->
            Format.printf "check_instr: var is used in mov %a@." Dump.dump_operand op;
            failwith "check_instr: mov"
        | Operand.Iconst _ | Rconst _ ->
            if dist then failwith "check_instr: try to move to const"
        | _ -> () in
      checkop ~dist:true op1; checkop ~dist:false op2;
      if (op1.typ != op2.typ) then begin
        Format.printf "check_instr: mismatch@. %a, and %a@."
          Dump.dump_operand op1 Dump.dump_operand op2;
        failwith "check_instr: type mismatch mv" end
  | Branch (k, op1, op2, bc) ->
    if not last then failwith "check_instr: branch is not located the end";
    if (op1.typ != op2.typ) then begin
      Format.printf "check_instr: mismatch@. %a, and %a@."
        Dump.dump_operand op1 Dump.dump_operand op2;
      failwith "check_instr: type mismatch branch" end
  | Bmov  (k, op1 , op2 , op3 , op4)  ->
      if (op1.typ != op2.typ) then begin
        Format.printf "check_instr: mismatch@. %a, and %a@."
          Dump.dump_operand op1 Dump.dump_operand op2;
        failwith "check_instr: type mismatch conditional mv1" end;
      if (op3.typ != op4.typ) then begin
        Format.printf "check_instr: mismatch@. %a, and %a@."
          Dump.dump_operand op3 Dump.dump_operand op4;
        failwith "check_instr: type mismatch conditional mv2" end
  | Call  (tpath , ops) -> ()
  | Callm (op, tpath, ops) -> ()
  | Ret   op -> ()
  | Alloc (op1, op2) ->
      if op2.typ != addr_size_op.typ then begin
        Format.printf "check_instr: mismatch@. %a, and %a@."
          Dump.dump_operand op1 Dump.dump_operand op2;
      failwith "check_instr: type mismatch conditional alloc" end;

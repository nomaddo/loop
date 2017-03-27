open Batteries
open Operand
open Ir
open Ilb

let rec set_control_flow bc =
  let open Ir in
  let rec set_succs bc =
    if bc.traverse_attr = 1 then () else begin
      bc.traverse_attr <- 1;
      let bopt = find_branch_instr bc |> Option.map get_distination in
      bc.succs <- Option.map_default (fun x -> [x]) [] bopt @
          Option.map_default (fun x -> [x]) [] bc.next ;
      List.iter set_succs bc.succs end in

  let set_prevs bc =
    let hash = Hashtbl.create 100 in
    let rec gather_info bc =
      if bc.traverse_attr != 0 then begin
        bc.traverse_attr <- 0;
        List.iter (fun x -> Hashtbl.add hash x bc) bc.succs;
        List.iter gather_info bc.succs
      end in
    gather_info bc;
    Hashtbl.iter (fun succ prev -> succ.preds <- prev :: succ.preds) hash in

  set_succs bc;
  set_prevs bc

let for_all_operand_index_mode f = function
  | Ir.Base_offset {base; offset} -> Ir.Base_offset {base = f base; offset=f offset}

let for_all_operand_ilb f instr =
  let core =
    match instr.Ir.instr_core with
    | Add     (op1, op2, op3) -> Add (f op1, f op2, f op3)
    | Sub     (op1, op2, op3) -> Sub (f op1, f op2, f op3)
    | Mul     (op1, op2, op3) -> Mul (f op1, f op2, f op3)
    | Div     (op1, op2, op3) -> Div (f op1, f op2, f op3)
    | Str     (index_mode, op) ->
        Str     (for_all_operand_index_mode f index_mode, f op)
    | Ldr     (op , index_mode) ->
        Ldr     (f op, for_all_operand_index_mode f index_mode)
    | Mov     (op1 , op2) -> Mov (f op1, f op2)
    | Cmp     (op1 , op2) -> Cmp (f op1, f op2)
    | Branch  (br_kind, bc) ->  instr.Ir.instr_core
    | Bmov    (br_kind, op1, op2) -> Bmov    (br_kind, f op1, f op2)
    | Bl      tpath
    | B       tpath ->  instr.Ir.instr_core
    | Ret     opt -> Ret (Option.map f opt)
    | Conv    (op1, op2) -> Conv    (f op1, f op2)
    | Call    (opt, tpath , ops) -> Call    (Option.map f opt, tpath , List.map f ops)
    | Alloc   (op1 , op2) -> Alloc   (f op1 , f op2)
    | Dealloc (op1 , op2) -> Dealloc (f op1 , f op2) in
  {Ir.instr_core = core; instr_attrs = instr.Ir.instr_attrs}

let for_all_operand bc f =
  Ir_util.iter 101 (fun bc ->
    bc.instrs <- List.map (for_all_operand_ilb f) bc.instrs) bc

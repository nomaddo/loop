open Batteries
open Typ
open Operand
open Ir

let rec dump fmt {funcs; memories} =
  Format.fprintf fmt "memory@.";
  List.iter (dump_memory fmt) memories;
  Format.fprintf fmt "@.";
  List.iter (dump_func fmt) funcs

and dump_memory fmt {name; shape} =
  Format.fprintf fmt "M %a: %d\n" dump_tpath name shape

and dump_func fmt {label_name; args; entry; } =
  Format.fprintf fmt "%s: %a start block_%d@."
    label_name dump_args args entry.id;
  (* Format.fprintf fmt "dealloc: %a@." (fun fmt -> List.iter (dump_ila fmt)) dealloc; *)
  Format.fprintf fmt "%a@." dump_bcs entry

and dump_args fmt l =
  Format.fprintf fmt "{";
  List.iter (Format.fprintf fmt "%a," dump_operand) l;
  Format.fprintf fmt "}"

and dump_operand fmt op =
  match op.opcore with
  | Iconst i -> Format.fprintf fmt "%d" i
  | Rconst s -> Format.fprintf fmt "%s" s
  | Var    tpath -> Format.fprintf fmt "$%a(%a)"
        dump_tpath tpath dump_typ op.typ
  | Tv     i ->
      begin try
        let Tpath tpath = List.find (function Tpath _ -> true
                                            | _ -> false)op.operand_attrs in
        Format.fprintf fmt "@%d(%a, %a)" i dump_typ op.typ dump_tpath tpath
      with Not_found ->
        Format.fprintf fmt "@%d(%a)" i dump_typ op.typ
      end
  | Sp       -> Format.fprintf fmt "sp"
  | Fp       -> Format.fprintf fmt "fp"

and dump_tpath fmt = function
  | Tident.Tident id -> Format.fprintf fmt "%s(%d)" id.name id.id

and dump_typ fmt = function
  | I4 -> Format.fprintf fmt "I4"
  | R8 -> Format.fprintf fmt "R8"

and dump_bcs fmt bc =
  Ir_util.reset_traverse_attr bc;
  Ir_util.iter 10 (dump_bc fmt) bc

and dump_bc fmt bc =
  Format.fprintf fmt "--- block_%d %a@." bc.id dump_bc_loop_info bc;

  Etc.dmsg Flags.show_dyn_arrays (fun () ->
    List.iter (fun (op1, op2) ->
      if bc.dyn_arrays = [] then Format.printf "dyn_array: none@." else
      Format.printf "dyn_array: %a: %a@." dump_operand op1 dump_operand op2) bc.dyn_arrays);

  Etc.dmsg Flags.show_stack (fun () ->
    List.iter (fun (op1, i) ->
      if bc.stack_layout = [] then Format.printf "stack: none@." else
      Format.printf "stack: %a: %d@." dump_operand op1 i) bc.stack_layout);

  Format.fprintf fmt "prevs: %a@." (fun fmt ->
    List.iter (fun (bc:'a Ir.basic_block) -> Format.fprintf fmt "%d " bc.id)) bc.preds;
  Format.fprintf fmt "succs: %a@." (fun fmt ->
    List.iter (fun (bc:'a Ir.basic_block) -> Format.fprintf fmt "%d " bc.id)) bc.succs;
  List.iter (dump_ila fmt) bc.instrs;
  Format.fprintf fmt "--- next block_%s@.@."
    (match bc.next with None -> "none" | Some b -> string_of_int b.id)

and dump_bc_loop_info fmt bc =
  let s = ref "" in
  let info = bc.loop in
  Option.may (fun bc_ -> if bc == bc_ then s := !s ^ "/pre") info.pre_initial;
  Option.may (fun bc_ -> if bc == bc_ then s := !s ^ "/init") info.initial;
  Option.may (fun bc_ -> if bc == bc_ then s := !s ^ "/entr") info.entrance;
  Option.may (fun bc_ -> if bc == bc_ then s := !s ^ "/term") info.terminate;
  Option.may (fun bc_ -> if bc == bc_ then s := !s ^ "/epi")  info.epilogue;
  Format.fprintf fmt "%s: loop(%d)" !s info.id

and dump_index_mode fmt = function
  | Base_offset {base; offset} ->
      Format.fprintf fmt "{%a + %a}" dump_operand base dump_operand offset

and dump_ila fmt instr =
  let d = dump_operand in
  match instr.instr_core with
  | Add (x, y, z)    -> Format.fprintf fmt "add %a, %a, %a@." d x d y d z
  | Sub (x, y, z)    -> Format.fprintf fmt "sub %a, %a, %a@." d x d y d z
  | Mul (x, y, z)    -> Format.fprintf fmt "mul %a, %a, %a@." d x d y d z
  | Div (x, y, z)    -> Format.fprintf fmt "div %a, %a, %a@." d x d y d z
  | Str (addr, y)    -> Format.fprintf fmt "store %a, %a@." dump_index_mode addr d y
  | Ld (x, addr)     -> Format.fprintf fmt "load %a, %a@." d x dump_index_mode addr
  | Conv (x, y)      -> Format.fprintf fmt "conv %a, %a@." d x d y
  | Mov (x, y)       -> Format.fprintf fmt "mov %a, %a@." d x d y
  | Branch (k, x, y, b) ->
      let msg =
        match k with
        | Le -> "ble" | Lt -> "blt" | Ge -> "bge"
        | Gt -> "bgt" | Eq -> "beq" | Ne -> "bne" in
      Format.fprintf fmt "%s %a, %a, block_%d@." msg d x d y b.id
  | Bmov (k, x, y, z, a) ->
      let msg =
        match k with
        | Le -> "mle" | Lt -> "mlt" | Ge -> "mge"
        | Gt -> "mgt" | Eq -> "meq" | Ne -> "mne" in
      Format.fprintf fmt "%s %a, %a, %a, %a@." msg d x d y d z d z
  | Call (opt, (tpath, _), ops) ->
      begin match opt with
      | None ->
        Format.fprintf fmt "call %a, %a@." dump_tpath tpath
          (fun fmt l -> List.iter (d fmt) l) ops
      | Some op ->
        Format.fprintf fmt "call %a, %a, %a@." d op dump_tpath tpath
          (fun fmt l -> List.iter (d fmt) l) ops
      end
  | Ret (Some x) -> Format.fprintf fmt "ret %a@." d x
  | Ret None     -> Format.fprintf fmt "ret@."
  | Alloc (x, y) -> Format.fprintf fmt "alloc %a, %a@." d x d y
  | Dealloc (x, y) -> Format.fprintf fmt "dealloc %a, %a@." d x d y

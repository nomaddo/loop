open Batteries
open Typ
open Operand
open Ir
open Ilb

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
  (* Format.fprintf fmt "dealloc: %a@." (fun fmt -> List.iter (dump_ilb fmt)) dealloc; *)
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
  | Tv     i -> begin
      try
        let Arg j = List.find (function Arg i -> true | _ -> false) op.attrs in
        Format.fprintf fmt "@arg_%d(%d)(%a)" j i dump_typ op.typ
      with Not_found ->
        Format.fprintf fmt "@%d(%a)" i dump_typ op.typ
    end
  | Sp       -> Format.fprintf fmt "sp"
  | Fp       -> Format.fprintf fmt "fp"

and dump_tpath fmt = function
  | Tident.Tident id -> Format.fprintf fmt "%s(%d)" id.name id.id

and dump_typ fmt = function
  | I2 -> Format.fprintf fmt "I2"
  | I4 -> Format.fprintf fmt "I4"
  | R8 -> Format.fprintf fmt "R8"

and dump_bcs fmt bc =
  Ir_util.reset_traverse_attr bc;
  Ir_util.iter 10 (dump_bc fmt) bc

and dump_bc fmt bc =
  Format.fprintf fmt "--- block_%d %a@." bc.id dump_bc_loop_info bc;
  Format.fprintf fmt "prevs: %a@." (fun fmt ->
    List.iter (fun (bc:'a Ir.basic_block) -> Format.fprintf fmt "%d " bc.id)) bc.preds;
  Format.fprintf fmt "succs: %a@." (fun fmt ->
    List.iter (fun (bc:'a Ir.basic_block) -> Format.fprintf fmt "%d " bc.id)) bc.succs;
  List.iter (dump_ilb fmt) bc.instrs;
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

and dump_set fmt set =
  Format.printf "(";
  Set.iter (fun op -> Format.printf "%a " dump_operand op) set;
  Format.printf ")";

and dump_var_attr fmt instr =
  try
    if !Flags.show_valid_vars then
      match Ir.Instr.find_vars instr with
      | Some set -> dump_set fmt set
      | None -> ()
    else ()
  with  _ -> ()

and dump_ilb fmt instr =
  let d = dump_operand in
  let dv = dump_var_attr in
  match instr.instr_core with
  | Add (x, y, z)    -> Format.fprintf fmt "add %a, %a, %a %a@." d x d y d z dv instr
  | Sub (x, y, z)    -> Format.fprintf fmt "sub %a, %a, %a %a@." d x d y d z dv instr
  | Mul (x, y, z)    -> Format.fprintf fmt "mul %a, %a, %a %a@." d x d y d z dv instr
  | Div (x, y, z)    -> Format.fprintf fmt "div %a, %a, %a %a@." d x d y d z dv instr
  | Str (addr, y)    -> Format.fprintf fmt "store %a, %a %a@." dump_index_mode addr d y dv instr
  | Conv (x, y)      -> Format.fprintf fmt "conv %a, %a %a@." d x d y dv instr
  | Mov (x, y)       -> Format.fprintf fmt "mov %a, %a %a@." d x d y dv instr
  | Branch (k, b) ->
      let msg =
        match k with
        | Le -> "ble" | Lt -> "blt" | Ge -> "bge"
        | Gt -> "bgt" | Eq -> "beq" | Ne -> "bne" in
      Format.fprintf fmt "%s block_%d@." msg b.id
  | Bmov (k, x, y) ->
      let msg =
        match k with
        | Le -> "mle" | Lt -> "mlt" | Ge -> "mge"
        | Gt -> "mgt" | Eq -> "meq" | Ne -> "mne" in
      Format.fprintf fmt "%s %a, %a %a@." msg d x d y dv instr
  | Call (opt, tpath, ops) ->
      begin match opt with
      | None ->
        Format.fprintf fmt "call %a, %a %a@." dump_tpath tpath dv instr
          (fun fmt l -> List.iter (d fmt) l) ops
      | Some op ->
        Format.fprintf fmt "call %a, %a, %a %a@." d op dump_tpath tpath dv instr
          (fun fmt l -> List.iter (d fmt) l) ops
      end
  | Ret Some x -> Format.fprintf fmt "ret %a %a@." d x dv instr
  | Ret None   -> Format.fprintf fmt "ret %a@." dv instr
  | Alloc (x, y) -> Format.fprintf fmt "alloc %a, %a %a@." d x d y dv instr
  | Dealloc (x, y) -> Format.fprintf fmt "dealloc %a, %a %a@." d x d y dv instr
  | Bl tpath -> Format.fprintf fmt "bl %a %a@." dump_tpath  tpath dv instr
  | B tpath -> Format.fprintf fmt "b %a %a@." dump_tpath  tpath dv instr
  | Ldr (x, y) -> Format.fprintf fmt "ldr %a, %a %a@." d x dump_index_mode y dv instr
  | Cmp (x, y) -> Format.fprintf fmt "cmp %a, %a %a@." d x d y dv instr

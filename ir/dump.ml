open Typ
open Operand
open Ir

let rec dump fmt {funcs; memories} =
  List.iter (dump_func fmt) funcs

and dump_func fmt {label_name; args; entry; all_bc} =
  Format.fprintf fmt "%s: %a@.%a@." label_name dump_args args dump_bcs all_bc

and dump_args fmt l =
  Format.fprintf fmt "{";
  List.iter (Format.fprintf fmt "%a," dump_operand) l;
  Format.fprintf fmt "}"

and dump_operand fmt op =
  match op.opcore with
  | Iconst i -> Format.fprintf fmt "%d" i
  | Rconst s -> Format.fprintf fmt "%s" s
  | Memory i -> Format.fprintf fmt "M_%d" i
  | Var    tpath -> Format.fprintf fmt "$%a(%a)"
        dump_tpath tpath dump_typ op.typ
  | Tv     i -> Format.fprintf fmt "@%d(%a)" i dump_typ op.typ

and dump_tpath fmt = function
  | Tident.Tident id -> Format.fprintf fmt "%s(%d)" id.name id.id
  | Tident.Tpath (id, path) ->
      Format.fprintf fmt "%s."id.name;
      dump_tpath fmt path

and dump_typ fmt = function
  | I4 -> Format.fprintf fmt "I4"
  | R4 -> Format.fprintf fmt "R4"

and dump_bcs fmt bcs =
  List.iter (dump_bc fmt) bcs

and dump_bc fmt bc =
  Format.fprintf fmt "--- block_%d %a@." bc.id dump_bc_loop_info bc;
  List.iter (dump_instr fmt) bc.instrs;
  Format.fprintf fmt "--- next block_%s@.@."
    (match bc.next with None -> "none" | Some b -> string_of_int b.id)

and dump_bc_loop_info fmt bc =
  let info = bc.loop in
  let msg =
    if bc == info.entrance then "entr" else
    if bc == info.terminate then "term" else
    if bc == info.epilogue then "epi" else "" in
  let msg =
  match info.pre_initial, info.initial with
  | None, None -> msg
  | Some b1, Some b2 ->
      if bc == b1 then "pre" else
      if bc == b2 then "init" else msg
  | Some b1, None ->
      if bc == b1 then "pre" else msg
  | None, Some b2 ->
      if bc == b2 then "init" else msg in
  Format.fprintf fmt "%s: %d" msg (Obj.magic info)

and dump_instr fmt instr =
  let d = dump_operand in
  match instr.instr_core with
  | Add (x, y, z)    -> Format.fprintf fmt "add %a, %a, %a@." d x d y d z
  | Sub (x, y, z)    -> Format.fprintf fmt "sub %a, %a, %a@." d x d y d z
  | Mul (x, y, z)    -> Format.fprintf fmt "mul %a, %a, %a@." d x d y d z
  | Div (x, y, z)    -> Format.fprintf fmt "div %a, %a, %a@." d x d y d z
  | Str (x, y)       -> Format.fprintf fmt "store %a, %a@." d x d y
  | Ld (x, y)        -> Format.fprintf fmt "load %a, %a@." d x d y
  | Conv (x, y)      -> Format.fprintf fmt "conv %a, %a@." d x d y
  | Mov (x, y)       -> Format.fprintf fmt "mov %a, %a@." d x d y
  | Ble (x, y, b)    -> Format.fprintf fmt "ble %a, %a, block_%d@." d x d y b.id
  | Blt (x, y, b)    -> Format.fprintf fmt "blt %a, %a, block_%d@." d x d y b.id
  | Bge (x, y, b)    -> Format.fprintf fmt "bge %a, %a, block_%d@." d x d y b.id
  | Bgt (x, y, b)    -> Format.fprintf fmt "bgt %a, %a, block_%d@." d x d y b.id
  | Beq (x, y, b)    -> Format.fprintf fmt "beq %a, %a, block_%d@." d x d y b.id
  | Bne (x, y, b)    -> Format.fprintf fmt "bne %a, %a, block_%d@." d x d y b.id
  | Mle (x, y, z, a) -> Format.fprintf fmt "mle %a, %a, %a, %a@." d x d y d z d z
  | Mlt (x, y, z, a) -> Format.fprintf fmt "mlt %a, %a, %a, %a@." d x d y d z d z
  | Mge (x, y, z, a) -> Format.fprintf fmt "mge %a, %a, %a, %a@." d x d y d z d z
  | Mgt (x, y, z, a) -> Format.fprintf fmt "mgt %a, %a, %a, %a@." d x d y d z d z
  | Meq (x, y, z, a) -> Format.fprintf fmt "meq %a, %a, %a, %a@." d x d y d z d z
  | Mne (x, y, z, a) -> Format.fprintf fmt "mne %a, %a, %a, %a@." d x d y d z d z
  | Callm (x, tpath, ops) -> Format.fprintf fmt "callm %a %a, %a@." d x
        dump_tpath tpath  (fun fmt l -> List.iter (d fmt) l) ops
  | Call (tpath, ops) -> Format.fprintf fmt "call %a, %a@." dump_tpath tpath
        (fun fmt l -> List.iter (d fmt) l) ops
  | Ret x -> Format.fprintf fmt "ret %a@." d x
  | Alloc (x, y) -> Format.fprintf fmt "alloc %a, %a@." d x d y

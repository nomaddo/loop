(* stack allocate
   allocate, deallocを消してSPを用いた表現にする

   定数のallocは関数のentryに集めて
   最後のdeallocをdeallocを消し１つにまとめる
*)

open Etc
open Batteries
open Ir
open Ilb

let stack_pointer = Operand.new_operand Operand.Sp Typ.I4
let frame_pointer = Operand.new_operand Operand.Fp Typ.I4

let new_instr = Instr.new_instr

let rec delete e l =
  List.fold_left (fun acc (x, y) ->
    if x = e then acc else (x, y) :: acc) [] l

let sum l =
  let open Operand in
  let x = ref [] in
  let i = List.fold_left (fun acc op -> match op.opcore with
      | Iconst i -> acc + i
      | Tv _ -> x := op :: !x; acc
      | _ -> acc) 0 l in
  (!x, i)

let split f l =
  let rec split acc f = function
  | x :: xs ->
      if f x
      then (List.rev (x :: acc), xs)
      else split (x :: acc) f xs
  | [] -> (List.rev acc, []) in
  split [] f l

let rec add_chain op = function
  | x :: xs ->
      Instr.new_instr ++ Add (op, op, x) :: add_chain op xs
  | [] -> []

let calc_ldr l op index_mode =
  match index_mode with
  | Base_offset {base; offset} -> begin
      let _, b = split (fun (a, b) -> a = base) l
                 |> snd |> List.split in
      Format.printf "calc_ldr1: %a@." Ilb_dump.dump_operand op;
      List.iter (fun a ->
        Format.printf "calc_ldr2: %a@." Ilb_dump.dump_operand a) b;
      let ops, i = sum b in
      let tv = Operand.new_tv Typ.I4 in
      let instrs = add_chain tv ops in
      if instrs = [] then
        [Ldr (op, Base_offset {base = frame_pointer; offset})
         |> Instr.new_instr]
      else
        [Mov (tv, Operand.new_operand (Operand.Iconst i) Typ.I4) |> Instr.new_instr] @
          instrs @
          [Add (tv, tv, offset) |> Instr.new_instr] @
          [Ldr (op, Base_offset {base = frame_pointer;
                                 offset = tv}) |> Instr.new_instr]
    end
  | _ -> assert false

let calc_str l index_mode op =
  match index_mode with
  | Base_offset {base; offset} -> begin
      let _, b = split (fun (a, b) -> a = base) l
                 |> snd |> List.split in
      let ops, i = sum b in
      let tv = Operand.new_tv Typ.I4 in
      let instrs = add_chain tv ops in
      if instrs = [] then
        [Str (Base_offset {base = frame_pointer; offset}, op)
         |> Instr.new_instr]
      else
        [Mov (tv, Operand.new_operand (Operand.Iconst i) Typ.I4) |> Instr.new_instr] @
          instrs @
          [Add (tv, tv, offset) |> Instr.new_instr] @
          [Str (Base_offset {base = frame_pointer;
                             offset = tv}, op) |> Instr.new_instr]
    end
  | _ -> failwith "calc_str"

let alloc_bc l bc =
  List.fold_left (fun acc instr ->
    match instr.instr_core with
    | Alloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Sub (stack_pointer, stack_pointer, op2)));
        (* Format.printf "alloc: %a %a@." Ilb_dump.dump_operand op1 Ilb_dump.dump_operand op2; *)
        (op1, op2) :: acc
    | Dealloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Add (stack_pointer, stack_pointer, op2)));
        (* Format.printf "dealloc: %a %a@." Ilb_dump.dump_operand op1 Ilb_dump.dump_operand op2; *)
        delete op1 acc
    | Ldr (op, index_mode) ->
        Instr.replace_instrs bc instr (calc_ldr acc op index_mode);
        acc
    | Str (index_mode, op) ->
        (* List.iter (fun (a, b) -> Format.printf "Str: %a %a@." Ilb_dump.dump_operand a Ilb_dump.dump_operand b) acc; *)
        Instr.replace_instrs bc instr (calc_str acc index_mode op);
        acc
    | _ -> acc) [] bc.instrs

let stack_allocate {Ir.label_name; args; entry} =
  ignore (Ir_util.fold 200 alloc_bc [] entry)

let transl {funcs; memories} =
  List.iter stack_allocate funcs

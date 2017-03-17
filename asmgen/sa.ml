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

(* operand list受け取って、定数を加算してi, それ以外はxに入れてしまう *)
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

let replace_index l op index_mode f =
  match index_mode with
  | Base_offset {base; offset} -> begin
      let _, b = split (fun (a, b) -> a = base) l
                 |> snd |> List.split in
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
          [f op (Base_offset {base = frame_pointer; offset = tv}) ]
    end
  | _ -> assert false

let calc_ldr l op index_mode =
  replace_index l op index_mode (fun op index -> Ldr (op, index) |> Instr.new_instr)

let calc_str l index_mode op =
  replace_index l op index_mode (fun op index -> Str (index, op) |> Instr.new_instr)

let mark_arg args index_mode =
  begin match index_mode with
    | Base_offset {base; offset} ->
        begin match List.index_of base args with
          | None -> ()
          | Some i ->
              base.Operand.attrs <- Operand.Arg i :: base.attrs
        end
    | Operand _ -> failwith "alloc_bc"
  end

let alloc_bc (l, args) bc =
  List.fold_left (fun (acc, args) instr ->
    match instr.instr_core with
    | Alloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Sub (stack_pointer, stack_pointer, op2)));
        (op1, op2) :: acc ,args
    | Dealloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Add (stack_pointer, stack_pointer, op2)));
        delete op1 acc, args
    | Ldr (op, index_mode) ->
        mark_arg args index_mode;
        Instr.replace_instrs bc instr (calc_ldr acc op index_mode);
        acc, args
    | Str (index_mode, op) ->
        mark_arg args index_mode;
        Instr.replace_instrs bc instr (calc_str acc index_mode op);
        acc, args
    | _ -> acc, args) (l, args) bc.instrs

let arg_to_tv_bc map bc =
  List.iter (fun instr ->
    match instr.instr_core with
    | Ldr (op, index_mode) ->
        begin match index_mode with
          | Base_offset {base; offset} ->
              begin try
                let tv = Map.find base map in
                if Operand.is_zero offset then
                  Instr.replace bc instr (Instr.new_instr (Mov (op, tv)))
                else
                  Instr.replace bc instr
                    (Instr.new_instr (Ldr(op, Base_offset {base = tv; offset})))
                with Not_found -> ()
              end
          | Operand _ -> failwith "arg_to_tv"
        end
    | Str (index_mode, op) ->
        begin match index_mode with
          | Base_offset {base; offset} ->
              begin try
                let tv = Map.find base map in
                if Operand.is_zero offset then
                  Instr.replace bc instr (Instr.new_instr (Mov (tv, op)))
                else
                  Instr.replace bc instr
                    (Instr.new_instr (Str (Base_offset {base = tv; offset}, op)))
                with Not_found -> ()
              end
          | Operand _ -> failwith "arg_to_tv"
        end
    | _ -> ()) bc.instrs; map

let arg_to_tv {Ir.label_name; args; entry} =
  let map = List.fold_lefti (fun map i (arg: Operand.operand) ->
      let tv = Operand.new_tv arg.Operand.typ in
      tv.Operand.attrs <- Operand.Arg i :: tv.Operand.attrs;
      Map.add arg tv map) Map.empty args in
  ignore (Ir_util.fold 100 arg_to_tv_bc map entry)

let stack_allocate {Ir.label_name; args; entry} =
  ignore (Ir_util.fold 200 alloc_bc ([], args) entry)

let transl {funcs; memories} =
  List.iter arg_to_tv funcs;
  List.iter stack_allocate funcs

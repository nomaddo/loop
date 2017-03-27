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

  Etc.dmsg Flags.dflag (fun () ->
    Format.printf "sum: @.";
    List.iter (Format.printf "%a " Ilb_dump.dump_operand) l;
    Format.printf "@.sum end: @.");

  let x = ref [] in
  let i = List.fold_left (fun acc op -> match op.opcore with
      | Iconst i -> acc + i
      | Tv _ -> x := op :: !x; acc
      | _ -> acc) 0 l in
  Format.printf "sum: keisan %d@." i;
  (!x, i)

let split (f: Operand.operand * Operand.operand -> bool) l =
  let rec split acc (f: Operand.operand * Operand.operand -> bool) = function
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

let replace_index stack dyn_arrays op index_mode f =
  match index_mode with
  | Base_offset {base; offset} -> begin
      Flags.dmsg (fun () ->
        Format.printf "replace_index: base = %a@." Ilb_dump.dump_operand base;
        List.iter (fun (e, size) -> Format.printf "replace_index: stack: %a@." Ilb_dump.dump_tpath e) stack;
      );
      let tv = Operand.new_tv Typ.I4 in
      let instrs =
        try
          match base.Operand.opcore with
          | Operand.Var tpath ->
              let i, _ = List.findi (fun _ (e, _) -> tpath = e) stack in
              let offset_n = List.take (i - 1) stack |> List.fold_left (fun sum (_, i) -> sum + i) 0 in
              let offset = Operand.new_operand (Operand.Iconst offset_n) Typ.I4 in
              [new_instr ++ Add (tv, stack_pointer, offset)]
          | _ -> raise Not_found
        with Not_found ->       (* variable array *)
          let i, _ = List.findi (fun i (e, _) -> base = e) dyn_arrays in
          let offset_n =
            List.fold_left (fun sum (_, i) -> sum + i) 0 stack in
          let offset = Operand.new_operand (Operand.Iconst offset_n) Typ.I4 in
          let instr = new_instr ++ Add (tv, stack_pointer, offset) in
          let instrs =
            List.take (i - 1) dyn_arrays
            |> List.fold_left (fun instrs (_, op) ->
              new_instr ++ Add (tv, tv, op) :: instrs) [] |> List.rev in
          instr :: instrs in
      instrs @ [f op (Base_offset {base = tv; offset})]
    end

let calc_ldr stack dyn_arrays op index_mode =
  replace_index stack dyn_arrays op index_mode
    (fun op index -> Ldr (op, index) |> Instr.new_instr)

let calc_str stack dyn_arrays index_mode op =
  replace_index stack dyn_arrays op index_mode
    (fun op index -> Str (index, op) |> Instr.new_instr)

let mark_arg args index_mode =
  begin match index_mode with
    | Base_offset {base; offset} ->
        begin match List.index_of base args with
          | None -> ()
          | Some i ->
              base.Operand.operand_attrs <- Operand.Arg i :: base.Operand.operand_attrs
        end
  end

let alloc_bc (l, args) bc =
  List.fold_left (fun (acc, args) instr ->
    match instr.instr_core with
    | Alloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Sub (stack_pointer, stack_pointer, op2)));
        acc ,args
    | Dealloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Add (stack_pointer, stack_pointer, op2)));
        acc, args
    | Ldr (op, index_mode) ->
        mark_arg args index_mode;
        let instrs = calc_ldr acc bc.dyn_arrays op index_mode in
        Flags.dmsg (fun () ->
          Format.printf "alloc_bc: %a to %a@." Ilb_dump.dump_ilb instr
            (fun fmt -> List.iter (Format.fprintf fmt "%a" Ilb_dump.dump_ilb)) instrs);
        Instr.replace_instrs bc instr instrs;
        acc, args
    | Str (index_mode, op) ->
        mark_arg args index_mode;
        let instrs = calc_str acc bc.dyn_arrays index_mode op in
        Flags.dmsg (fun () ->
          Format.printf "alloc_bc: %a to %a@." Ilb_dump.dump_ilb instr
            (fun fmt -> List.iter (Format.fprintf fmt "%a" Ilb_dump.dump_ilb)) instrs);
        Instr.replace_instrs bc instr instrs;
        acc, args
    | _ -> acc, args) (l, args) bc.instrs

let arg_to_tv_bc map bc =
  List.iter (fun instr ->
    match instr.instr_core with
    | Ldr (op, index_mode) ->
        begin match index_mode with
          | Base_offset {base; offset} ->
              begin try
                  match Map.find base map with
                  | `Reg tv ->
                      if Operand.is_zero offset then
                        Instr.replace bc instr (Instr.new_instr (Mov (op, tv)))
                      else
                        Instr.replace bc instr
                          (Instr.new_instr (Ldr(op, Base_offset {base = tv; offset})))
                  | `Stack tv ->
                      Instr.replace bc instr
                        (Instr.new_instr (Ldr(op, Base_offset {base = tv; offset})))
                with Not_found -> ()
              end
        end
    | Str (index_mode, op) ->
        begin match index_mode with
          | Base_offset {base; offset} ->
              begin try
                  match Map.find base map with
                  | `Reg tv ->
                      if Operand.is_zero offset then
                        Instr.replace bc instr (Instr.new_instr (Mov (tv, op)))
                      else
                        Instr.replace bc instr
                          (Instr.new_instr (Str (Base_offset {base = tv; offset}, op)))
                  | `Stack tv ->
                      Instr.replace bc instr
                        (Instr.new_instr (Str (Base_offset {base = tv; offset}, op)))

                with Not_found -> ()
              end
        end
    | _ -> ()) bc.instrs; map

(* TODO
   いろいろな種類のレジスタであることを意識して変換
*)

(* ヒープレイアウトlからオフセット位置を計算する *)
let size_of_offset (l: (Operand.operand * int) list) x =
  let sum h =
    List.fold_left (fun acc (_, i) -> acc + i) 0 h in
  let i, _ = List.findi (fun i (e, _) -> e == x) l in
  let h, _ = List.split_nth i l in
  sum h

let arg_to_tv {Ir.label_name; args; entry} =
  let r_cnt = ref 0 in
  let s_cnt = ref 0 in
  let map = List.fold_lefti (fun map i (arg: Operand.operand) ->
      match arg.Operand.typ with
      | I4 ->
          if !r_cnt > 4 then begin    (* レジスタに乗り切らない *)
            incr r_cnt;
            let tv = Operand.new_tv arg.Operand.typ in
            tv.Operand.operand_attrs <- Operand.Arg i :: tv.Operand.operand_attrs;
            Map.add arg (`Stack tv) map
          end
          else begin
            incr r_cnt;
            let tv = Operand.new_tv arg.Operand.typ in
            tv.Operand.operand_attrs <- Operand.Arg i :: tv.Operand.operand_attrs;
            Map.add arg (`Reg tv) map end) Map.empty args in
  ignore (Ir_util.fold 100 arg_to_tv_bc map entry)

let max_stack set bc =
  let set_ = Set.of_list bc.stack_layout in
  Set.union set set_

let stack_allocate {Ir.label_name; args; entry} =
  let max_set = Ir_util.fold 101 max_stack Set.empty entry in
  let stack = Set.to_list max_set in
  ignore (Ir_util.fold 200 alloc_bc (stack, args) entry)

let transl {funcs; memories} =
  List.iter arg_to_tv funcs;
  List.iter stack_allocate funcs

(* stack allocate
   allocate, deallocを消してSPを用いた表現にする

   定数のallocは関数のentryに集めて
   最後のdeallocをdeallocを消し１つにまとめる
*)

open Etc
open Batteries
open Ir
open Ilb

exception Multi of Ilb.ilb Ir.instr list

let stack_pointer = Operand.new_operand Operand.Sp Typ.I4

let new_instr = Instr.new_instr

let dump_stack stack =
    Format.printf "stack: begin@.";
    List.iter (fun (op, size) -> Format.printf "stack: %a %d@." Ilb_dump.dump_operand op size) stack;
    Format.printf "stack: end@."

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
        List.iter (fun (op, size) ->
          Format.printf "replace_index: stack: %a@." Ilb_dump.dump_operand op) stack;
      );
      let tv = Operand.new_tv Typ.I4 in
      let instrs =
        try
          match base.Operand.opcore with
          | Operand.Var tpath ->
              let i, _ = List.findi (fun _ (e, _) ->
                  match e.Operand.opcore with
                  | Operand.Var tpath_ -> tpath = tpath_ | _ -> false) stack in
              let offset_n = List.take (i - 1) stack |> List.fold_left (fun sum (_, i) -> sum + i) 0 in
              let offset = Operand.new_operand (Operand.Iconst offset_n) Typ.I4 in
              [new_instr ++ Add (tv, stack_pointer, offset)]
          | _ -> raise Not_found
        with Not_found ->       (* variable array *)
          dump_stack stack;
          Format.printf "base: %a@." Ilb_dump.dump_operand base;
          Format.printf "offset: %a@." Ilb_dump.dump_operand offset;
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

let alloc_bc (stack, args) bc =
  List.fold_left (fun (stack, args) instr ->
    match instr.instr_core with
    | Alloc (op1, op2) ->
        let tv = Operand.new_tv Typ.I4 in
        Instr.replace_instrs bc instr [new_instr ++ Sub (tv, stack_pointer, op2)];
        stack ,args
    | Dealloc (op1, op2) ->
        Instr.replace bc instr (new_instr (Add (stack_pointer, stack_pointer, op2)));
        stack, args
    | Ldr (op, index_mode) ->
        mark_arg args index_mode;
        let instrs = calc_ldr stack bc.dyn_arrays op index_mode in
        Flags.dmsg (fun () ->
          Format.printf "alloc_bc: %a to %a@." Ilb_dump.dump_ilb instr
            (fun fmt -> List.iter (Format.fprintf fmt "%a" Ilb_dump.dump_ilb)) instrs);
        Instr.replace_instrs bc instr instrs;
        stack, args
    | Str (index_mode, op) ->
        mark_arg args index_mode;
        let instrs = calc_str stack bc.dyn_arrays index_mode op in
        Flags.dmsg (fun () ->
          Format.printf "alloc_bc: %a to %a@." Ilb_dump.dump_ilb instr
            (fun fmt -> List.iter (Format.fprintf fmt "%a" Ilb_dump.dump_ilb)) instrs);
        Instr.replace_instrs bc instr instrs;
        stack, args
    | _ -> stack, args) (stack, args) bc.instrs

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

let arg_to_tv stack {Ir.label_name; args; entry} =
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
  ignore (Ir_util.fold 1204 arg_to_tv_bc map entry)

let copy = Operand.copy
let is_marked = Operand.is_marked
let is_constant = Operand.is_constant

let r_num = 12
let d_num = 8

let reg_cnt =
  let cnt = ref 0 in
  fun () -> incr cnt; !cnt

type alloc = Success of Operand.operand
           | Spill   of Operand.operand * Ilb.ilb

module Regs = struct
  exception Find of int

  type t =
    { r_reg: (Operand.operand * int) option array;
      d_reg: (Operand.operand * int) option array;
    }

  let dump_regs regs =
    Format.printf "dump_regs: r_reg@.";
    Array.iteri (fun i -> function
      | None -> Format.printf "%d: None@." i
      | Some (op, _) -> Format.printf "%d: %a@." i Ilb_dump.dump_operand op) regs.r_reg;
    Format.printf "dump_regs: d_reg@.";
      Array.iteri (fun i -> function
      | None -> Format.printf "%d: None@." i
      | Some (op, _) -> Format.printf "%d: %a@." i Ilb_dump.dump_operand op) regs.d_reg;
    Format.printf "dump_regs: end@."

  let findi regs op =
    try
      match op.Operand.typ with
      | Typ.I4 -> Some (Array.findi (function
            None -> false
          | Some (op_, _) -> op.Operand.id = op_.Operand.id) regs.r_reg)
      | Typ.R8 -> Some (Array.findi (function
            None -> false
          | Some (op_, _) -> op.Operand.id = op_.Operand.id) regs.d_reg)
    with Not_found -> None

  let mem regs op =
    match op.Operand.opcore with
    | Operand.Tv _ -> begin match findi regs op with None -> false | Some _ -> true end
    | _ -> false

  let create_regs () =
    { r_reg = Array.create r_num None;
      d_reg = Array.create d_num None;
    }

  let update regs i op =
    match op.Operand.typ with
    | Typ.I4 -> regs.d_reg.(i) <- Some (op, reg_cnt ())
    | Typ.R8 -> regs.r_reg.(i) <- Some (op, reg_cnt ())

  let is_allocated stack regs op =
    match op.Operand.opcore with
    | Operand.Tv _ -> mem regs op
    | Operand.Var _ ->
        List.exists (fun (op_, _) -> op = op_) stack
    | _ -> true

  let alloc regs op =
    let open Operand in
    (* 既に割付け済み *)
    if not (List.filter (function Reg _ -> true
                                | _ -> false) op.operand_attrs = []) then Success op else
    match op.typ with
    | Typ.I4 ->
        begin try
          let n = Array.findi (function Some (op_, _) -> op = op_
                                      | None -> true) regs.r_reg in
          regs.r_reg.(n) <- Some (op, reg_cnt ());
          op.operand_attrs <- Reg (Typ.I4, n) :: op.operand_attrs;
          Success (copy op)
          with Not_found -> begin
              (* 適当にライフレンジが短いのを避難させる *)
              let min = Array.fold_lefti (fun (n, nth) i -> function
                  | (Some (op, n_)) -> if n > n_ then (n, nth) else (n_, i)
                  | None -> (n, nth))
                  (int_size, -100) regs.r_reg |> snd in
              op.operand_attrs <- Reg (Typ.I4, min) :: op.operand_attrs;
              regs.r_reg.(min) <- Some (op, reg_cnt ());
              failwith "i4 spill"
            end end
    | Typ.R8 ->
        begin try
          let n = Array.findi (function Some (opt, _) -> false
                                      | None -> true) regs.d_reg in
          op.operand_attrs <- Reg (Typ.R8, n) :: op.operand_attrs;
          regs.r_reg.(n) <- Some (op, reg_cnt ());
          Success (copy op)
          with Not_found -> begin
              let min = Array.fold_lefti (fun (n, nth) i  -> function
                  | Some (op, n_) -> if n > n_ then (n, nth) else (n_, i)
                  | None          -> (n, nth)
                ) (int_size, -100) regs.d_reg |> snd in
              op.operand_attrs <- Reg (Typ.R8, min) :: op.operand_attrs;
              regs.d_reg.(min) <- Some (op, reg_cnt ());
              failwith "r8 spill"
            end end

  let break_regs regs funtyp =
    begin match funtyp.Typ.ret_typ with
    | None -> ()
    | Some ret_typ ->
        begin match ret_typ with
          | I4 -> regs.r_reg.(0) <- None
          | R8 -> regs.d_reg.(0) <- None
        end
    end;
    let r = ref 0 in
    let d = ref 0 in
    List.iter (function
      | Typ.I4 ->
          if !r < 4 then
            regs.r_reg.(!r) <- None;
          incr r
      | Typ.R8 ->
        if !d < 8 then
          regs.d_reg.(!r) <- None;
        incr d) funtyp.Typ.args

  let set_after_call regs op =
    match op.Operand.typ with
    | Typ.I4 ->
        regs.r_reg.(0) <- Some (op, reg_cnt ())
    | Typ.R8 ->
        regs.d_reg.(0) <- Some (op, reg_cnt ())
end

open Regs

let transl_bc (stack, regs) bc =
  List.iter (fun instr -> try
    let core = match instr.instr_core with
    | Add (op1, op2, op3) ->
        assert (is_allocated stack regs op2 &&
                is_allocated stack regs op3);
        begin match op1.typ with
          | Typ.I4 ->
              begin match Regs.alloc regs op1 with
                | Success op1 -> Add (op1, op2, op3)
                | Spill _ -> failwith ""
              end
          | Typ.R8 -> failwith ""
        end
    | Sub (op1, op2, op3) ->
        assert (is_allocated stack regs op2 &&
                is_allocated stack regs op3);
        begin match op1.typ with
          | Typ.I4 ->
              begin match Regs.alloc regs op1 with
                | Success op1 -> Sub (op1, op2, op3)
                | Spill _ -> failwith ""
              end
          | Typ.R8 -> failwith ""
        end

    | Mul (op1, op2, op3) ->
        assert (is_allocated stack regs op2 && is_allocated stack regs op3);
        begin match op1.typ with
          | Typ.I4 ->
              begin match Regs.alloc regs op1 with
                | Success op1 -> Mul (op1, op2, op3)
                | Spill _ -> failwith ""
              end
          | Typ.R8 -> failwith ""
        end
    | Div (op1, op2, op3) ->
        assert (is_allocated stack regs op2 && is_allocated stack regs op3);
        begin match op1.typ with
          | Typ.I4 ->
              begin match Regs.alloc regs op1 with
                | Success op1 -> Div (op1, op2, op3)
                | Spill _ -> failwith ""
              end
          | Typ.R8 -> failwith ""
        end
    | Str (index_mode, op) ->
        assert (is_allocated stack regs op);
        begin match index_mode with
        | Base_offset {base; offset} ->
            assert (is_allocated stack regs base);
            assert (is_allocated stack regs offset);
            Str (index_mode, op)
        end
    | Ldr (op, index_mode) ->
        begin match index_mode with
        | Base_offset {base; offset} ->
            assert (is_allocated stack regs base);
            assert (is_allocated stack regs offset);
            match alloc regs op with
            | Success op -> Ldr (op, index_mode)
        end
    | Mov (op1, op2) -> begin
        assert (Regs.mem regs op2);
        match alloc regs op1 with
        | Success op -> Mov (op1, op2)
        | Spill _ -> failwith ""
      end
    | Cmp (op1, op2) ->
        assert (is_allocated stack regs op1);
        assert (is_allocated stack regs op2);
        instr.instr_core
    | Branch _ -> instr.instr_core
    | Bmov (k, op1, op2) ->
        assert (is_allocated stack regs op2);
        begin match alloc regs op1 with
        | Success op2 -> Bmov (k, op1, op2)
        | Spill _ -> failwith ""
        end
    | Ret opt ->
        begin match opt with
        | None -> Ret None
        | Some op ->
            let op_ = Operand.new_tv ~attrs:[Reg (op.typ, 0)] op.typ in
            raise (Multi [Instr.new_instr (Mov (copy op, op_));
                          Instr.new_instr (Ret (Some op_))])
        end
    | Conv (op1, op2) ->
        assert (is_allocated stack regs op2);
        begin match alloc regs op1 with
          | Success op1 -> Conv (op1, op2)
          | Spill _ -> failwith ""
        end
    | Call (opt, (tpath, funtyp), ops) ->
        assert (List.for_all (fun op -> Regs.is_allocated stack regs op) ops);
        let r = ref 0 in
        let d = ref 0 in
        let instrs =
          List.map (fun op ->
              match op.typ with
              | Typ.I4 ->
                  if !r < 4 then begin
                    let reg = Reg (op.typ, !r) in
                    let new_op = new_tv ~attrs:[Reg (op.typ, )] op.typ in
                    Instr.new_instr (Mov (new_op, op))
                  end else
                    Instr.new_instr (Str (Base_offset {base=Sp}))

          )
        match opt with
        | None ->
            assert (funtyp.Typ.ret_typ = None);
            Regs.break_regs regs funtyp;

        | Some op ->
            assert (funtyp.Typ.ret_typ != None);
            Regs.break_regs regs funtyp;
            Regs.set_after_call regs op
    | _ -> failwith ""
    in
    instr.instr_core <- core
    with Multi instrs ->
      Instr.replace_instrs bc instr instrs
  ) bc.instrs

let init_env func =
  let r = ref 0 in
  let d = ref 0 in
  let regs = Regs.create_regs () in
  let stack = ref [] in
  List.iter (fun arg ->
    match arg.Operand.typ with
    | Typ.I4 ->
        let n = !r in
        incr r;
        if n < 4 then
          regs.r_reg.(n) <- Some (arg, reg_cnt ())
        else
          stack := (arg, 4) :: !stack
    | Typ.R8 ->
        let n = !d in
        incr d;
        if n < d_num then
          regs.d_reg.(n) <- Some (arg, reg_cnt ())
        else
          stack := (arg, 8) :: !stack) func.args;
  (regs, stack)

let max_stack set bc =
  let set_ = Set.of_list bc.stack_layout in
  Set.union set set_

let stack_allocate stack {Ir.label_name; args; entry} =
  let max_set = Ir_util.fold 1203 max_stack Set.empty entry in
  let stack = !stack @ Set.to_list max_set in
  ignore (Ir_util.fold 1202 alloc_bc (stack, args) entry)

let transl_func func =
  let regs, stack = init_env func in
  (* arg_to_tv stack func; *)
  stack_allocate stack func;
  while Ilb_simplify.remove_constant_move func do
    Ilb_simplify.remove_redundant_instr func
  done;
  try
    Ir_util.iter_ 1201 transl_bc (!stack, regs) func.entry
  with exn ->
    dump_regs regs;
    Format.printf "%a@." Ilb_dump.dump_bcs func.entry;
    raise exn

let transl {funcs; memories} =
  List.iter transl_func funcs

(* 定数のmovの除去

*)

open Batteries
open Etc
open Operand
open Ir
open Ilb

let new_instr = Instr.new_instr

let flag = ref false

let constant_folding x y f g =
  match x.opcore, y.opcore with
  | Iconst i, Iconst j ->
      assert (x.typ == y.typ);
      new_operand ++ Iconst (f i j) ++ x.typ
  | Rconst s, Rconst t ->
      assert (x.typ == y.typ);
      new_operand ++ Rconst (g ++ float_of_string s ++ float_of_string t
                             |> string_of_float) ++ x.typ
  | _ -> assert false

let try_replace map op =
  let d = Ilb_dump.dump_operand in
  try let op_ = Map.find op map in
    Format.printf "try_replace: %a to %a@." d op d op_;
    flag_on flag; op_
  with Not_found -> op

let compare_op k op1 op2 =
  let cmpi k i =
    match k with
    | Le -> i <= 0
    | Lt -> i < 0
    | Ge -> i >= 0
    | Gt -> i > 0
    | Eq -> i = 0
    | Ne -> i != 0 in
  let cmpr k s =
    let f = float_of_string s in
    match k with
    | Le -> f <= 0.
    | Lt -> f < 0.
    | Ge -> f >= 0.
    | Gt -> f > 0.
    | Eq -> f = 0.
    | Ne -> f != 0. in
  let op = constant_folding op1 op2 (+) (+.) in
  match op.opcore with
  | Iconst i -> cmpi k i
  | Rconst s -> cmpr k s
  | _ -> failwith "compare_op"

let simplify_index map index_mode =
  match index_mode with
  | Base_offset { base;  offset; } ->
      Base_offset  { base;  offset = try_replace map offset; }

let optimize_constant map op1 op2 op3 f g =
  if Operand.is_constant op2 && Operand.is_constant op3 then
    let op2_ = constant_folding op2 op3 f g in
    let map = Map.add op1 op2_ map in
    Some map
  else if Operand.is_zero op2 then
    let map = Map.add op1 op3 map in
    Some map
  else if Operand.is_zero op3 then
    let map = Map.add op1 op2 map in
    Some map
  else None

let simplify_bc map bc =
  List.fold_left (fun (map, instrs) instr ->
    match instr.instr_core with
    | Add (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ (+) (+.) in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Add (op1, op2_, op3_) in
          (map, instr :: instrs)

    | Sub (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ (-) (-.) in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Sub (op1, op2_, op3_) in
          (map, instr :: instrs)

    | Mul (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ ( * ) ( *. )in
          (map, new_instr ++ Mov (op1, op2)  :: instrs)
        else
          let instr = new_instr ++ Mul (op1, op2_, op3_) in
          (map, instr :: instrs)
    | Div (op1, op2, op3) ->
        let op2_ = try_replace map op2 in
        let op3_ = try_replace map op3 in
        let map = Map.remove op1 map in
        if Operand.is_constant op2_ && Operand.is_constant op3_ then
          let op2 = constant_folding op2_ op3_ (/) (/.) in
          (map, new_instr ++ Mov (op1, op2) :: instrs)
        else
          let instr = new_instr ++ Div (op1, op2_, op3_) in
          (map, instr :: instrs)
    | Mov (op1, op2) ->
        let op2_ = try_replace map op2 in
        if Operand.is_constant op2_ then begin
          let map = Map.add op1 op2_ map in
          (map, instrs) end
        else begin
          let instr = new_instr ++ Mov (op1, op2_) in
          let map = Map.add op1 op2_ map in
          (map, instr :: instrs)
        end
    | Str   (index_mode, op) ->
        let instr =
          new_instr
          ++ Str (simplify_index map index_mode, try_replace map op) in
        (map, instr :: instrs)
    | Ldr (op, index_mode) ->
        let instr =
          new_instr
          ++ Ldr (op, simplify_index map index_mode) in
        let map = Map.remove op map in
        (map, instr :: instrs)
    | Conv  (op1 , op2) ->
        let instr =
          new_instr
          ++ Conv (op1, try_replace map op2) in
        let map = Map.remove op1 map in
        (map, instr :: instrs)
    | Branch (k, dist) ->
        (map, instr :: instrs)
    | Bmov (k, op1 , op2) ->
        if op1 = op2
        then (map, instrs)
        else
          let map = Map.remove op1 map in
          (map, instr :: instrs)
    | Call  (opt, tpath , ops) ->
        let instr =
          new_instr ++ Call (opt, tpath, List.map (try_replace map) ops) in
        (map, instr :: instrs)
    | Ret (Some op) ->
        let instr =
          new_instr ++ Ret (Some (try_replace map op)) in
         (map, instr :: instrs)
    | Ret None ->
        (map, instr :: instrs)
    | Alloc (op1, op2) ->
        let instr =
          new_instr ++ Alloc (op1, try_replace map op2) in
        (map, instr :: instrs)
    | Dealloc (op1, op2) ->
        let instr =
          new_instr ++ Dealloc (op1, try_replace map op2) in
        (map, instr :: instrs)
    | _ -> (map, instr :: instrs)
  ) (map, []) bc.instrs
  |> (fun (map, instrs) -> map, List.rev instrs)

let dump_map map =
  Format.printf "dump_map: begin@.";
  Map.foldi (fun k v () -> Format.printf "%a -> %a@." Dump.dump_operand k Dump.dump_operand v) map ();
  Format.printf "dump_map: end@."

let remove_constant_move {Ir.entry} =
  flag_off flag;
  let map = Map.empty in
  let map = Ir_util.fold (entry.traverse_attr + 1) (fun map bc ->
      let map, instrs = simplify_bc map bc in
      bc.instrs <- instrs; map) map entry in
  ignore map;
  (* dump_map map; *)
  !flag

let add op set =
  match op.Operand.opcore with
  | Operand.Iconst _ -> set
  | Operand.Rconst _ -> set
  | Operand.Tv    _  -> Set.add op set
  | Operand.Fp       -> set
  | Operand.Sp       -> set
  | Operand.Var _    -> set

let mark_instr set instr =
  match instr.instr_core with
  | Add     (op1, op2, op3)   ->
      let set = add op2 set
                |> add op3
                |> Set.remove op1 in
      Instr.remove_attr (function Vars _ -> true) instr;
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Sub     (op1, op2, op3)   ->
      let set = add op2 set in
      let set = add op3 set in
      let set = Set.remove op1 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Mul     (op1, op2, op3)   ->
      let set = add op2 set in
      let set = add op3 set in
      let set = Set.remove op1 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Div     (op1, op2, op3)   ->
      let set = add op2 set in
      let set = add op3 set in
      let set = Set.remove op1 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Str     (index_mode, op)  ->
      let set = add op set in
      begin match index_mode with
        | Ir.Base_offset {base; offset} ->
            let set = add offset set in
            instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
            set
      end
  | Ldr     (op, index_mode)  ->
      begin match index_mode with
        | Ir.Base_offset {base; offset} ->
            let set = add offset set in
            instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
            set
      end
  | Mov     (op1, op2)        ->
      let set = add op2 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Cmp     (op1, op2)        ->
      let set = add op1 set |> add op2 in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Branch  (kind, bc)        -> set
  | Bmov    (kind, op1 , op2) ->
      let set = add op2 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Ret     op                ->
      begin match op with
        | None -> set
        | Some op ->
            let set = add op set in
            instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
            set
      end
  | Conv    (op1, op2)        ->
      let set = add op2 set in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | Call    (opt, tpath, ops)      ->
      let set = List.fold_left (fun set op -> add op set) set ops in
      instr.instr_attrs <- Ir.Vars set :: instr.instr_attrs;
      set
  | _ -> begin
      Format.printf "invalid instr: %a@." Ilb_dump.dump_ilb instr;
      assert false
    end

let remove_instr bc instr =
  let f bc instr op =
      begin match Instr.next bc instr with
      | None -> ()
      | Some instr_ ->
          match Instr.find_vars instr_ with
          | None -> ()
          | Some set ->
              if Set.mem op set then () else begin
                Format.printf "remove_instr: %a"Ilb_dump.dump_ilb instr;
                Instr.delete bc instr
              end
      end in
  match instr.instr_core with
  | Add (op1, op2, op3) -> f bc instr op1
  | Sub (op1, op2, op3) -> f bc instr op1
  | Mul (op1, op2, op3) -> f bc instr op1
  | Div (op1, op2, op3) -> f bc instr op1
  | Str (index_mode, op) -> ()
  | Ldr (op, index_mode) -> f bc instr op
  | Mov (op1, op2) -> f bc instr op1
  | Cmp (op1, op2) -> ()
  | Branch (k, bc) -> ()
  | Bmov (k, op1, op2) -> f bc instr op1
  | Bl tpath | B tpath -> ()
  | Ret opopt -> ()
  | Conv (op1, op2) -> f bc instr op1
  | Call (opt, tpath, ops) -> ()
  | Alloc (op1, op2)
  | Dealloc (op1, op2) -> ()

let rec set_var_attr set bc =
  if bc.traverse_attr = 700 then set else begin
    bc.traverse_attr <- 700;
    match bc.next with
    | None -> List.fold_left mark_instr set (List.rev bc.instrs)
    | Some _ ->
        List.map (set_var_attr set) bc.succs
        |> List.fold_left (fun acc set -> Set.union acc set) set
  end


let remove_redundant_instr {label_name; args; entry} =
  set_var_attr Set.empty entry |> ignore;
  Ir_util.iter 701 (fun bc ->
    List.iter (remove_instr bc) bc.instrs) entry

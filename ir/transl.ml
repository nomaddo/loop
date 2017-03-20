open Batteries
open Etc
open Operand
open Typ
open Ir
open Instr
open Typed_ast

let zero = Operand.new_operand (Iconst 0) I4

let symbol_tbl: (Tident.path, Typed_ast.typ) Hashtbl.t = Hashtbl.create 10

let global_intf : Tyenv.intf ref = ref (Obj.magic ())

let transl_typ typ =
  match typ with
  | Void -> failwith "void has no values"
  | Int  -> I4
  | Real -> R8
  | Array (typ, e) -> I4
  | Lambda (typs, rettyp) -> I4

let transl_rettyp typ =
  match typ with
  | Lambda (types, rettyp) -> transl_typ rettyp
  | _ -> failwith "transl_rettyp"

let rec typ_sizeof op typ =
  match typ with
    | Int -> [new_instr ++ Mov (op, Operand.new_operand (Iconst 4) I4)]
    | Real -> [new_instr ++ Mov (op, Operand.new_operand (Iconst 8) I4)]
    | Array (typ, e) ->
        let op1 = new_tv I4 in
        let x = typ_sizeof op1 typ in
        let op2 = new_tv I4 in
        let y = transl_expr op2 e in
        x @ y @ [new_instr ++ Mul (op, op1, op2)]
    | Lambda _ -> [new_instr ++ Mov (op, addr_size_op)]
    | _ -> failwith "typ_sizeof"

and transl_expr op e =
  match e.Typed_ast.expr_core with
  | Typed_ast.Var tpath ->
      let tmp = Operand.new_tv ++ transl_typ e.Typed_ast.expr_typ in
      let operand =
        new_operand (Var tpath) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Ld (tmp, Base_offset {base=operand; offset=zero})] @
      [Instr.new_instr ++ Ir.Mov (op, tmp)]
  | Iconst int ->
      let operand =
        new_operand (Iconst int) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Mov (op, operand)]
  | Rconst str ->
      let operand =
        new_operand (Rconst str) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Mov (op, operand)]
  | Aref (tpath, es) ->
      let base = Operand.new_var tpath (transl_typ e.expr_typ) in
      let size = Operand.new_tv (transl_typ e.expr_typ) in
      let instrs1, ops =
        List.fold_left (fun (acc, vars) e ->
          let v = Operand.new_tv (transl_typ e.expr_typ) in
          let instrs = transl_expr v e in
          acc @ instrs , v::vars) ([],[]) es in
      let ops = List.rev ops in
      let instrs2 = transl_ashape (Tyenv.find_path !global_intf tpath) size ops in
      instrs1 @ instrs2 @
        [new_instr ++ Ld (op, Ir.Base_offset {base; offset = size})]
  | Call (_ as tpath, es) ->
      match tpath with
      | Tident.Tident ident ->
          begin match Tyenv.find_prim ident with
            | None ->
                let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
                List.fold_left2 (fun  l op e -> transl_expr op e @ l) [] ops es
                @ [new_instr ++ Ir.Call (Some op, tpath, ops)]
            | Some s ->
                transl_prim es op op.typ s
          end

and multiple_ops ret ops =
  List.fold_left (fun instrs op -> new_instr ++ Mul (ret, ret, op) :: instrs) [] ops
  |> List.rev

and calc_each_size_of_dimension atyp =
  let instrs = ref [] in
  let ops = ref [] in
  let rec  f atyp =
    match atyp with
    | Array (atyp_, e) ->
        let op = new_tv ++ transl_typ e.expr_typ in
        ops := op :: !ops;
        instrs := !instrs @ transl_expr op e;
        f atyp_
    | _ as typ -> typ in
  let basetyp = f atyp in
  basetyp, !ops, !instrs

and transl_ashape atyp retop ops =
  let rec calc_rec acc retop ops eops =
    match eops with
    | x :: [] -> new_instr ++ Mov (retop, x) :: acc
    | x :: tl -> multiple_ops x ops @
          calc_rec (new_instr ++ Add (retop, retop, x) :: acc) retop (List.tl ops) tl
    | [] -> failwith "calc_rec" in
  let basetyp, dimension_sizes, instrs1 = calc_each_size_of_dimension atyp in
  let new_op = new_tv I4 in
  let instrs2 = typ_sizeof new_op basetyp in
  instrs1 @ instrs2 @ calc_rec [] retop (List.tl dimension_sizes) ops @
    [new_instr ++ Mul (retop,  retop, new_op)]

and transl_bin es op typ f =
  let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
  let instrs = List.fold_left2 (fun acc op e -> transl_expr op e :: acc)
      [] ops es |> List.rev |> List.flatten  in
  instrs @ f op ops

and transl_prim es op typ s =
  match s with
  | "plus" | "fplus"  -> transl_bin es op typ add
  | "minus"| "fminus" -> transl_bin es op typ sub
  | "mul"  | "fmul"   -> transl_bin es op typ mul
  | "div"  | "fdiv"   -> transl_bin es op typ div
  | "gt"   | "fgt"    -> transl_bin es op typ mge
  | "lt"   | "flt"    -> transl_bin es op typ mlt
  | "ge"   | "fge"    -> transl_bin es op typ mge
  | "le"   | "fle"    -> transl_bin es op typ mle
  | "eq"   | "feq"    -> transl_bin es op typ meq
  | "ne"   | "fne"    -> transl_bin es op typ mne
  | "rtoi" | "itor"   -> transl_bin es op typ conv
  | _ -> raise Not_found

let rec typ_sizeof_static = function
  | Int -> int_size
  | Real -> double_size
  | Array (typ, e) -> typ_sizeof_static typ * expr_sizeof_static e
  | Lambda _ -> addr_size
  | Void -> failwith "typ_sizeof_static"

and expr_sizeof_static e =
  match e.expr_core with
  | Iconst i -> i
  | Call (Tident.Tident id, es) ->
      let [x; y] = List.map expr_sizeof_static es in
      begin match Tyenv.find_prim id with
      | Some "plus" -> x + y
      | Some "minus" -> x - y
      | Some "mul" -> x * y
      | Some "div" -> x / y
      | _ -> failwith "expr_sizeof_static: 1"
      end
  |  _ -> failwith "expr_sizeof_static: 2"

let rec transl_decls parent bc dealloc decls =
  match decls with
  | [] ->
      if Instr.include_ret bc then () else
        bc.instrs <- bc.instrs @ List.map new_instr dealloc;
      bc, dealloc
  | h :: tl -> begin
      match h with
      | Typed_ast.Decl (typ, tpath, None) ->
          Hashtbl.add symbol_tbl tpath typ;
          let op = new_tv I4 in
          let instrs = typ_sizeof op typ in
          let var = Operand.new_var tpath ++ transl_typ typ in
          bc.instrs <- bc.instrs @ instrs @ [new_instr ++ Alloc (var, op)];
          transl_decls parent bc (Dealloc (var,op) :: dealloc) tl
      | Decl (typ, tpath, Some e) ->
          Hashtbl.add symbol_tbl tpath typ;
          let size_op = new_tv I4 in
          let alloc_instrs = typ_sizeof size_op typ in
          let op = Operand.new_var tpath (transl_typ e.expr_typ) in
          let tmp = Operand.new_tv (transl_typ e.expr_typ) in
          let instrs = transl_expr tmp e in
          bc.instrs <- bc.instrs @ alloc_instrs @ [new_instr ++ Alloc (op, size_op)]
            @ instrs @ [new_instr ++ Str (Base_offset {base=new_var tpath ++ transl_typ typ; offset=zero}, tmp)];
          transl_decls parent bc (Dealloc (op, size_op) :: dealloc) tl
      | If (cond, d, dopt) -> begin
          let op = new_tv I4 in
          let then_bc = Bc.new_bc parent in
          let next_bc = Bc.new_bc parent in
          let instrs, binstr =
            try match cond.expr_core with
              | Call  (Tident.Tident id, es) ->
                assert (Tyenv.find_prim id != None);
                let [x; y] as ops = List.map (fun e -> new_tv (transl_typ e.expr_typ)) es in
                let instrs = List.map2 transl_expr  ops es |> List.flatten in
                let s = Tyenv.find_prim id |> Option.get in
                instrs, begin match s with
                  | "gt" | "fgt" -> [Instr.new_branch Gt x y then_bc ]
                  | "ge" | "fge" -> [Instr.new_branch Ge x y then_bc ]
                  | "lt" | "flt" -> [Instr.new_branch Lt x y then_bc ]
                  | "le" | "fle" -> [Instr.new_branch Le x y then_bc ]
                  | "eq" | "feq" -> [Instr.new_branch Eq x y then_bc ]
                  | "ne" | "fne" -> [Instr.new_branch Ne x y then_bc ]
                  | _ -> assert false
                end
              | _ -> assert false
            with _ -> transl_expr op cond, [Instr.new_branch Ne op false_op then_bc] in
          bc.instrs <- bc.instrs @ instrs @ binstr;
          let last_then, dealloc_ = transl_decls parent then_bc dealloc d in
          Bc.concat_bc last_then next_bc;
          begin match dopt with
            | None ->
                Bc.concat_bc bc next_bc;
                transl_decls parent next_bc dealloc_ tl
            | Some d' ->
                let else_bc = Bc.new_bc parent in
                let last_else, dealloc__ = transl_decls parent else_bc dealloc d' in
                Bc.concat_bc bc else_bc;
                Bc.concat_bc last_else next_bc;
                transl_decls parent next_bc dealloc__ tl
          end
        end
      | Assign (tpath, e) ->
          let var = Operand.new_var tpath (transl_typ e.expr_typ) in
          let op = new_tv (transl_typ e.expr_typ) in
          let instrs1 = transl_expr op e in
          let instrs2 =
            [new_instr ++ Str (Base_offset { base = var; offset = zero}, op)] in
          bc.instrs <- bc.instrs @ instrs1 @ instrs2;
          transl_decls parent bc dealloc tl
      | Astore (tpath, es, e) ->
          let ops = List.map (fun e -> new_tv ++ transl_typ e.expr_typ) es in
          let instrs1 =
            fold_rev2 (fun () -> transl_expr) () ops es |> snd |> List.flatten in
          let atyp = Hashtbl.find symbol_tbl tpath in
          let retop = new_tv ++ transl_typ e.expr_typ in
          let instrs2 = transl_ashape atyp retop ops in
          let result_op = new_tv ++ transl_typ e.expr_typ in
          let instrs3 = transl_expr result_op e in
          let instrs4 =
            [new_instr ++ Str (Base_offset {base = Operand.new_var tpath ++ transl_typ (Tyenv.find_path !global_intf tpath); offset = retop}, result_op)] in
          bc.instrs <- bc.instrs @ instrs1 @ instrs2 @ instrs3 @ instrs4;
          transl_decls parent bc dealloc tl
      | For    (tpath, e1, dir, e2, eopt, ds) ->
          let ind_var = new_operand ~attrs:[Ind] (Var tpath) (transl_typ e1.expr_typ) in

          let pre_initial = Ir.Bc.new_bc parent in
          let initial     = Ir.Bc.new_bc parent in
          let entrance    = Ir.Bc.new_bc parent in
          let terminate   = Ir.Bc.new_bc parent in
          let epilogue    = Ir.Bc.new_bc parent in

          (* pre_initial *)
          let op1 = new_tv ++ transl_typ e1.expr_typ in
          let op2 = new_tv ++ transl_typ e2.expr_typ in
          let in1 = transl_expr op1 e1 in
          let in2 = transl_expr op2 e2 in
          pre_initial.instrs <- in1 @ in2 @
              [new_instr ++
                 (match dir with Ast.To -> Branch (Lt, op1, op2, epilogue)
                               | Ast.Downto -> Branch (Gt, op1, op2, epilogue))];
          Bc.concat_bc bc pre_initial;

          (* initial *)
          let bct_var = new_tv ~attrs:[Bct] op1.typ in
          let in1 =
            Mov (new_tv ~attrs:[Ind; Tpath tpath] (transl_typ e1.expr_typ), op1) in
          let in2 = Sub (bct_var, op2, op1) in
          initial.instrs <- List.map new_instr [in1; in2];
          Bc.concat_bc pre_initial initial;

          (* entrance *)
          let last_body, dealloc_ = transl_decls parent entrance dealloc ds in
          Bc.concat_bc initial entrance;
          Bc.concat_bc last_body terminate;

          (* terminate *)
          let byop = new_tv ++ transl_typ e2.expr_typ in
          let f, g = match dir with Ast.To -> sub, add | Ast.Downto -> add, sub in
          let in3 = match eopt with
            | None   -> [new_instr ++ Mov (byop, new_operand (Iconst 1) I4)]
            | Some e -> transl_expr byop e in
          let in4 = f bct_var [bct_var; byop] in
          let in5 = g ind_var [ind_var; byop] in
          let in6 = Instr.new_branch Le bct_var op2 entrance in
          terminate.instrs <- in3 @ in4 @ in5 @ List.map new_instr dealloc_ @ [in6];
          Bc.concat_bc terminate epilogue;

          let next_bc = Bc.new_bc parent in
          Bc.concat_bc epilogue next_bc;
          transl_decls parent next_bc dealloc tl
      | While  (e, ds)                        -> begin
          let entrance    = Ir.Bc.new_bc parent in
          let terminate   = Ir.Bc.new_bc parent in
          let epilogue    = Ir.Bc.new_bc parent in

          (* terminate *)
          Bc.concat_bc bc terminate;

          let op = new_tv ++ transl_typ e.expr_typ in
          let instrs1 = transl_expr op e in
          let instrs2 = [Instr.new_branch Eq op true_op entrance] in
          terminate.instrs <- instrs1 @ instrs2;

          (* entrance *)
          let last_bc, dealloc_ = transl_decls parent entrance [] ds in
          Bc.concat_bc last_bc terminate;

          last_bc.instrs <- last_bc.instrs @ List.map new_instr dealloc_;

          (* epilogue *)
          Bc.concat_bc terminate epilogue;

          let next_bc = Bc.new_bc parent in
          Bc.concat_bc epilogue next_bc;
          transl_decls parent next_bc dealloc tl;
        end
      | Call   (Tident.Tident ident as tpath, es, typ) ->
          begin match Tyenv.find_prim ident with
            | None ->
                let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
                let instrs =
                  List.fold_left2 (fun  l op e -> transl_expr op e @ l) [] ops es
                  @ [new_instr ++ Ir.Call (None, tpath, ops)] in
                bc.instrs <- bc.instrs @ instrs;
            | Some s ->
                let op = new_tv I4 in
                let instrs = transl_prim es op op.typ s in
                bc.instrs <- bc.instrs @ instrs;
          end;
          transl_decls parent bc dealloc tl
      | Return e                              ->
          let op = new_tv ++ transl_typ e.expr_typ in
          let instrs = transl_expr op e in
          bc.instrs <- bc.instrs @ instrs @ List.map new_instr dealloc
            @ [new_instr (Ret op)];
          bc, []
    end

let transl_args args =
  List.map (fun (tpath, typ) ->
    new_operand (Var tpath) (transl_typ typ)) args

let transl_toplevel (f,g,m) bc mod_name = function
  | Typed_ast.Fundef (typ, tpath, args, decls) ->
      let total = Loop_info.total_loop () in
      let entry = Bc.new_bc total in
      transl_decls total entry [] decls |> ignore;
      Ir_util.set_control_flow entry;
      let func =
        { label_name = Tident.make_label mod_name tpath;
          args = transl_args args;
          entry; loops = [] } in
      (func::f, g, m)
  | Global_var (typ, tpath, None) ->
      let mx = { shape = typ_sizeof_static typ; name = tpath } in
      (f, mx :: g, m)
  | Global_var (typ, tpath, Some e) ->
      let mx = { shape = typ_sizeof_static typ; name = tpath } in
      let op = new_tv ++ transl_typ e.expr_typ in
      let instrs = transl_expr op e in
      (f, mx :: g, instrs :: m)
  | Prim (typ, path, str) -> (f, g, m)

let implementation mod_name intf tops =
  let bc = Bc.new_bc Loop_info.dummy_loop  in
  global_intf := intf;
  let funcs, memories, _ =
    List.map (transl_toplevel ([],[],[]) bc mod_name) tops
    |> List.fold_left (fun (a, b, c) (x, y, z) ->
      x @ a, y @ b, z @ c) ([], [], []) in
  { funcs = List.rev funcs ; memories = List.rev memories; }

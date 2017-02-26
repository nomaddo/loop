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

let addr_size = 4

let rec typ_sizeof = function
  | Int -> 4
  | Real -> 8
  | Array (typ, e) ->
      typ_sizeof typ * addr_size * expr_sizeof e
  | Lambda _ -> addr_size
  | _ -> failwith "typ_sizeof"

and expr_sizeof e =
  match e.expr_core with
  | Iconst i -> i
  | Call (Tident.Tident id, [x; y]) ->
      let sopt = Tyenv.find_prim id in
      begin match sopt with
        | None -> failwith "expr_sizeof: sizeof array in toplevel must be fixed"
        | Some "+" -> expr_sizeof x + expr_sizeof y
        | Some "-" -> expr_sizeof x + expr_sizeof y
        | Some "*" -> expr_sizeof x + expr_sizeof y
        | Some "/" -> expr_sizeof x + expr_sizeof y
        | _ -> failwith "expr_sizeof: sizeof array in toplevel must be fixed"
      end
  | _ -> failwith "expr_sizeof: sizeof array in toplevel must be fixed"

let rec transl_expr bc op e =
  match e.Typed_ast.expr_core with
  | Typed_ast.Var tpath ->
      let operand =
        new_operand (Var tpath) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Mov (op, operand) ++ bc]
  | Iconst int ->
      let operand =
        new_operand (Iconst int) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Mov (op, operand) ++ bc]
  | Rconst str ->
      let operand =
        new_operand (Rconst str) (transl_typ e.expr_typ) in
      [Instr.new_instr ++ Ir.Mov (op, operand) ++ bc]
  | Aref (tpath, es) ->
      let v = Operand.new_name tpath (transl_typ e.expr_typ) in
      let tv = Operand.new_tv (transl_typ e.expr_typ) in
      let instrs1, ops =
        List.fold_left (fun (x, y) e ->
          let v = Operand.new_tv (transl_typ e.expr_typ) in
          let instrs = transl_expr bc v e in
          x @ instrs , v::y) ([],[]) es in
      let ops = List.rev ops in
      let instrs2 = transl_ashape bc (Tyenv.find_path !global_intf tpath) v ops in
      instrs1 @ instrs2 @ [new_instr ++ Ld (op, Ir.Base_offset {base = tv; offset = v}) ++ bc]
  | Call (_ as tpath, es) ->
      match tpath with
      | Tident.Tident ident ->
          begin match Tyenv.find_prim ident with
            | None ->
                let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
                List.fold_left2 (fun  l op e -> transl_expr bc op e @ l) [] ops es
                @ [new_instr ++ Callm (op, tpath, ops) ++ bc]
            | Some s ->
                transl_prim bc es op op.typ s
          end

and transl_ashape bc atyp retop ops =
  let rec elist acc atyp =
    match atyp with
    | Array (atyp', e) ->
        elist ((fun op -> transl_expr bc op e) :: acc) atyp'
    | _ as typ -> typ, List.rev acc in
  let rec calc_ops eop ops =
    List.fold_left (fun l op -> new_instr ++ Mul (eop, eop, op) ++ bc :: l) [] ops
    |> List.rev in
  let rec calc_rec acc retop ops eops =
    match eops with
    | x :: [] -> new_instr ++ Mov (retop, x) ++ bc :: acc
    | x :: tl -> calc_ops x ops @
          calc_rec (new_instr ++ Add (retop, retop, x) ++ bc :: acc) retop (List.tl ops) tl
    | [] -> failwith "calc_rec" in
  let basetyp, fs =  elist [] atyp in
  let ashape_ops, instrs =
    List.map (fun f -> let op = new_tv I4 in op, f op) fs
    |> List.split |> fun (x, y) -> x, List.flatten y in
  let i = typ_sizeof basetyp in
  instrs @ calc_rec [] retop (List.tl ashape_ops) ops @
    [new_instr ++ Mul (retop,  retop, Operand.new_operand (Iconst i) I4) ++ bc]

and transl_bin bc es op typ f =
  let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
  let instrs = List.fold_left2 (fun acc op e -> transl_expr bc op e :: acc)
      [] ops es |> List.rev |> List.flatten  in
  instrs @ f op ops

and transl_prim bc es op typ s =
  match s with
  | "plus" | "fplus"  -> transl_bin bc es op typ (add  bc)
  | "minus"| "fminus" -> transl_bin bc es op typ (sub  bc)
  | "mul"  | "fmul"   -> transl_bin bc es op typ (mul  bc)
  | "div"  | "fdiv"   -> transl_bin bc es op typ (div  bc)
  | "gt"   | "fgt"    -> transl_bin bc es op typ (mge  bc)
  | "lt"   | "flt"    -> transl_bin bc es op typ (mlt  bc)
  | "ge"   | "fge"    -> transl_bin bc es op typ (mge  bc)
  | "le"   | "fle"    -> transl_bin bc es op typ (mle  bc)
  | "eq"   | "feq"    -> transl_bin bc es op typ (meq  bc)
  | "ne"   | "fne"    -> transl_bin bc es op typ (mne  bc)
  | "rtoi" | "itor"   -> transl_bin bc es op typ (conv bc)
  | _ -> raise Not_found

let rec transl_decls parent bc decls =
  match decls with
  | [] -> bc
  | h :: tl -> begin
      match h with
      (* 配列が宣言された場合にメモリ確保に相当する処理をどう表すのか *)
      | Typed_ast.Decl (typ, tpath, None) ->
          Hashtbl.add symbol_tbl tpath typ;
          transl_decls parent bc tl
      | Decl (typ, tpath, Some e) ->
          Hashtbl.add symbol_tbl tpath typ;
          let op = Operand.new_name tpath (transl_typ e.expr_typ) in
          let instrs = transl_expr bc op e in
          bc.instrs <- bc.instrs @ instrs;
          transl_decls parent bc tl
      | If (cond, d, dopt) -> begin
          let op = new_var I4 in
          let then_bc = Bc.new_bc parent in
          let next_bc = Bc.new_bc parent in
          let instrs, binstr =
            try match cond.expr_core with
              | Call  (Tident.Tident id, es) ->
                assert (Tyenv.find_prim id != None);
                let [x; y] as ops = List.map (fun e -> new_tv (transl_typ e.expr_typ)) es in
                let instrs = List.map2 (transl_expr bc) ops es |> List.flatten in
                let s = Tyenv.find_prim id |> Option.get in
                instrs, begin match s with
                  | "gt" | "fgt" -> [new_instr ++ Ir.Bgt (x, y, then_bc) ++ bc]
                  | "ge" | "fge" -> [new_instr ++ Ir.Bge (x, y, then_bc) ++ bc]
                  | "lt" | "flt" -> [new_instr ++ Ir.Blt (x, y, then_bc) ++ bc]
                  | "le" | "fle" -> [new_instr ++ Ir.Bgt (x, y, then_bc) ++ bc]
                  | "eq" | "feq" -> [new_instr ++ Ir.Beq (x, y, then_bc) ++ bc]
                  | "ne" | "fne" -> [new_instr ++ Ir.Bne (x, y, then_bc) ++ bc]
                  | _ -> assert false
                end
              | _ -> assert false
            with _ -> transl_expr bc op cond, [new_instr ++ Ir.Beq (op, true_op, then_bc) ++ bc] in
          bc.instrs <- bc.instrs @ instrs @ binstr;
          let last_then = transl_decls parent then_bc d in
          ignore (transl_decls parent next_bc tl);
          Bc.concat_bc last_then next_bc;
          Bc.concat_bc bc next_bc;
          begin match dopt with
            | None -> next_bc
            | Some d' ->
                let else_bc = Bc.new_bc parent in
                let last_else = transl_decls parent else_bc d' in
                Bc.concat_bc bc else_bc;
                Bc.concat_bc last_else next_bc;
                next_bc
          end
        end
      | Assign (tpath, e) ->
          let var = new_operand (Operand.Var tpath) (transl_typ e.expr_typ) in
          let op = new_var (transl_typ e.expr_typ) in
          let instrs1 = transl_expr bc op e in
          let instrs2 =
            if Tyenv.find_path_is_toplevel !global_intf tpath
            then [new_instr ++ Str (Base_offset { base = var; offset = zero}, op) ++ bc]
            else [new_instr ++ Mov (var, op) ++ bc]
            in
          bc.instrs <- bc.instrs @ instrs1 @ instrs2;
          transl_decls parent bc tl
      | Astore (tpath, es, e) ->
          let ops = List.map (fun e -> new_tv ++ transl_typ e.expr_typ) es in
          let instrs1 =
            fold_rev2 transl_expr bc ops es |> snd |> List.flatten in
          let atyp = Hashtbl.find symbol_tbl tpath in
          let retop = new_tv ++ transl_typ e.expr_typ in
          let instrs2 = transl_ashape bc atyp retop ops in
          let result_op = new_tv ++ transl_typ e.expr_typ in
          let instrs3 = transl_expr bc result_op e in
          let instrs4 =
            [new_instr ++ Str (Base_offset {base = Operand.new_name tpath ++ transl_typ (Tyenv.find_path !global_intf tpath); offset = retop}, result_op) ++ bc] in
          bc.instrs <- bc.instrs @ instrs1 @ instrs2 @ instrs3 @ instrs4;
          transl_decls parent bc tl
      | For    (tpath, e1, dir, e2, eopt, ds) ->
          let ind_var = new_operand ~attrs:[Ind] (Var tpath) (transl_typ e1.expr_typ) in
          let loop_info = Loop_info.make_loop ~pre:true ~init:true parent in
          loop_info.attrs <- [For];
          loop_info.ind_vars <- [ind_var];

          let pre_initial = Option.get loop_info.pre_initial in
          let initial = Option.get loop_info.initial in
          let entrance = loop_info.entrance in
          let terminate = loop_info.terminate in
          let epilogue = loop_info.epilogue in

          (* pre_initial *)
          let op1 = new_var ++ transl_typ e1.expr_typ in
          let op2 = new_var ++ transl_typ e2.expr_typ in
          let in1 = transl_expr pre_initial op1 e1 in
          let in2 = transl_expr pre_initial op2 e2 in
          pre_initial.instrs <- in1 @ in2 @
              [new_instr ++
                 (match dir with Ast.To -> Blt (op1, op2, epilogue)
                               | Ast.Downto -> Bgt (op1, op2, epilogue)) ++
                 pre_initial];
          Bc.concat_bc bc pre_initial;

          (* initial *)
          let bct_var = new_var ~attrs:[Bct] op1.typ in
          let in1 =
            new_instr ++ Mov (new_name ~attrs:[Ind] tpath ++ transl_typ e1.expr_typ, op1) ++ initial in
          let in2 =
            new_instr ++ Sub (bct_var, op2, op1) ++ initial in
          initial.instrs <- [in1; in2];
          Bc.concat_bc pre_initial initial;

          (* entrance *)
          let last_body = transl_decls loop_info entrance ds in
          Bc.concat_bc initial entrance;
          Bc.concat_bc last_body terminate;

          (* terminate *)
          let byop = new_var ++ transl_typ e2.expr_typ in
          let f, g = match dir with Ast.To -> sub, add | Ast.Downto -> add, sub in
          let in3 = match eopt with
            | None   -> [new_instr ++ Mov (byop, new_operand (Iconst 1) I4) ++ terminate]
            | Some e -> transl_expr initial byop e in
          let in4 = f initial bct_var [bct_var; byop] in
          let in5 = g terminate ind_var [ind_var; byop] in
          let in6 = new_instr ++ Ble (bct_var, op2, entrance) ++ terminate in
          terminate.instrs <- in3 @ in4 @ in5 @ [in6];
          Bc.concat_bc terminate epilogue;

          let next_bc = Bc.new_bc parent in
          Bc.concat_bc epilogue next_bc;
          next_bc
      | While  (e, ds)                        -> begin
          let loop_info = Loop_info.make_loop ~pre:false ~init:false parent in
          let entrance = loop_info.entrance in
          let terminate = loop_info.terminate in
          let epilogue = loop_info.epilogue in

          (* terminate *)
          Bc.concat_bc bc terminate;

          let op = new_tv ++ transl_typ e.expr_typ in
          let instrs1 = transl_expr terminate op e in
          let instrs2 = [new_instr ++ Beq (op, true_op, entrance) ++ terminate] in
          terminate.instrs <- instrs1 @ instrs2;

          (* entrance *)
          let last_bc = transl_decls loop_info entrance ds in
          Bc.concat_bc last_bc terminate;

          (* epilogue *)
          Bc.concat_bc terminate epilogue;

          let next_bc = Bc.new_bc parent in
          Bc.concat_bc epilogue next_bc;
          next_bc
        end
      | Call   (Tident.Tident ident as tpath, es, typ) ->
          begin match Tyenv.find_prim ident with
            | None ->
                let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
                let instrs =
                  List.fold_left2 (fun  l op e -> transl_expr bc op e @ l) [] ops es
                  @ [new_instr ++ Ir.Call (tpath, ops) ++ bc] in
                bc.instrs <- bc.instrs @ instrs;
            | Some s ->
                let op = new_tv I4 in
                let instrs = transl_prim bc es op op.typ s in
                bc.instrs <- bc.instrs @ instrs;
          end;
          transl_decls parent bc tl
      | Call   (Tident.Tpath _ as tpath, es, typ) ->
          let ops = List.map (fun e -> Operand.new_tv (transl_typ e.expr_typ)) es in
          let instrs =
            List.fold_left2 (fun  l op e -> transl_expr bc op e @ l) [] ops es
            @ [new_instr ++ Ir.Call (tpath, ops) ++ bc] in
          bc.instrs <- bc.instrs @ instrs;
          transl_decls parent bc tl
      | Return e                              ->
          let op = new_tv ++ transl_typ e.expr_typ in
          let instrs = transl_expr bc op e in
          bc.instrs <- bc.instrs @ instrs;
          bc
    end

let transl_args args =
  List.map (fun (tpath, typ) ->
    new_operand (Var tpath) (transl_typ typ)) args

let transl_toplevel (f,g,m) bc mod_name = function
  | Typed_ast.Fundef (typ, tpath, args, decls) ->
      let total = Loop_info.total_loop () in
      let entry = Bc.new_bc total in
      ignore (transl_decls total entry decls);
      let func =
        { label_name = Tident.make_label mod_name tpath;
          args = transl_args args;
          entry; loops = []; all_bc =  List.rev !Bc.all_bc } in
      Bc.clear_bc ();
      (func::f, g, m)
  | Global_var (typ, tpath, None) ->
      let mx = { shape = typ_sizeof typ; name = tpath } in
      (f, mx :: g, m)
  | Global_var (typ, tpath, Some e) ->
      let mx = { shape = typ_sizeof typ; name = tpath } in
      let op = new_tv ++ transl_typ e.expr_typ in
      let instrs = transl_expr bc op e in
      (f, mx :: g, instrs :: m)
  | Prim _ -> (f, g, m)

let implementation mod_name intf tops =
  let bc = Bc.new_bc (Loop_info.total_loop ()) in
  Bc.all_bc := [];
  global_intf := intf;
  let funcs, memories, _ =
    List.map (transl_toplevel ([],[],[]) bc mod_name) tops
    |> List.fold_left (fun (a, b, c) (x, y, z) ->
      x @ a, y @ b, z @ c) ([], [], []) in
  { funcs = List.rev funcs ; memories = List.rev memories; }

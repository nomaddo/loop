open Batteries
open Etc
open Ir
open Typed_ast

let array_tbl: (Tident.path, Typed_ast.typ) Hashtbl.t = Hashtbl.create 10

let transl_typ typ =
  match typ with
  | Void -> failwith "void has no values"
  | Int  -> I4
  | Real -> R4
  | Array (typ, e) -> I4
  | Lambda (typs, rettyp) -> I4

let rec transl_expr (intf: Tyenv.intf) (op: Ir.operand) e =
  match e.Typed_ast.expr_core with
  | Typed_ast.Var tpath ->
      let operand =
        {opcore = Ir.Var tpath; typ = transl_typ e.expr_typ} in
      [Ir.Mov (op, operand)]
  | Iconst int ->
      let operand =
        {opcore = Ir.Iconst int; typ = transl_typ e.expr_typ} in
      [Ir.Mov (op, operand)]
  | Rconst str ->
      let operand =
        {opcore = Ir.Rconst str; typ = transl_typ e.expr_typ} in
      [Ir.Mov (op, operand)]
  | Call (Tident.Tident ident, es) ->
      begin match Tyenv.find_prim ident with
      | None ->
          let ops = List.map (fun e -> Ir.new_tv (transl_typ e.expr_typ)) es in
          let _, instrs =
            List.fold_left2 (fun (intf, l) op e ->
              intf, transl_expr intf op e @ l) (intf, []) ops es in
          instrs
      | Some s -> transl_prim intf es op op.typ s
      end
  | _ -> not_implemented_yet ()

and transl_bin intf es op typ f =
  let ops = List.map (fun e -> Ir.new_tv (transl_typ e.expr_typ)) es in
  let instrs = List.fold_left2 (fun acc op e -> transl_expr intf op e :: acc)
      [] ops es |> List.rev |> List.flatten  in
  instrs @ f op ops

and transl_prim intf es op typ s =
  match s with
  | "plus" | "fplus"  -> transl_bin intf es op typ add
  | "minus"| "fminus" -> transl_bin intf es op typ sub
  | "mul"  | "fmul"   -> transl_bin intf es op typ mul
  | "div"  | "fdiv"   -> transl_bin intf es op typ div
  | "gt"   | "fgt"    -> transl_bin intf es op typ mge
  | "lt"   | "flt"    -> transl_bin intf es op typ mlt
  | "ge"   | "fge"    -> transl_bin intf es op typ mge
  | "le"   | "fle"    -> transl_bin intf es op typ mle
  | "eq"   | "feq"    -> transl_bin intf es op typ meq
  | "ne"   | "fne"    -> transl_bin intf es op typ mne
  | "rtoi" | "itor"   -> transl_bin intf es op typ conv
  | _ -> raise Not_found

let rec transl_decls (intf: Tyenv.intf) bc decls =
  match decls with
  | [] -> bc
  | h :: tl -> begin
      match h with
      | Typed_ast.Decl (typ, tpath, None) ->
          transl_decls intf bc tl
      | Decl (typ, tpath, Some e) ->
          let op = Ir.new_var (transl_typ e.expr_typ) in
          let instrs = transl_expr intf op e in
          bc.instrs <- bc.instrs @ instrs;
          transl_decls intf bc tl
      | If (cond, d, dopt) -> begin
          let op = new_var I4 in
          let then_bc = new_bc () in
          let next_bc = new_bc () in
          let instrs, binstr =
            try match cond.expr_core with
              | Call  (Tident.Tident id, es) ->
                assert (Tyenv.find_prim id != None);
                let [x; y] as ops = List.map (fun e -> new_tv (transl_typ e.expr_typ)) es in
                let instrs = List.map2 (transl_expr intf) ops es |> List.flatten in
                let s = Tyenv.find_prim id |> Option.get in
                instrs, begin match s with
                  | "gt" | "fgt" -> [Ir.Bgt (x, y, then_bc)]
                  | "ge" | "fge" -> [Ir.Bge (x, y, then_bc)]
                  | "lt" | "flt" -> [Ir.Blt (x, y, then_bc)]
                  | "le" | "fle" -> [Ir.Bgt (x, y, then_bc)]
                  | "eq" | "feq" -> [Ir.Beq (x, y, then_bc)]
                  | "ne" | "fne" -> [Ir.Bne (x, y, then_bc)]
                  | _ -> assert false
                end
              | _ -> assert false
            with _ -> transl_expr intf op cond, [Ir.Beq (op, true_op, then_bc)] in
          bc.instrs <- bc.instrs @ instrs @ binstr;
          let last_then = transl_decls intf then_bc d in
          ignore (transl_decls intf next_bc tl);
          concat_bc last_then next_bc;
          concat_bc bc next_bc;
          begin match dopt with
            | None -> next_bc
            | Some d' ->
                let else_bc = new_bc () in
                let last_else = transl_decls intf else_bc d' in
                concat_bc bc else_bc;
                concat_bc last_else next_bc;
                next_bc
          end
        end
      | Assign (tpath, e)                     ->
          let op = { opcore = Ir.Var tpath; typ = transl_typ e.expr_typ } in
          let instrs = transl_expr intf op e in
          bc.instrs <- instrs @ bc.instrs;
          transl_decls intf bc tl
      | Astore (tpath, es, e)                 -> not_implemented_yet ()
      | For    (tpath, e1, dir, e2, eopt, ds) -> not_implemented_yet ()
      | While  (e, ds)                        -> not_implemented_yet ()
      | Call   (tpath, es, typ)               -> not_implemented_yet ()
      | Return e                              -> not_implemented_yet ()
    end

let transl_args intf args =
  List.map (fun (tpath, typ) ->
    {Ir.opcore = Ir.Var tpath; typ = transl_typ typ}) args

let transl_fun mod_name intf = function
  | Typed_ast.Fundef (typ, tpath, args, decls) ->
      let entry = new_bc () in
      ignore (transl_decls intf entry decls);
      { Ir.label_name = Tident.make_label mod_name tpath;
        args = transl_args intf args;
        entry; }
  | _ -> not_implemented_yet ()

let implementation mod_name intf tops =
  { funcs = List.map (transl_fun intf mod_name) tops;
    memories = [] }

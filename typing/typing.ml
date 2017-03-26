open Batteries
open Tyenv
open Etc
open Typed_ast

let dummy_const =
  { expr_core = Iconst 0; expr_typ = Typed_ast.Int }

let type_fail expected typ =
  Format.printf "type_fail: %s expected, but %s@."
    (Typed_ast.show_typ expected) (Typed_ast.show_typ typ);
  failwith "type_error"

let rec assert_typ expected typ =
  let open Typed_ast in
  match expected, typ with
  | Int, Int | Real, Real | Void, Void -> ()
  | Lambda (la, a), Lambda (lb, b) ->
      List.iter2 assert_typ la lb;
      assert_typ a b
  | Array _, Array _ -> not_implemented_yet ()
  | _ ->
      type_fail expected typ

and check_aref_args intf typ l =
  Etc.dmsg Flags.dflag (fun () ->
    Format.printf "check_aref_args: %s@." (Typed_ast.show_typ typ));
  match typ with
  | Array (typ, e) ->
      begin match l with
      | [] -> typ
      | hd :: tl ->
          assert_typ Int hd.expr_typ;
          check_aref_args intf typ tl
      end
  | _ ->
      if l = [] then typ
      else failwith "type_array: the number of argument of array must be equal to its dimension "

let rec type_expr intf e =
  match e with
  | Ast.Var pident ->
      let tpath, typ = Tyenv.lookup_ppath intf pident in
      { expr_core = Var tpath; expr_typ = typ }
  | Ast.Iconst i ->
      { expr_core = Iconst i; expr_typ  = Int }
  | Ast.Rconst s ->
      { expr_core = Rconst s; expr_typ  = Real }
  | Ast.Call (ppath, es) ->
      let tes = List.map (type_expr intf) es in
      let typs = List.map (fun e -> e.expr_typ) tes in
      let tpath, rettyp = Tyenv.lookup_ppath intf ppath in
      assert_typ rettyp (Lambda (typs, ret_typ rettyp));
      let expr_core : expr_core = Call (tpath, tes) in
      { expr_core; expr_typ = ret_typ rettyp }
  | Ast.Aref (ppath, es) ->
      let tpath, array_typ = Tyenv.lookup_ppath intf ppath in
      let tes = List.map (type_expr intf) es in
      let expr_typ = check_aref_args intf array_typ tes in
      let expr_core = Aref (tpath, tes) in
      { expr_core; expr_typ }

and type_decl intf rettyp decl =
  match decl with
  | Ast.Decl (typ, ppath, eopt) -> begin
      let new_typ = type_typ intf typ in
      let tpath, new_intf = Tyenv.insert_path intf (Pident.ident ppath) new_typ in
      let eopt = Option.map (type_expr intf) eopt in
      (new_intf, Decl (new_typ, tpath, eopt))
    end
  | Ast.If (e, ds, dsopt) ->
      let te = type_expr intf e in
      assert_typ (te.expr_typ) Int;
      let _then = snd ++ type_decls intf rettyp ds in
      let _else = match dsopt with
        | None -> None
        | Some ds -> Some (snd ++ type_decls intf rettyp ds)  in
      (intf, If (te, _then, _else))
  | Ast.Assign (ppath, e) ->
      let tpath, typ = Tyenv.lookup_ppath intf ppath in
      let e = type_expr intf e in
      assert_typ typ e.expr_typ;
      intf, Assign (tpath, e)
  | Ast.Astore (ppath, es, e) ->
      let tpath, typ = Tyenv.lookup_ppath intf ppath in
      let es = List.map (type_expr intf) es in
      List.iter (fun e -> assert_typ Int e.expr_typ) es;
      let e = type_expr intf e in
      assert_astore typ es;
      intf, Astore (tpath, es, e)
  | Ast.For (ppath, e1, dir, e2, eopt, decls) ->
      let tpath, new_intf = Tyenv.insert_path intf (Pident.ident ppath) Int in
      let e1 = type_expr intf e1 in
      let e2 = type_expr intf e2 in
      let eopt = Option.map (type_expr intf) eopt in
      let _, decls = type_decls new_intf rettyp decls in
      intf, For (tpath, e1, dir, e2, eopt, decls)
  | Ast.While (e, decls) ->
      let e = type_expr intf e in
      let _, decls = type_decls intf rettyp decls in
      intf, While (e, decls)
  | Ast.Call (ppath, es) ->
      let tpath, typ = Tyenv.lookup_ppath intf ppath in
      let es = List.map (type_expr intf) es in
      begin match typ with
      | Lambda (typs, ret) ->
          List.iter2 (fun e t -> assert_typ e.expr_typ t) es typs;
          intf, Call (tpath, es, ret)
      | _ -> failwith ((Pident.show_path ppath) ^ " is not function")
      end
  | Ast.Return None ->
      assert_typ rettyp Void;
      intf, Return None
  | Ast.Return (Some e) ->
      let e = type_expr intf e in
      assert_typ rettyp e.expr_typ;
      intf, Return (Some e)

(*  *)
and assert_astore typ es =
  match typ, es with
  | Int, [] | Real, [] | Lambda _, [] -> ()
  | Array (atyp, _), x::xs ->
      assert_astore atyp xs
  | Array (atyp, _), [] -> failwith "assert_astore: dimension mismatch"
  | _ -> failwith "assert_astore: unexpected"

and type_decls intf rettyp decls =
  List.fold_left (fun (intf, l) d ->
    let intf, newd = type_decl intf rettyp d in
    intf, newd::l) (intf, []) decls
  |> fun (intf, l) -> (intf, List.rev l)

and type_typ intf typ =
  match typ with
  | Ast.Int -> Int
  | Ast.Real -> Real
  | Ast.Void -> Void
  | Ast.Array (t, e) -> begin
      let s = type_typ intf t in
      let u = type_expr intf e in
      begin match u.expr_typ with
      | Int -> ()
      | _ -> failwith "type_typ: array shape must be specified by int expr"
      end;
      Array (s, u)
    end
  | Ast.Lambda (typs, ret) ->
      Lambda (List.map (type_typ intf) typs, type_typ intf ret)

let rec check_args (intf: Tyenv.intf) args =
  let new_intf = List.fold_left (fun intf (s, typ) ->
      Tyenv.insert_path intf s (type_typ intf typ) |> snd)
      intf args in
  let args = List.map (fun (ident, _) ->
      Tyenv.lookup_ppath new_intf ++ Pident.Pident ident) args in
  args, new_intf

let type_top_decl intf = function
  | Ast.Fundef (typ, ppath, args, decls) ->
      let targs, new_intf = check_args intf args in
      let argtyps = List.split targs |> snd in
      let rettyp = type_typ intf typ in
      let funtyp = Lambda (argtyps, rettyp) in
      let tpath, new_intf = insert_path new_intf
          (Pident.ident ppath) funtyp in
      let new_intf, decls = List.fold_left (fun (intf, decls) decl ->
          let new_intf, d = type_decl intf rettyp decl in
          (new_intf, d::decls)) (new_intf, []) decls in
      let new_intf = { new_intf with intf_path = Map.empty } in
      (new_intf, Fundef (funtyp, tpath, targs, List.rev decls))
  | Ast.Global_var (typ, ppath, eopt) ->
      let typ = type_typ intf typ in
      let tpath, new_intf = Tyenv.insert_path ~top:true intf (Pident.ident ppath) typ in
      let eopt = Option.map (fun e -> let te = type_expr intf e in
          assert_typ typ te.expr_typ; te
        ) eopt in
      (new_intf, Global_var (typ, tpath, eopt))
  | Ast.Prim (typ, ppath, s) ->
      let typ = type_typ intf typ in
      let tpath, new_intf = Tyenv.insert_path intf (Pident.ident ppath) typ in
      (* primitiveを探す処理を入れる *)
      (new_intf, Prim (typ, tpath, s))

let implementation mod_name t =
  let intf = Tyenv.create_intf mod_name in
  let intf =
    List.fold_left (fun intf (pident, typ, _) ->
      insert_tident intf pident typ
      |> snd ) intf Tyenv.primitives in
  let intf, decls = List.fold_left (fun (intf, decls) d ->
    let new_intf, d = type_top_decl intf d in
    (new_intf, d::decls)) (intf, []) t in
  (intf, List.rev decls)

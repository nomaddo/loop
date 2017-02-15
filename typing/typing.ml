open Batteries
open Env
open Etc
open Typed_ast

let make_lambda ty args =
  List.map (fun (s, typ) -> typ) args
  |> fun l -> Ast.Lambda (l, ty)

let type_decl: Env.intf -> Ast.decl -> (Env.intf * Typed_ast.decl) = fun intf decl ->
  failwith "typ_decl"

let rec assert_typ : Env.intf -> Typed_ast.typ -> Typed_ast.typ -> unit = fun intf expected typ ->
  let open Typed_ast in
  match expected, typ with
  | Int, Int | Real, Real | Void, Void -> ()
  | Lambda (la, a), Lambda (lb, b) ->
      List.iter2 (assert_typ intf) la lb;
      assert_typ intf a b
  | Array _, Array _ -> not_implemented_yet ()
  | _ ->
      failwith "assert_typ: "

let rec type_expr : Env.intf -> Ast.expr -> Typed_ast.expr = fun intf e ->
  match e with
  | Ast.Var pident ->
      let tpath, typ = Env.lookup_ppath intf pident in
      { expr_core = Var tpath; expr_typ = typ }
  | Ast.Iconst i ->
      { expr_core = Iconst i; expr_typ  = Int }
  | Ast.Rconst s ->
      { expr_core = Rconst s; expr_typ  = Real }
  | Ast.Call (ppath, es) ->
      let tes = List.map (type_expr intf) es in
      let typs = List.map (fun e -> e.expr_typ) tes in
      let tpath, rettyp = Env.lookup_ppath intf ppath in
      assert_typ intf rettyp (Lambda (typs, ret_typ rettyp));
      let expr_core : expr_core = Call (tpath, tes) in
      { expr_core; expr_typ = ret_typ rettyp }
  | Ast.Aref (ppath, es) ->
      let tpath, array_typ = Env.lookup_ppath intf ppath in
      let tes = List.map (type_expr intf) es in
      let expr_typ = check_aref_args intf array_typ tes in
      let expr_core = Aref (tpath, tes) in
      { expr_core; expr_typ }

and check_aref_args : Env.intf -> Typed_ast.typ -> Typed_ast.expr list -> Typed_ast.typ = fun intf typ l ->
  match typ with
  | Array (typ, e) ->
      begin match l with
      | [] -> typ
      | hd :: tl ->
          assert_typ intf Int hd.expr_typ;
          check_aref_args intf typ tl
      end
  | _ ->
      if l = [] then typ
      else failwith "type_array: the number of argument of array must be equal to its dimension "

and type_typ : Env.intf -> Ast.typ -> Typed_ast.typ = fun intf typ ->
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

let rec check_args (intf: Env.intf) args =
  let a, b = List.span (function (s, typ) -> match typ with
      | Ast.Int | Ast.Real | Ast.Void | Ast.Lambda _ -> true | _ -> false) args in
  let new_intf = List.fold_left (fun intf (s, typ) ->
      Env.insert_path intf s (type_typ intf typ)) intf (a @ b) in
  let args = List.map (fun (ident, _) ->
      Env.lookup_ppath new_intf ++ Pident.Pident ident) (a @ b) in
  args, new_intf

let type_top_decl intf = function
  | Ast.Fundef (typ, ppath, args, decls) ->
      let targs, new_intf = check_args intf args in
      let new_intf = insert_path new_intf
          (Pident.ident ppath) (type_typ new_intf ++ make_lambda typ args) in
      let fpath, rettyp = Env.lookup_ppath new_intf ppath in
      let _, decls = List.fold_left (fun (intf, decls) decl ->
          let new_intf, d = type_decl intf decl in
          (new_intf, d::decls)) (new_intf, []) decls in
      (new_intf, Fundef (rettyp, fpath, targs, decls))
  | _ -> assert false           (* fix me *)

let implementation mod_name t =
  let intf = Env.create_intf mod_name in
  List.fold_left (fun (intf, decls) d ->
    let new_intf, d = type_top_decl intf d in
    (new_intf, d::decls)) (intf, []) t

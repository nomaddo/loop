open Batteries
open Env
open Etc

let make_lambda ty args =
  List.map (fun (s, typ) -> typ) args
  |> fun l -> Ast.Lambda (l, ty)

let type_decl: Env.intf -> Ast.decl -> (Env.intf * Typed_ast.decl) = fun intf decl ->
  failwith "typ_decl"

let type_expr : Env.intf -> Ast.expr -> Typed_ast.expr = fun intf e ->
  match e with
  | Ast.Var pident ->
      let tpath, typ = Env.lookup_ppath intf pident in
      { Typed_ast.expr_core = Typed_ast.Var tpath; expr_typ = typ }
  | _ -> assert false

let rec check_args (intf: Env.intf) args =
  let a, b = List.span (function (s, typ) -> match typ with
      | Ast.Int | Ast.Real | Ast.Void | Ast.Lambda _ -> true | _ -> false) args in
  let new_intf = List.fold_left (fun intf (s, typ) ->
      Env.insert_path intf s (type_typ intf typ)) intf (a @ b) in
  let args = List.map (fun (ident, _) ->
      Env.lookup_ppath new_intf ++ Pident.Pident ident) (a @ b) in
  args, new_intf

and type_typ : Env.intf -> Ast.typ -> Typed_ast.typ = fun intf typ ->
  match typ with
  | Ast.Int -> Typed_ast.Int
  | Ast.Real -> Typed_ast.Real
  | Ast.Void -> Typed_ast.Void
  | Ast.Array (t, e) -> begin
      let s = type_typ intf t in
      let u = type_expr intf e in
      begin match u.Typed_ast.expr_typ with
      | Typed_ast.Int -> ()
      | _ -> failwith "type_typ: array shape must be specified by int expr"
      end;
      Typed_ast.Array (s, u)
    end
  | Ast.Lambda (typs, ret) ->
      Typed_ast.Lambda (List.map (type_typ intf) typs, type_typ intf ret)


let type_top_decl intf = function
  | Ast.Fundef (typ, ppath, args, decls) ->
      let targs, new_intf = check_args intf args in
      let new_intf = insert_path new_intf
          (Pident.ident ppath) (type_typ new_intf ++ make_lambda typ args) in
      let fpath, rettyp = Env.lookup_ppath new_intf ppath in
      let _, decls = List.fold_left (fun (intf, decls) decl ->
          let new_intf, d = type_decl intf decl in
          (new_intf, d::decls)) (new_intf, []) decls in
      (new_intf, Typed_ast.Fundef (rettyp, fpath, targs, decls))
  | _ -> assert false           (* fix me *)

let implementation mod_name t =
  let intf = Env.create_intf mod_name in
  List.fold_left (fun (intf, decls) d ->
    let new_intf, d = type_top_decl intf d in
    (new_intf, d::decls)) (intf, []) t

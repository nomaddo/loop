open Batteries
open Etc

type intf = { intf_name : Tident.path;
              intf_mod  : (Pident.ident, Tident.path * intf) Map.t;
              intf_mod_ : (Tident.ident, intf) Map.t;
              intf_path : (Pident.ident, Tident.path * Typed_ast.typ) Map.t ;
              intf_path_: (Tident.ident, Typed_ast.typ) Map.t
}

let rec recreate_intf intf =
  let intf_mod  =
    Map.foldi (fun p (t, intf) env ->
        Map.add p (Tident.recreate_path t, intf) env) Map.empty intf.intf_mod in
  let intf_path =
    Map.foldi (fun p (t, intf) env ->
        Map.add p (Tident.recreate_path t, intf) env) Map.empty intf.intf_path in
  { intf_name = Tident.recreate_path intf.intf_name;
    intf_mod; intf_path;
    intf_mod_  = Map.fold (fun (ppath, typ) intf ->
                     Map.add ++ Tident.ident ppath ++ typ ++ intf) intf_mod Map.empty ;
    intf_path_ = Map.fold (fun (ppath, intf') intf ->
                     Map.add ++ Tident.ident ppath ++ intf' ++ intf) intf_path Map.empty;
  }

let read_mods () =
  let files =
    !Flags.search_path
    |> List.fold_left (fun acc s ->
           Enum.append acc @@ Sys.files_of s ) ++ Enum.empty ()
    |> Enum.filter (fun s -> Filename.check_suffix s "mod") in
  Enum.map (fun s -> open_in s |> Marshal.input) files
  |> Enum.fold (fun (a, b) intf ->
         let new_intf = recreate_intf intf in
         let ppath    = new_intf.intf_name in
         (Map.add (Tident.ident ppath) new_intf a,
          Map.add (Tident.ident ppath).Tident.name (ppath, new_intf) b))
                    (Map.empty, Map.empty)

let create_intf mod_name =
  let intf_mod_, intf_mod = read_mods () in
  { intf_name = Tident.path mod_name;
    intf_mod; intf_mod_;
    intf_path = Map.empty;
    intf_path_ = Map.empty;
 }

let rec lookup_ppath intf pident =
  match pident with
  | Pident.Pident ident ->
     Map.find ident intf.intf_path
  | Pident.Ppath (ident, path) ->
     let tpath_mod, lintf = Map.find ident intf.intf_mod in
     let tpath_name, typ  = lookup_ppath lintf path in
     (Tident.Tpath (Tident.ident tpath_mod, tpath_name), typ)

let insert_path intf s ty =
  try lookup_ppath intf (Pident.Pident s);
      failwith ("insert_path: same var name is introduced " ^ s)
  with Not_found ->
    let tpath = Tident.path s in
    { intf with intf_path  = Map.add s (tpath, ty) intf.intf_path ;
                intf_path_ = Map.add (Tident.ident tpath) ty intf.intf_path_ }

let insert_path_for_args intf l =
  List.fold_left (fun intf (s, ty) -> insert_path intf s ty) intf l

let mkhash l =
  let h = Hashtbl.create 100 in
  List.iter (fun (id, ty) -> Hashtbl.add h id ty) l;
  h

let primitives: (string, Ast.typ) Hashtbl.t =
  let open Ast in
  mkhash [
      "+",  Lambda ([Int; Int], Int);
      "-",  Lambda ([Int; Int], Int);
      "*",  Lambda ([Int; Int], Int);
      "/",  Lambda ([Int; Int], Int);
      "+",  Lambda ([Real; Real], Real);
      "-",  Lambda ([Real; Real], Real);
      "*",  Lambda ([Real; Real], Real);
      "/",  Lambda ([Real; Real], Real);
      "<",  Lambda ([Int; Int], Int);
      ">",  Lambda ([Int; Int], Int);
      "<=", Lambda ([Int; Int], Int);
      ">=", Lambda ([Int; Int], Int);
      "==", Lambda ([Int; Int], Int);
      "!=", Lambda ([Int; Int], Int);
      "<",  Lambda ([Real; Real], Int);
      ">",  Lambda ([Real; Real], Int);
      "<=", Lambda ([Real; Real], Int);
      ">=", Lambda ([Real; Real], Int);
      "==", Lambda ([Real; Real], Int);
      "!=", Lambda ([Real; Real], Int);
    ]

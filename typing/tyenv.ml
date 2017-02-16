open Batteries
open Etc

type intf = { intf_name : Tident.path;
              intf_mod  : (Pident.ident, Tident.path * intf) Map.t;
              intf_mod_ : (Tident.ident, intf) Map.t;
              intf_path : (Pident.ident, Tident.path * Typed_ast.typ) Map.t ;
              intf_path_: (Tident.ident, Typed_ast.typ) Map.t
            }

let show_intf intf =
  Map.iter (fun pident (tpath, typ) ->
    Format.printf "%s: (%s, %s)@."
    ++ Pident.show_ident pident
    ++ Tident.show_path tpath
    ++ Typed_ast.show_typ typ) intf.intf_path

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
      intf_path_ = Map.empty; }

let rec lookup_ppath intf ppath =
  (* begin if !Flags.dflag then show_intf intf end; *)
  match ppath with
  | Pident.Pident ident ->
      begin try Map.find ident intf.intf_path with
      | exn ->
          if !Flags.dflag then
            Format.printf "lookup_ppath: %s not found@." (Pident.show_path ppath);
          raise exn
      end
  | Pident.Ppath (ident, path) ->
     let tpath_mod, lintf = Map.find ident intf.intf_mod in
     let tpath_name, typ  = lookup_ppath lintf path in
     (Tident.Tpath (Tident.ident tpath_mod, tpath_name), typ)

let rec mem_ppath intf ppath =
  match ppath with
  | Pident.Pident ident ->
      Map.mem ident intf.intf_path
  | Pident.Ppath (ident, path) ->
      if Map.mem ident intf.intf_mod then
        let _, lintf = Map.find ident intf.intf_mod in
        mem_ppath lintf path
      else false

let rec find_path intf tpath =
  match tpath with
  | Tident.Tident id -> begin try Map.find id intf.intf_path_ with
      | exn ->
          Format.printf "find_path: %s not found@." ++ Tident.show_path tpath;
          raise exn
    end
  | Tident.Tpath (id, tpath) ->
      find_path (Map.find id intf.intf_mod_) tpath

let insert_path intf s ty =
  if mem_ppath intf (Pident.Pident s) then
      failwith ("insert_path: same var name is introduced " ^ s)
  else
    let tpath = Tident.path s in
    tpath, { intf with intf_path  = Map.add s (tpath, ty) intf.intf_path ;
                       intf_path_ = Map.add (Tident.ident tpath) ty intf.intf_path_ }

let insert_path_for_args intf l =
  List.fold_left (fun intf (s, ty) -> insert_path intf s ty |> snd) intf l

let mkhash l =
  let h = Hashtbl.create 100 in
  List.iter (fun (id, ty) -> Hashtbl.add h id ty) l;
  h

let primitives =
  let open Ast in
  [
    "+",  Lambda ([Int; Int], Int);
    "-",  Lambda ([Int; Int], Int);
    "*",  Lambda ([Int; Int], Int);
    "/",  Lambda ([Int; Int], Int);
    "+.",  Lambda ([Real; Real], Real);
    "-.",  Lambda ([Real; Real], Real);
    "*.",  Lambda ([Real; Real], Real);
    "/.",  Lambda ([Real; Real], Real);
    "<",  Lambda ([Int; Int], Int);
    ">",  Lambda ([Int; Int], Int);
    "<=", Lambda ([Int; Int], Int);
    ">=", Lambda ([Int; Int], Int);
    "==", Lambda ([Int; Int], Int);
    "!=", Lambda ([Int; Int], Int);
    "<.",  Lambda ([Real; Real], Int);
    ">.",  Lambda ([Real; Real], Int);
    "<=.", Lambda ([Real; Real], Int);
    ">=.", Lambda ([Real; Real], Int);
    "==.", Lambda ([Real; Real], Int);
    "!=.", Lambda ([Real; Real], Int);
    "rtoi", Lambda ([Real], Int);
    "itor", Lambda ([Int], Real);
  ]

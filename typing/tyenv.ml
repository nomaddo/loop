open Batteries
open Etc

type intf = { intf_name : Tident.path;
              intf_mod  : (Pident.ident, Tident.path * intf) Map.t;
              intf_mod_ : (Tident.ident, intf) Map.t;
              intf_path : (Pident.ident, Tident.path * Typed_ast.typ) Map.t ;
              intf_path_: (Tident.ident, Typed_ast.typ * bool) Map.t
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
        Map.add ++ Tident.ident ppath ++ (intf', false) ++ intf) intf_path Map.empty;
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

let rec mem_ppath intf ppath =
  match ppath with
  | Pident.Pident ident ->
      Map.mem ident intf.intf_path

let rec find_path intf tpath =
  match tpath with
  | Tident.Tident id -> begin try Map.find id intf.intf_path_ |> fst with
      | exn ->
          Format.printf "find_path: %s not found@." ++ Tident.show_path tpath;
          raise exn
    end


let rec find_path_is_toplevel intf tpath =
  match tpath with
  | Tident.Tident id -> begin try Map.find id intf.intf_path_ |> snd with
      | exn ->
          Format.printf "find_path: %s not found@." ++ Tident.show_path tpath;
          raise exn
    end

let insert_path ?(top=false) intf s ty =
  if mem_ppath intf (Pident.Pident s) then
      failwith ("insert_path: same var name is introduced " ^ s)
  else
    let tpath = Tident.path s in
    tpath, { intf with intf_path  = Map.add s (tpath, ty) intf.intf_path ;
                       intf_path_ = Map.add (Tident.ident tpath) (ty, top) intf.intf_path_ }

let insert_tident ?(top=false) intf (tident: Tident.ident) ty =
  let tpath = Tident.Tident tident in
  let s = tident.Tident.name in
    tpath, { intf with intf_path  = Map.add s (tpath, ty) intf.intf_path ;
                       intf_path_ = Map.add (Tident.ident tpath) (ty, top) intf.intf_path_ }

let insert_path_for_args intf l =
  List.fold_left (fun intf (s, ty) -> insert_path intf s ty |> snd) intf l

let mkhash l =
  let h = Hashtbl.create 100 in
  List.iter (fun (id, ty) -> Hashtbl.add h id ty) l;
  h

let primitives =
  let open Typed_ast in
  [
 "+",  Lambda ([Int; Int], Int), "plus";
 "-",  Lambda ([Int; Int], Int), "minus";
 "*",  Lambda ([Int; Int], Int), "mul";
 "/",  Lambda ([Int; Int], Int), "div";
 "+.",  Lambda ([Real; Real], Real), "fplus";
 "-.",  Lambda ([Real; Real], Real), "fminus";
 "*.",  Lambda ([Real; Real], Real), "fmul";
 "/.",  Lambda ([Real; Real], Real), "fdiv";
 "<",  Lambda ([Int; Int], Int), "gt";
 ">",  Lambda ([Int; Int], Int), "lt";
 "<=", Lambda ([Int; Int], Int), "ge";
 ">=", Lambda ([Int; Int], Int), "le";
 "==", Lambda ([Int; Int], Int), "eq";
 "!=", Lambda ([Int; Int], Int), "ne";
 "<.",  Lambda ([Real; Real], Int), "fgt";
 ">.",  Lambda ([Real; Real], Int), "flt";
 "<=.", Lambda ([Real; Real], Int), "fge";
 ">=.", Lambda ([Real; Real], Int), "fle";
 "==.", Lambda ([Real; Real], Int), "feq";
 "!=.", Lambda ([Real; Real], Int), "fne";
 "rtoi", Lambda ([Real], Int), "rtoi";
 "itor", Lambda ([Int], Real), "itor";
  ]
  |> List.map (fun (s, typ ,name) -> (Tident.make_ident s, typ, name))

let find_prim tident =
  try List.find (function (id, _, s) -> tident = id) primitives
      |> function (_, _, s) -> Some s
  with Not_found -> None

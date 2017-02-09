open Batteries
open Env
open Etc

let make_lambda ty args =
  List.map (fun (typ, s) -> typ) args
  |> fun l -> Ast.Lambda (l, ty)

let read_mods () =
  !Flags.search_path
  |> List.fold_left (fun acc s ->
    Enum.append acc @@ Sys.files_of s ) ++ Enum.empty ()
  |> Enum.filter (fun s -> Filename.check_suffix s "mod")
  |> Enum.fold (fun acc file ->
    Map.add ++ Filename.basename file
    ++ (Tident.path (Filename.basename file), lazy (Marshal.input (open_in file))) ++ acc) Map.empty

let create_intf mod_name =
  let outers = read_mods () in
  { Env.intf_name = Tident.path mod_name;
    intf_mod = outers;
    intf_path = Map.empty; }

let type_top_decl intf d =
  assert false

let implementation mod_name t =
  let intf = create_intf mod_name in
  List.fold_left (fun intf d -> type_top_decl intf d) [] t

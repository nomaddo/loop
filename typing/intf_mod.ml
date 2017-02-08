open Tident
open Batteries

type intf_mod =
  (Tident.path * Ast.typ) list

let rec mangling = function
  | Tident ident -> Tident { ident with id = Btypes.gen_sym () }
  | Tpath (ident, path) -> Tpath ({ident with id = Btypes.gen_sym ()}, mangling path)

let search_file path =
  let file = List.find (fun s ->
      Sys.file_exists (s ^ path.name)) !Flags.search_path in
  file ^ path.name

let load_mod path =
  try
    let file = search_file path in
    let inc  = open_in file in
    let (intf_mod : intf_mod)  = Marshal.input inc  in
    List.fold_left (fun acc (p, ty) -> (mangling p, ty) :: acc) [] intf_mod
    |> List.rev
  with
  | Not_found -> begin
      exit 1
    end

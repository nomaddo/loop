open Batteries

type intf = { intf_name: Tident.path;
              intf_mod:  (Pident.ident, Tident.path * intf lazy_t) Map.t;
              intf_path: (Pident.ident, Tident.path * Ast.typ) Map.t }

let rec recreate_intf intf =
  { intf_name = Tident.recreate_path intf.intf_name;
    intf_mod  = Map.foldi (fun p (t, intf) env ->
                    Map.add p (Tident.recreate_path t, intf) env) Map.empty intf.intf_mod ;
    intf_path = Map.foldi (fun p (t, intf) env ->
                    Map.add p (Tident.recreate_path t, intf) env) Map.empty intf.intf_path ;
  }

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

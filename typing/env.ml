module TypeEnv = Map.Make (struct
    type t = Pident.path
    let compare  = compare
  end)

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

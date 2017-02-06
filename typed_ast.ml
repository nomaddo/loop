module TypeEnv = Map.Make (struct
    type t = Pident.path
    let compare  = compare
  end)

let primitives = [1;2;3]

let a = TypeEnv.empty

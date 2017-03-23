let print_ast = ref false
let print_tast = ref false
let print_ila = ref false
let print_ilb = ref true
let search_path : string list ref = ref []

let dflag = ref false

let opt = ref true

let show_valid_vars = ref false

let dmsg f =
  if !dflag then f () else ()

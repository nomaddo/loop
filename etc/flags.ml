let print_ast = ref false
let print_tast = ref false
let print_ila = ref false
let print_ilb = ref false
let search_path : string list ref = ref []

let dflag = ref false

let opt = ref true

let toilb = ref false
let sa    = ref false
let ra    = ref false

let show_stack = ref false
let show_dyn_arrays = ref false
let show_valid_vars = ref false

let dmsg f =
  if !dflag then f () else ()

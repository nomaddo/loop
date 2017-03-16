open Batteries
open Operand
open Ilb

let rec set_control_flow bc =
  let open Ir in
  let rec set_succs bc =
    if bc.traverse_attr = 1 then () else begin
      bc.traverse_attr <- 1;
      let bopt = find_branch_instr bc |> Option.map get_distination in
      bc.succs <- Option.map_default (fun x -> [x]) [] bopt @
          Option.map_default (fun x -> [x]) [] bc.next ;
      List.iter set_succs bc.succs end in

  let set_prevs bc =
    let hash = Hashtbl.create 100 in
    let rec gather_info bc =
      if bc.traverse_attr != 0 then begin
        bc.traverse_attr <- 0;
        List.iter (fun x -> Hashtbl.add hash x bc) bc.succs;
        List.iter gather_info bc.succs
      end in
    gather_info bc;
    Hashtbl.iter (fun succ prev -> succ.preds <- prev :: succ.preds) hash in

  set_succs bc;
  set_prevs bc

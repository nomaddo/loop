open Batteries
open Ir
open Ir.Instr

let rec fold num f acc bc =
  if bc.traverse_attr = num then acc else begin
    bc.traverse_attr <- num;
    let acc = f acc bc in
    List.fold_left (fold num f) acc bc.succs
  end

let rec traverse_if f cond acc (bc: 'a basic_block) =
  if cond acc bc then begin
    Format.printf "traverse_if: visit block_%d@." bc.id;
    f acc bc;
    List.iter (traverse_if f cond acc) bc.succs
  end

let rec iter num f (bc: 'a basic_block) =
  if bc.traverse_attr = num then () else begin
    (* Format.printf "iter: visit block_%d@." bc.id; *)
    f bc;
    bc.traverse_attr <- num;
    List.iter (iter num f) bc.succs
  end

let reset_traverse_attr bc = iter (-100) (fun _ -> ()) bc

let rec find_path (x: 'a basic_block) (y: 'a basic_block) =
  let mark bc = bc.traverse_attr <- bc.traverse_attr + 10000 in
  let unmark bc = bc.traverse_attr <- bc.traverse_attr - 10000 in
  let marked bc = x.traverse_attr > 10000 in
  Format.printf "find_path: block_%d to block_%d@." x.id y.id;
  if x == y then begin unmark x; [[x]] end else begin
    if marked x then begin unmark x; raise Not_found end else begin
      mark x;
      let ans =
        if x.succs = [] then begin unmark x; raise Not_found end else
          List.map (fun node ->
            try find_path node y |> List.filter (fun l -> l != []) |> List.map (fun l -> x :: l)
            with Not_found -> [[]]) x.succs
          |> List.flatten in
      unmark x;
      ans
    end
  end

let rec dump_path (l: 'a basic_block list) =
  Format.printf "dump_path:";
  List.iter (fun (bc: 'a basic_block) -> Format.printf "%d " bc.id) l;
  Format.printf "@."

let rec set_control_flow bc =
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

  let reset_loop bc =
    iter (-100) (fun bc ->
      bc.loop <- Loop_info.dummy_loop) bc in

  let set_total_loop loop bc =
    iter 0 (fun bc ->
      if Loop_info.is_dummy_loop bc.loop
      then begin
        bc.loop <- loop;
      end) bc in

  (* succs, predsがセットされたことを前提にする *)
  let rec set_loop_info bc =
    (* bcから与えられたtermまでを同じループとみなしてセットする *)
    let set_info term loop succ =
      let paths = find_path succ term in
      List.iter dump_path paths;
      List.iter (fun l ->
        List.iter (fun node ->
          if Loop_info.is_dummy_loop (node.loop) then node.loop <- loop
          else begin
            match node.loop.parent with
            | None -> node.loop.parent <- Some loop
            | Some _ -> () end) l) paths in
    let rec search set bc =
      List.iter (fun succ -> if Set.mem succ set then
          let loop = Loop_info.new_loop () in
          loop.entrance  <- Some succ;
          loop.terminate <- Some bc;
          succ.loop <- loop;
          bc.loop <- loop;
      ) bc.succs;
      List.iter (fun succ -> if Set.mem succ set
        then () else search (Set.add succ set) succ) bc.succs
    in
    iter 100 (search (Set.singleton bc)) bc
  in
  set_succs bc;
  set_prevs bc;
  reset_loop bc;
  set_loop_info bc;
  set_total_loop (Loop_info.total_loop ()) bc

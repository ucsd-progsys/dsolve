module E = Map.Make(String)

include E

let maplist f env =
  fold (fun k v r -> (f k v)::r) env []

let filterlist f env =
  fold (fun k v r -> if f k v then v::r else r) env []

let mapfilter f env =
  fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let addn items env =
  List.fold_left (fun e (k, v) -> add k v e) env items

let pprint pprint_range env =
  Misc.join (maplist (fun x t -> Printf.sprintf "%s -> %s" x (pprint_range t)) env) "; "

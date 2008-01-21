(* A small environment module; provided so we (hopefully) won't have to screw
   with OCaml's env. *)

module E = Map.Make(Common.ComparablePath)

include E

let maplist f env =
  fold (fun k v r -> (f k v)::r) env []

let filterlist f env =
  fold (fun k v r -> if f k v then v::r else r) env []

let mapfilter f env =
  fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let addn items env =
  List.fold_left (fun e (k, v) -> add k v e) env items

let pprint pprint_range ppf env =
  iter (fun x t -> Format.fprintf ppf "@[%s@ ->@ %a;@\n@]" (Path.unique_name x) pprint_range t) env 

let cardinality e = fold (fun _ _ c -> c + 1) e 0

let compare e1 e2 = Pervasives.compare (cardinality e1) (cardinality e2)

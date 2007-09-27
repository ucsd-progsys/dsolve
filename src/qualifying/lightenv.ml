(* A small environment module; provided so we (hopefully) won't have to screw
   with OCaml's env. *)

module ComparableIdent = struct
  type t = Ident.t
  let compare = compare
end

module E = Map.Make(ComparableIdent)

include E

let maplist f env =
  fold (fun k v r -> (f k v)::r) env []

let filterlist f env =
  fold (fun k v r -> if f k v then v::r else r) env []

let mapfilter f env =
  fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let find_default k default m =
  try
    find k m
  with Not_found -> default

let addn items env =
  List.fold_left (fun e (k, v) -> add k v e) env items
(*
let pprint pprint_range env =
  Misc.join (maplist (fun x t -> Printf.sprintf "%s -> %s" x (pprint_range t)) env) "; "
*)

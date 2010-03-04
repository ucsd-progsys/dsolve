(* DSOLVE -no-recrefs -hide-rectypes *)

let show x = ()

let nonnull = function
  | x::xs -> 1 
  | []    -> 0 

(**********************************************************************)
(* STEP 1: MAP (expand each input into key-value list) ****************)
(**********************************************************************)

let rec expand f xs = 
  match xs with
  | []      -> []
  | x::xs'  -> (f x) @ (expand f xs')

(**********************************************************************)
(* STEP 2: GROUP (cluster key-value pairs by key) *********************)
(**********************************************************************)

let group kvs = 
  let t = Hashtbl.create 37 in
  List.iter (fun x -> 
    let (k,v) = x in
    let vs    = if Hashtbl.mem t k then Hashtbl.find t k else [] in
    Hashtbl.add t k (v::vs)
  ) kvs;
  t

(**********************************************************************)
(* STEP 3: REDUCE (fold over values associated with each key **********)
(**********************************************************************)

let collapse f t =
  let groups = Hashtbl.fold (fun k vs acc -> (k, vs)::acc) t [] in 
  List.map (fun g -> match g with 
    | (k, (v::vs)) -> (k, List.fold_left f v vs)
    | (k, [])      -> assert (0 = 1); assert false
  ) groups

(**********************************************************************)
(* Putting it all together ********************************************)
(**********************************************************************)

let map_reduce xs mapper reducer = 
  let kvs = expand mapper xs in
  let t   = group kvs in
  let rs  = collapse reducer t in
  rs


(* DSOLVE -no-recrefs -hide-rectypes *)

let show x = ()

let nonnull = function
  | x::xs -> true 
  | []    -> false 

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
    | (k, [])      -> assert (0 = 1); assert false      (* NON-NULL *)
  ) groups

(**********************************************************************)
(* Putting it all together ********************************************)
(**********************************************************************)

let map_reduce xs mapper reducer = 
  let kvs = expand mapper xs in
  let t   = group kvs in
  let rs  = collapse reducer t in
  rs

(**********************************************************************)
(* K-MEANS via Map-Reduce *********************************************)
(**********************************************************************)

let rec repeatn n f =
  if n <= 0 then () else 
    let _ = f () in 
    repeatn (n-1) f

let min_index a =
  let rec loop i j = 
    if i >= Array.length a then j else
      loop (i+1) (if a.(i) < a.(j) then i else j)
  in loop 0 0

let nearest dist ya x  =
  let da = Array.map (dist x) ya in
  let xi = min_index da in 
  [xi, (x, 1)]

let recenter plus p1 p2 = 
  let (x1,sz1) = p1 in
  let (x2,sz2) = p2 in 
  (plus x1 x2, sz1 + sz2)

let kmeans n dist plus div xs ya =
  if Array.length ya <= 0 then () else
    repeatn n begin fun u ->   
      let ys' = map_reduce xs (nearest dist ya) (recenter plus) in
      List.iter begin fun p ->
        let i, (x, sz) = p in
        assert (0 < sz);                          (* DIV BY ZERO  *)
        ya.(i) <- div x sz                        (* ARRAY BOUNDS *)
      end ys' 
    end

(* NICER SYNTAX 
let kmeans n dist plus div xs ya =
  repeatn n begin fun () ->   
    map_reduce xs (nearest dist ya) (recenter plus)
    |> List.map  (fun (i, (x,sz)) -> assert(0 < sz); (i, div x sz)) 
    |> List.iter (fun (i, y') -> ya.(i) <- y')                     
  end
*)

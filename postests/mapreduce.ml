(* DSOLVE -no-recrefs -hide-rectypes *)

(**********************************************************************)
(* Some Generic Helper Functions **************************************)
(**********************************************************************)

let (<~) = (:=)
let (|>) = fun x f -> f x
let show = fun x -> ()

let rec repeatn n f =
  if n <= 0 then () else 
    let _ = f () in 
    repeatn (n-1) f

let rec ffor i j f = 
  if i < j then begin
    f i;
    ffor (i+1) j f
  end

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
  List.iter begin fun x -> 
    let (k,v) = x in
    let vs    = if Hashtbl.mem t k then Hashtbl.find t k else [] in
    Hashtbl.add t k (v::vs)
  end kvs;
  t

(**********************************************************************)
(* STEP 3: REDUCE (fold over values associated with each key **********)
(**********************************************************************)

let collapse f t = 
  Hashtbl.fold begin fun k vs acc ->
    match vs with
    | v::vs' -> (k, List.fold_left f v vs')::acc
    | []     -> assert (0=1); assert false      (* NON-NULL *)
  end t []

(**********************************************************************)
(* Putting it all together ********************************************)
(**********************************************************************)

let map_reduce mapper reducer xs = 
  xs |> expand mapper
     |> group 
     |> collapse reducer

(**********************************************************************)
(* CLIENT 1: Matrix Multiplication via Map-Reduce  ********************)
(* Multiply m x n dimensional matrix (a) ******************************)
(* With         n dimensional vector (x) ******************************)
(* to get       m dimensional vector (b) ******************************)
(**********************************************************************)

let rec range i j = 
  if i >= j then [] else i::(range (i+1) j)

let row_multiply n a x i =
  let _ = assert (Array.length a.(i) = n) in
  range 0 n 
  |> List.map (fun j -> (i, a.(i).(j) *. x.(j)))

let matrix_multiply m n a x =
  assert (Array.length a = m && Array.length x = n && 0 < m); 
  let b = Array.make m 0.0 in
  range 0 m
  |> map_reduce (row_multiply n a x) (+.) 
  |> List.iter (fun p -> let i, v = p in b.(i) <- v)
  |> fun _ -> b

(**********************************************************************)
(* CLIENT 2: K-MEANS via Map-Reduce ***********************************)
(**********************************************************************)

let min_index a =
  let min = ref 0 in
  ffor 0 (Array.length a) begin fun i ->
    if a.(i) < a.(!min) then 
      min <~ i
  end;
  !min

let nearest dist ctra x  =
  let da = Array.map (dist x) ctra in
  [min_index da, (x, 1)]

let recenter plus p1 p2 = 
  let (x1, sz1) = p1 in
  let (x2, sz2) = p2 in 
  (plus x1 x2, sz1 + sz2)

let kmeans n dist plus div (xs : 'a list) (ctra : 'a array) =
  assert (Array.length ctra > 0); 
  ffor 0 n begin fun _ ->
    map_reduce (nearest dist ctra) (recenter plus) xs 
    |> List.iter begin fun p -> 
         let i, (x, sz) =  p in
         let ci'        =  div x sz in  (* DIVIDE BY ZERO  *)
         ctra.(i)       <- ci'          (* ARRAY BOUNDS *)
       end 
  end

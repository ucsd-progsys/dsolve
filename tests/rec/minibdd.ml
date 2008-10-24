type view = Zero | One | Node of int * view (*low*) * view (*high*)

type bdd  = int * view 

type operator =
  | Op_and | Op_or | Op_imp
  | Op_any of (bool -> bool -> bool)

let var b = match b with
  | Zero | One -> max_int 
  | Node (v, _, _) -> v

let utag b = match b with
  | Zero        -> 0
  | One         -> 1 
  | Node (v,_,_)-> 2 

let low b = match b with
  | Zero | One -> assert (0 = 1)
  | Node (_, l, _) -> l

let high b = match b with
  | Zero | One -> assert (0 = 1) 
  | Node (_, _, h) -> h

let mk x low high =
  if low = high then low else (0, Node (x, low, high)) (* hashcons_node v low high *)

let cache_default_size = 7001

let mk_not x = 
  let cache = H1.create cache_default_size in
  let rec mk_not_rec x = 
    try
      H1.find cache x
    with Not_found -> 
      let res = match x with
	| Zero -> one
	| One -> zero
	| Node (v, l, h) -> mk v (mk_not_rec l) (mk_not_rec h)
      in
      H1.add cache x res;
      res
  in
  mk_not_rec x

let apply_op op b1 b2 = match op with
  | Op_and -> b1 && b2
  | Op_or  -> b1 || b2
  | Op_imp -> (not b1) || b2
  | Op_any f -> f b1 b2

(* val gapply: operator -> bdd -> bdd -> bdd *)
let gapply op =
  let op_z_z = of_bool (apply_op op false false) in
  let op_z_o = of_bool (apply_op op false true) in
  let op_o_z = of_bool (apply_op op true false) in
  let op_o_o = of_bool (apply_op op true true) in
  fun b1 b2 ->
    let cache = H2.create cache_default_size in
    let rec app ((u1,u2) as u12) =
      match op with
	| Op_and ->
	    if u1 = u2 then 
	      u1
	    else if u1 = zero || u2 = zero then
	      zero
	    else if u1 = one then
	      u2
	    else if u2 = one then
	      u1 
	    else
	      app_gen u12
	| Op_or ->
            if u1 = u2 then
	      u1
	    else if u1 = one || u2 = one then
	      one
	    else if u1 = zero then
	      u2
	    else if u2 = zero then
	      u1
	    else 
	      app_gen u12
	| Op_imp -> 
	    if u1 = zero then
	      one
	    else if u1 = one then
	      u2
	    else if u2 = one then
	      one
	    else
	      app_gen u12
 	| Op_any _ ->
	    app_gen u12
    and app_gen ((u1,u2) as u12) =
      match (u1, u2) with
	| Zero, Zero -> op_z_z
	| Zero, One  -> op_z_o
	| One,  Zero -> op_o_z
	| One,  One  -> op_o_o
	| _ ->
	    try
	      H2.find cache u12
	    with Not_found -> 
	      let res = 
		let v1 = var u1 in
		let v2 = var u2 in
		if v1 = v2 then
		  mk v1 (app (low u1, low u2)) (app (high u1, high u2))
		else if v1 < v2 then
		  mk v1 (app (low u1, u2)) (app (high u1, u2))
		else (* v1 > v2 *)
		  mk v2 (app (u1, low u2)) (app (u1, high u2))
	      in
	      H2.add cache u12 res;
	      res
    in
    app (b1, b2)

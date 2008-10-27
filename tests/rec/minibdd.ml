let show x = x

type bdd = Zero | One | Node of int * bdd (*low*) * bdd (*high*)

type operator =
  | Op_and | Op_or | Op_imp
  | Op_any of (bool -> bool -> bool)

let myfail x = (* assert (0=1); *) assert false

let var b = match b with
  | Zero         -> 1000 
  | One          -> 1000 
  | Node (v,_,_) -> v

let utag b = match b with
  | Zero        -> 0
  | One         -> 1 
  | Node (v,_,_)-> 2 

let low b = match b with
  | Zero -> myfail () 
  | One -> myfail ()
  | Node (_, l, _) -> l

let high b = match b with
  | Zero -> myfail ()
  | One -> myfail () 
  | Node (_, _, h) -> h

let cache_default_size = 7001

let zero = Zero 

let one  = One 

let of_bool b = if b then one else zero

let rec check x = 
  match x with Zero -> () | One -> () | Node (v, l, h) ->
    assert (v < var l); 
    assert (v < var h); 
    check l; check h

let mk x low high =
  let _ = show low in
  let _ = show high in
  if low = high then low else (Node (x, low, high))  

let mk_not x =
  let cache = Hashtbl.create cache_default_size in
  let _     = show x in
  let _     = check x in
  let rec mk_not_rec x = 
    if Hashtbl.mem cache x then 
      Hashtbl.find cache x
    else 
      let _ = show x in
      let res = 
        match x with
        | Zero           -> show one
        | One            -> show zero
        | Node (v, l, h) -> 
            let _ = show l in
            let _ = show h in
            mk v (mk_not_rec l) (mk_not_rec h)  in
      Hashtbl.add cache x res;
      res
  in
  let rv = mk_not_rec x in
  let _  = show rv in 
  let _  = check rv in
  rv

let apply_op op b1 b2 = match op with
  | Op_and -> b1 && b2
  | Op_or  -> b1 || b2
  | Op_imp -> (not b1) || b2
  | Op_any f -> f b1 b2


let gapply op = 
  let op_z_z = of_bool (apply_op op false false) in
  let op_z_o = of_bool (apply_op op false true) in
  let op_o_z = of_bool (apply_op op true false) in
  let op_o_o = of_bool (apply_op op true true) in
  fun b1 b2 -> 
    let cache = Hash2.create cache_default_size in
    let rec app u1 u2  =
      match op with
	| Op_and ->
	    if u1 == u2 then 
	      u1
	    else if u1 == zero || u2 == zero then
	      zero
	    else if u1 == one then
	      u2
	    else if u2 == one then
	      u1 
	    else
	      app_gen u1 u2 
	| Op_or ->
            if u1 == u2 then
	      u1
	    else if u1 == one || u2 == one then
	      one
	    else if u1 == zero then
	      u2
	    else if u2 == zero then
	      u1
	    else 
	      app_gen u1 u2 
	| Op_imp -> 
	    if u1 == zero then
	      one
	    else if u1 == one then
	      u2
	    else if u2 == one then
	      one
	    else
	      app_gen u1 u2 
 	| Op_any _ ->
	    app_gen u1 u2 
    and app_gen u1 u2 = 
      match (u1, u2) with
	| Zero, Zero -> show op_z_z
	| Zero, One  -> op_z_o
	| One,  Zero -> op_o_z
	| One,  One  -> op_o_o
	| _ ->
            if Hash2.mem cache u1 u2 then
              Hash2.find cache u1 u2 
            else  
	      let res = 
		let v1 = var u1 in
		let v2 = var u2 in
		if v1 == v2 then
                  mk v1 (app (low u1) (low u2)) (app (high u1) (high u2))
                else if v1 < v2 then
		  mk v1 (app (low u1) u2) (app (high u1) u2)
		else (* v1 > v2 *)
		  mk v2 (app u1 (low u2)) (app u1 (high u2)) 
	      in
	      Hash2.add cache u1 u2 (show res);
	      res 
    in 
    app b1 b2


(* monadized 
let mk_not x = 
  let cache = Myhash.create cache_default_size in
  let rec mk_not_rec c x = 
    if Myhash.mem c x then (c, Myhash.get c x) else
      let (c, res) = match x with
	| Zero -> 
            (c, one)
	| One -> 
            (c, zero)
	| Node (v, l, h) -> 
            let (c', l') = mk_not_rec c  l in
            let (c'',h') = mk_not_rec c' h in
            (c'', mk v l' h')
      in
      (Myhash.set c x res, res)
  in
  let ( _, res) = mk_not_rec cache x in
  res

(* val gapply: operator -> bdd -> bdd -> bdd *)
let gapply op =
  let op_z_z = of_bool (apply_op op false false) in
  let op_z_o = of_bool (apply_op op false true) in
  let op_o_z = of_bool (apply_op op true false) in
  let op_o_o = of_bool (apply_op op true true) in
  fun b1 b2 ->
    let cache = Myhash.create cache_default_size in
    let rec app c (u1,u2) =
      match op with
	| Op_and ->
	    if u1 = u2 then 
	      (c, u1)
	    else if u1 = zero || u2 = zero then
	      (c, zero)
	    else if u1 = one then
	      (c, u2)
	    else if u2 = one then
	      (c, u1) 
	    else
	      app_gen c (u1, u2) 
	| Op_or ->
            if u1 = u2 then
	      (c, u1)
	    else if u1 = one || u2 = one then
	      (c, one)
	    else if u1 = zero then
	      (c, u2)
	    else if u2 = zero then
	      (c, u1)
	    else 
	      app_gen c (u1, u2) 
	| Op_imp -> 
	    if u1 = zero then
	      (c, one)
	    else if u1 = one then
	      (c, u2)
	    else if u2 = one then
	      (c, one)
	    else
	      app_gen (u1, u2) 
 	| Op_any _ ->
	    app_gen c (u1, u2) 
    and app_gen c (u1,u2)  =
      match (u1, u2) with
	| Zero, Zero -> (c, op_z_z)
	| Zero, One  -> (c, op_z_o)
	| One,  Zero -> (c, op_o_z)
	| One,  One  -> (c, op_o_o)
	| _ ->
            if Myhash.mem c (u1, u2) then 
              (c, Myhash.get c (u1, u2))
            else
              let (c, res) = 
		let v1 = var u1 in
		let v2 = var u2 in
		if v1 = v2 then
                  let (c', l) = app c  (low u1,  low u2) in
                  let (c'',h) = app c' (high u1, high u2) in
                  (c'', mk v1 l h)  
		else if v1 < v2 then
                  let (c', l) = app c'  (low u1, u2) in
                  let (c'',h) = app c'' (high u1, u2) in
                  (c'', mk v1 l h)
		else (* v1 > v2 *)
                  let (c', l) = app c' (u1, low u2) in
                  let (c'',h) = app c' (u1, high u2) in
                  (c'', mk v2 l h) 
	      in
	      (Myhash.set c (u1, u2) res, res)
    in
    let (_, res) = app cache (b1, b2) in
    res
*)


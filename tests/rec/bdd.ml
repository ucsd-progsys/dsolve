(* Binary Decision Diagrams, adapted from code by Jean-Christophe Filliatre *)
type variable = int 

let myfail s = 
  print_string s; 
  assert false

type bdd = Zero of int | One of int | Node of int * variable * bdd * bdd

let skip (x:bdd) = () 

let tag  = function
  | Zero t              -> t
  | One t               -> t
  | Node (t,_,_,_)      -> t

let var b = match b with
  | Zero _              -> 1000
  | One  _              -> 1000 
  | Node (_,v,_, _)     -> v

let low b = match b with
  | Zero _              -> myfail "Bdd.low"
  | One  _              -> myfail "Bdd.low"
  | Node (_,_,l,_)      -> l

let high b = match b with
  | Zero _              -> myfail "Bdd.high" 
  | One  _              -> myfail "Bdd.high"
  | Node (_,_,_,h)      -> h

let hash_node v l h = 
  abs (19 * (19 * (tag l) + (tag h)) + v)

let hash = function
  | Zero _ -> 0
  | One  _ -> 1
  | Node (_, v, l, h) -> hash_node v l h

let gentag = let r = ref (-1) in fun z -> incr r; !r

(* SIMPLE VERSION *)
let hashcons_node v l h = 
  Node (gentag (), v, l, h)


type table = {
  mutable table : bdd Weak.t array;
  mutable totsize : int;             (* sum of the bucket sizes *)
  mutable limit : int;               (* max ratio totsize/table length *)
}

let create sz =
  let sz = if sz < 7 then 7 else sz in
  let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
  let emptybucket = Weak.create 0 in
  { table   = Array.create sz emptybucket;
    totsize = 0;
    limit   = 3; }

let t = create 100003 

let add t x = () (* SLICE *)

let hashcons_node v l h =
  let index = (hash_node v l h) mod (Array.length t.table) in
  let bucket = t.table.(index) in
  let sz = Weak.length bucket in
  let rec loop i =
    if i >= sz then begin
      let hnode = Node (gentag (), v, l, h) in
      (* SLICE: add_index t hnode index; *)
      hnode
    end else begin
      match Weak.get_copy bucket i with
      | Some x -> assert false
      (*
        | Some (Node(_, v',l', h')) -> assert false
      if v==v' && l == l' && h == h' then
	    begin match Weak.get bucket i with
            | Some (Node(t',v',l',h')) -> 
                if v==v' && l == l' && h == h'
                then Node(t', v',l', h') else loop (i+1) (* assert false *)
            | _ -> loop (i+1)
            end
          else loop (i+1)
        *)
        | _ -> loop (i+1)
    end
  in
  loop 0

(*
let zero = Zero (gentag ())  
let one  = One (gentag ()) 
let _    = add t zero; add t one


let mk v low high =
  if low == high then low else hashcons_node v low high

let mk_var v = mk v zero one

let cache_default_size = 7001

let mk_not x = 
  let cache = Hashtbl.create cache_default_size in
  let rec mk_not_rec x = 
    if Hashtbl.mem cache x then
      Hashtbl.find cache x 
    else
      let res = match x with
	| Zero _         -> one
	| One  _         -> zero
	| Node (_,v,l,h) -> mk v (mk_not_rec l) (mk_not_rec h)
      in
      Hashtbl.add cache x res;
      res
  in
  mk_not_rec x

let of_bool b = if b then one else zero

type operator =
  | Op_and | Op_or | Op_imp
  | Op_any of (bool -> bool -> bool)

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
	| (Zero _), (Zero _) -> op_z_z
	| (Zero _), (One  _) -> op_z_o
	| (One  _), (Zero _) -> op_o_z
	| (One  _), (One  _) -> op_o_o
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
	      Hash2.add cache u1 u2 res;
	      res 
    in 
    app b1 b2

let mk_and = gapply Op_and
let mk_or = gapply Op_or
let mk_imp = gapply Op_imp
let mk_iff = gapply (Op_any (fun b1 b2 -> b1 == b2))

let apply f = gapply (Op_any f)

(* formula -> bdd *)

type formula = 
  | Ffalse 
  | Ftrue 
  | Fvar of variable 
  | Fand of formula * formula
  | For  of formula * formula
  | Fimp of formula * formula
  | Fiff of formula * formula
  | Fnot of formula

let rec build = function
  | Ffalse -> zero
  | Ftrue -> one
  | Fvar v -> mk_var v
  | Fand (f1, f2) -> mk_and (build f1) (build f2)
  | For (f1, f2) -> mk_or (build f1) (build f2)
  | Fimp (f1, f2) -> mk_imp (build f1) (build f2)
  | Fiff (f1, f2) -> mk_iff (build f1) (build f2)
  | Fnot f -> mk_not (build f)
  *)

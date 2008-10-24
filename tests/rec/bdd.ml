(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Binary Decision Diagrams *)

let debug = true

type variable = int (* 1..max_var *)

let max_var = ref 10
let get_max_var () = !max_var
let set_max_var n = if n <= 0 then invalid_arg "Bdd.set_max_var"; max_var := n 

(*type bdd = { tag: int; node : view } and *) 
type view = Zero | One | Node of variable * (int * view) (*low*) * (int * view) (*high*)
type bdd  = int * view
type t    = int * view (* export *)

let view b = snd b
let tag  b = fst b
let node b = snd b

let equal x y = match x, y with
  | Node (v1, l1, h1), Node (v2, l2, h2) -> 
      v1 == v2 && l1 == l2 && h1 == h2
  | _ ->
      x == y
	   
let hash_node v l h = abs (19 * (19 * (tag l) + (tag h)) + v)

let hash = function
  | Zero -> 0
  | One -> 1
  | Node (v, l, h) -> hash_node v l h

let gentag = let r = ref (-1) in fun () -> incr r; !r
    
type table = {
  mutable table : bdd Weak.t array;
  mutable totsize : int;             (* sum of the bucket sizes *)
  mutable limit : int;               (* max ratio totsize/table length *)
}

let create sz =
  let sz = if sz < 7 then 7 else sz in
  let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
  let emptybucket = Weak.create 0 in
  { table = Array.create sz emptybucket;
    totsize = 0;
    limit = 3; }

let t = create (*251*)(*7001*)(*65537*)100003 

let fold f t init =
  let rec fold_bucket i b accu =
    if i >= Weak.length b then accu else
      match Weak.get b i with
	| Some v -> fold_bucket (i+1) b (f v accu)
	| None -> fold_bucket (i+1) b accu
  in
  Array.fold_right (fold_bucket 0) t.table init

let iter f t =
  let rec iter_bucket i b =
    if i >= Weak.length b then () else
      match Weak.get b i with
	| Some v -> f v; iter_bucket (i+1) b
	| None -> iter_bucket (i+1) b
  in
  Array.iter (iter_bucket 0) t.table

let count t =
  let rec count_bucket i b accu =
    if i >= Weak.length b then accu else
      count_bucket (i+1) b (accu + (if Weak.check b i then 1 else 0))
  in
  Array.fold_right (count_bucket 0) t.table 0

let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1)

let rec resize () =
  if debug then Format.eprintf "resizing...@.";
  let oldlen = Array.length t.table in
  let newlen = next_sz oldlen in
  if newlen > oldlen then begin
    let newt = create newlen in
    newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
    fold (fun d () -> add newt d) t ();
    t.table <- newt.table;
    t.limit <- t.limit + 2;
  end

and add t d =
  add_index t d ((hash (node d)) mod (Array.length t.table))

and add_index t d index =
  let bucket = t.table.(index) in
  let sz = Weak.length bucket in
  let rec loop i =
    if i >= sz then begin
      let newsz = min (sz + 3) (Sys.max_array_length - 1) in
      if newsz <= sz then 
	failwith "Hashcons.Make: hash bucket cannot grow more";
      let newbucket = Weak.create newsz in
      Weak.blit bucket 0 newbucket 0 sz;
      Weak.set newbucket i (Some d);
      t.table.(index) <- newbucket;
      t.totsize <- t.totsize + (newsz - sz);
      if t.totsize > t.limit * Array.length t.table then resize ();
    end else begin
      if Weak.check bucket i
      then loop (i+1)
      else Weak.set bucket i (Some d)
    end
  in
  loop 0

let hashcons_node v l h =
  let index = (hash_node v l h) mod (Array.length t.table) in
  let bucket = t.table.(index) in
  let sz = Weak.length bucket in
  let rec loop i =
    if i >= sz then begin
      let hnode = (gentag (), Node (v, l, h)) in
      add_index t hnode index;
      hnode
    end else begin
      match Weak.get_copy bucket i with
        | Some (_,Node(v',l',h')) when v==v' && l==l' && h==h' ->
	    begin match Weak.get bucket i with
              | Some v -> v
              | None -> loop (i+1)
            end
        | _ -> loop (i+1)
    end
  in
  loop 0
  
let stats () =
  let len = Array.length t.table in
  let lens = Array.map Weak.length t.table in
  Array.sort compare lens;
  let totlen = Array.fold_left ( + ) 0 lens in
  (len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))

(* zero and one allocated once and for all *)
let zero = (gentag (), Zero) 
let one  = (gentag (), One) 
let () = add t zero; add t one
  
let init sz =
  let sz = if sz < 7 then 7 else sz in
  let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
  let emptybucket = Weak.create 0 in
  t.table <- Array.create sz emptybucket;
  t.totsize <- 0;
  t.limit <- 3;
  add t zero; add t one

let var b = match node b with
  | Zero | One -> !max_var + 1
  | Node (v, _, _) -> v

let low b = match node b with
  | Zero | One -> invalid_arg "Bdd.low"
  | Node (_, l, _) -> l

let high b = match node b with
  | Zero | One -> invalid_arg "Bdd.low"
  | Node (_, _, h) -> h

let mk v ~low ~high =
  if low == high then low else hashcons_node v low high

let mk_var v = mk v ~low:zero ~high:one

module Bdd = struct
  type t = bdd
  let equal = (==)
  let hash b = tag b
  let compare b1 b2 = Pervasives.compare (tag b1) (tag b2)
end
module H1 = Hashtbl.Make(Bdd)

let cache_default_size = 7001

let mk_not x = 
  let cache = H1.create cache_default_size in
  let rec mk_not_rec x = 
    try
      H1.find cache x
    with Not_found -> 
      let res = match node x with
	| Zero -> one
	| One -> zero
	| Node (v, l, h) -> mk v (mk_not_rec l) (mk_not_rec h)
      in
      H1.add cache x res;
      res
  in
  mk_not_rec x

let bool_of = function Zero -> false | One -> true | _ -> invalid_arg "bool_of"
let of_bool b = if b then one else zero

module H2 = Hashtbl.Make(
  struct
    type t = bdd * bdd
    let equal (u1,v1) (u2,v2) = u1==u2 && v1==v2
    let hash (u,v) = 
      let s = (tag u) + (tag v) in abs (s * (s+1) / 2 + (tag u))
  end)

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
    let cache = H2.create cache_default_size in
    let rec app ((u1,u2) as u12) =
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
	      app_gen u12
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
	      app_gen u12
	| Op_imp -> 
	    if u1 == zero then
	      one
	    else if u1 == one then
	      u2
	    else if u2 == one then
	      one
	    else
	      app_gen u12
 	| Op_any _ ->
	    app_gen u12
    and app_gen ((u1,u2) as u12) =
      match (node u1, node u2) with
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
		if v1 == v2 then
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

(*
let memo cache f x = 
  try find cache x with Not_found ->
    let r = f x in add cache x r; r
*)



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

(* satisfiability *)

let is_sat b = (node b) != Zero

let tautology b = (node b) == One

let rec int64_two_to = function
  | 0 -> 
      Int64.one
  | n -> 
      let r = int64_two_to (n/2) in
      let r2 = Int64.mul r r in
      if n mod 2 == 0 then r2 else Int64.mul (Int64.of_int 2) r2

let count_sat =
  let cache = H1.create cache_default_size in
  let rec count b = 
    try
      H1.find cache b
    with Not_found ->
      let n = match node b with
	| Zero -> Int64.zero
	| One -> Int64.one
	| Node (v, l, h) -> 
	    let dvl = var l - v - 1 in
	    let dvh = var h - v - 1 in
	    Int64.add
	      (Int64.mul (int64_two_to dvl) (count l))
	      (Int64.mul (int64_two_to dvh) (count h))
      in
      H1.add cache b n;
      n
  in
  count

let any_sat =
  let rec mk acc b = match node b with
    | Zero -> raise Not_found
    | One -> acc
    | Node (v, (_,Zero), h) -> mk ((v,true)::acc) h
    | Node (v, l, _) -> mk ((v,false)::acc) l
  in
  mk []

(* TODO: a CPS version of all_sat *)
let all_sat = 
  let cache = H1.create cache_default_size in
  let rec mk b = 
    try
      H1.find cache b
    with Not_found ->
      let res = match node b with
	| Zero -> []
	| One -> [[]]
	| Node (v, l, h) -> 
	    (List.map (fun a -> (v,false)::a) (mk l)) 
	    @ (List.map (fun a -> (v,true)::a) (mk h))
      in
      H1.add cache b res;
      res
  in
  mk

(* DOT pretty-printing *)

module S = Set.Make(Bdd)

open Format

let print_to_dot b ~file =
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  fprintf fmt "digraph bdd {@\n";
  let ranks = Hashtbl.create 17 in (* var -> set of nodes *)
  let add_rank v b = 
    try Hashtbl.replace ranks v (S.add b (Hashtbl.find ranks v))
    with Not_found -> Hashtbl.add ranks v (S.singleton b)
  in
  let visited = H1.create cache_default_size in
  let rec visit b =
    if not (H1.mem visited b) then begin
      H1.add visited b ();
      match node b with
	| Zero ->
	    fprintf fmt "%d [shape=box label=\"0\"];" (tag b)
	| One -> 
	    fprintf fmt "%d [shape=box label=\"1\"];" (tag b)
	| Node (v, l, h) -> 
	    add_rank v b; 
	    fprintf fmt "%d [label=\"x%d\"];" (tag b) v;
	    fprintf fmt "%d -> %d;@\n" (tag b) (tag h);
	    fprintf fmt "%d -> %d [style=\"dashed\"];@\n" (tag b) (tag l);
	    visit h; visit l
    end
  in
  Hashtbl.iter
    (fun v s -> 
       fprintf fmt "{rank=same; ";
       S.iter (fun x -> fprintf fmt "%d " (tag x)) s;
       fprintf fmt ";}@\n"
    )
    ranks;
  visit b;
  fprintf fmt "}@.";
  close_out c

let display b =
  let file = Filename.temp_file "bdd" ".dot" in
  print_to_dot b ~file;
  let cmd = sprintf "dot -Tps %s | gv -" file in
  begin try ignore (Sys.command cmd) with _ -> () end;
  try Sys.remove file with _ -> ()

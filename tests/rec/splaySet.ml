(**************************************************************************)
(*  The OCaml Reins Library                                               *)
(*                                                                        *)
(*  Copyright 2007 Mike Furr.                                             *)
(*  All rights reserved.  This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1 with the linking        *)
(*  exception given in the COPYING file.                                  *)
(**************************************************************************)

open Types

module BaseSet = struct

  type 'elt tree = 
    | Empty
    | Node of 'elt tree * 'elt * 'elt tree
	
  type 'a path = 
    | Top 
    | PathL of 'a path * 'a tree
    | PathR of 'a path * 'a tree

  type 'a curs = 'a path * 'a tree

  let of_result (x,_) = x

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false

  let singleton x = Node(Empty,x,Empty)

  let node l e r = Node(l,e,r)

  let to_cursor t = (Top,t)

  let rec from_cursor (p,t) = match p with
    | Top -> t
    | PathL(p',Node(_,v,r)) -> from_cursor (p', Node(t,v,r))
    | PathR(p',Node(l,v,_)) -> from_cursor (p', Node(l,v,t))
    | _ -> assert false

  let at_top (p,t) = (p = Top)
  let has_left (p,t) = match t with
    | Node(Empty,_,_) -> false
    | Node _ -> true
    | _ -> false

  let has_right (p,t) = match t with
    | Node(_,_,Empty) -> false
    | Node _ -> true
    | _ -> false

  let went_left = function PathL _,_ -> true | _ -> false
  let went_right = function PathR _,_ -> true | _ -> false

  let move_up (p,t) = match p with
    | Top -> failwith "move_up"
    | PathL(p',Node(_,v,r)) -> p', Node(t,v,r)
    | PathR(p',Node(l,v,_)) -> p', Node(l,v,t)
    | _ -> assert false (* parent can't be emptytree *)

  let move_down_left (p,t) = match t with
    | Empty -> failwith "move_down_left"
    | Node(l,v,r) -> PathL(p,t),l

  let move_down_right (p,t) = match t with
    | Empty -> failwith "move_down_right"
    | Node(l,v,r) -> PathR(p,t),r

  let rec move_to_ancestor cmp x ((p,t) as curs) = match p with
    | Top -> curs
    | PathL(p', Node(_,v,_)) -> 
	if cmp x v < 0 then curs 
	else move_to_ancestor cmp x (move_up curs)
    | PathR(p', Node(_,v,_)) ->
	if cmp x v > 0 then curs 
	else move_to_ancestor cmp x (move_up curs)
    | _ -> assert false


  let rec splay curs = match curs with
    | Top,_ -> curs
    | _, Empty -> splay (move_up curs)

    (* no grand-parent, so just zig one level *)
    | PathL(Top,Node(_,v,r)), Node(ll,lv,lr) -> 
	Top,Node(ll,lv,Node(lr,v,r))
	
    | PathR(Top,Node(l,v,_)),Node(rl,rv,rr) -> 
	Top,Node(Node(l,v,rl),rv,rr)

    (* has grand-parent *)
    (* zig-zig *)
    | PathL(PathL(gp_p,Node(_,v,r)),Node(_,lv,lr)), Node(lll,llv,llr) ->
	let br = Node(lr,v,r) in
	let mr = Node(llr,lv,br) in
	  splay (gp_p, Node(lll,llv,mr))

    (* zig-zig *)
    | PathR(PathR(gp_p,Node(l,v,_)),Node(ll,lv,_)), Node(rrl,rrv,rrr) ->
	let bl = Node(l,v,ll) in
	let ml = Node(bl,lv,rrl) in
	  splay (gp_p,Node(ml,rrv,rrr))

    (* zig-zag *)
    | PathL(PathR(gp_p,Node(l,v,_)),Node(_,rv,rr)), Node(rll,rlv,rlr) ->
	let newl = Node(l,v,rll) in
	let newr = Node(rlr,rv,rr) in
	  splay (gp_p,Node(newl, rlv, newr))
	  
    (* zig-zag *)
    | PathR(PathL(gp_p,Node(_,v,r)),Node(ll,lv,_)), Node(lrl,lrv,lrr) ->
	let newl = Node(ll,lv,lrl) in
	let newr = Node(lrr,v,r) in
	  splay(gp_p, Node(newl, lrv, newr))

    (* all of remaining cases are impossible. e.g., the grandparent
       tree being Empty *)
    | _ -> assert false

  let rec add_at cmp x ((p,t) as curs) = match t with
    | Empty -> p,Node(Empty,x,Empty)
    | Node(l,v,r) -> match cmp x v with
	| 0 -> curs
	| c when c < 0 -> add_at cmp x (PathL(p,t),l)
	| _ -> add_at cmp x (PathR(p,t),r)

  let add cmp x t = 
    let curs = add_at cmp x (to_cursor t) in
      from_cursor (splay curs)

  let rec closest_to cmp x ((p,t) as curs) = match t with
    | Empty -> if at_top curs then curs else move_up curs
    | Node(l,v,r) -> match cmp x v with
	| 0 -> curs
	| c when c < 0 -> closest_to cmp x (PathL(p,t),l)
	| _ -> closest_to cmp x (PathR(p,t),r)

  let top_node = function
    | Empty -> raise (Invalid_argument "splay:top_node")
    | Node(_,v,_) -> v

  let rec goto_min ((p,t) as curs) = match t with 
    | Empty -> curs
    | Node(Empty,_,_) -> curs
    | Node(l,_,_) -> goto_min ((PathL(p,t)),l)

  let rec goto_max ((p,t) as curs) = match t with 
    | Empty -> curs
    | Node(_,_,Empty) -> curs
    | Node(_,_,r) -> goto_max ((PathR(p,t)),r)

  let rec min_elt t = 
    if is_empty t then raise Not_found
    else
      let c = goto_min (to_cursor t) in
      let t = from_cursor (splay c) in
	top_node t, t

  let max_elt t = 
    if is_empty t then raise Not_found
    else
      let c = goto_max (to_cursor t) in
      let t = from_cursor (splay c) in
	top_node t, t
      
  let mem cmp x t = 
    let curs = closest_to cmp x (to_cursor t) in
    let t = from_cursor (splay curs) in
      match t with
	| Empty -> false,t
	| Node(_,v,_) -> if cmp x v = 0
	  then true,t
	  else false,t

  (* TODO: fix this to be better than O(n) stack *)
  let rec iter f = function
    | Empty -> ()
    | Node(l,v,r) -> iter f l; f v; iter f r

  let rec get_and_remove_min = function
    | Empty -> raise (Invalid_argument "remove_min")
    | Node(Empty,v,r) -> v,r
    | Node(l,v,r) -> 
	let d,newl = get_and_remove_min l in
	  d, Node(newl,v,r)

  let remove cmp x t = 
    let (p,t) = closest_to cmp x (to_cursor t) in
    let t = match t with
	| Empty -> t
	| Node(Empty,v,r) -> if (cmp v x) = 0 then r else t
	| Node(l,v,Empty) -> if (cmp v x) = 0 then l else t
	| Node(l,v,r) -> 
	    if (cmp v x) = 0 then
	      let d,newl = get_and_remove_min l in
		Node(newl,d,r)
	    else t
    in
      from_cursor (splay (p,t))


  let rec split cmp v t = match t with
    | Empty -> Empty, Empty
    | Node(l1,elt,r1) ->
	match cmp v elt with
	  | 0 -> l1,r1
	  | c when c < 0 ->
	      let l2,r2 = split cmp v l1 in
		l2,Node(r2,elt,r1)
	  | _ ->
	      let l2,r2 = split cmp v r1 in
		Node(l1,elt,l2), r2

  let rec union cmp t1 t2 = match t1,t2 with
    | Empty, t | t, Empty -> t
    | t1, Node(l,v,r) ->
	let l',r' = split cmp v t1 in
	  Node((union cmp l' l),v,(union cmp r' r))

  let rec concat t1 t2 = match t1,t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Node(l1,v1,r1), Node(l2,v2,r2) ->
	let m,t2' = get_and_remove_min t2 in
	  Node(t1,m,t2')

  let rec diff cmp t1 t2 = match t1,t2 with
    | Empty, _ -> Empty 
    | _, Empty -> t1
    | _, Node(l,v,r) ->
	let l',r' = split cmp v t1 in
	  concat (diff cmp l' l) (diff cmp r' r)

  let rec inter cmp t1 t2 = match t1,t2 with
    | Empty,_ | _,Empty -> Empty
    | t1, Node(l,v,r) ->
	let l',r' = split cmp v t1 in
	  if fst (mem cmp v t1)
	  then Node((inter cmp l' l),v,(inter cmp r' r))
	  else concat (inter cmp l' l) (inter cmp r' r)

  let at_right = function
    | _,Empty -> true
    | _,Node _ -> false

  let at_left = at_right

  let has_value = function _,Node _ -> true | _ -> false
  let get_value = function
    | _,Empty -> failwith "get_value"
    | _,Node(_,v,_) -> v
	
  let rec cardinal = function
    | Empty -> 0
    | Node(l,_,r) -> 1 + (cardinal l) + (cardinal r)

  let choose t = match t with
    | Empty -> raise Not_found
    | Node(l,v,r) -> v, t

  (* TODO: fix this to be better than O(n) stack *)
  let rec fold f acc t = match t with
    | Empty -> acc
    | Node(l,v,r) ->
	fold f (f (fold f acc l) v) r
	
  let rec well_ordered cmp = function
    | Empty -> true
    | Node(Empty,e,Empty) -> true
    | Node(Node(_,le,_) as l,e,Empty) -> 
	((cmp le e) < 0) && well_ordered cmp l
    | Node(Empty,e,(Node(_,re,_) as r)) -> 
	((cmp re e) > 0) && well_ordered cmp r
    | Node(Node(_,le,_) as l,e,(Node(_,re,_) as r)) -> 
	((cmp le e) < 0) &&((cmp re e) > 0) && 
	  well_ordered cmp l && well_ordered cmp r

  let well_formed t = well_ordered t

  let rec compare_ kcmp t1 t2 = match t1,t2 with
    | Empty, Empty -> 0
    | Empty, Node _ -> -1
    | Node _, Empty -> 1
    | _ ->
	(* This actually may be one of the most efficient ways to
	   implement this since we will always be removing near the
	   top thanks to the splay property. *)
	let xk,t1' = get_and_remove_min t1 in
	let yk,t2' = get_and_remove_min t2 in
	  match kcmp xk yk with
	    | 0 -> compare_ kcmp t1' t2'
	    | v -> v

  let rec to_string to_s t = 
    let rec h = function
      | Empty -> ""
      | Node(Empty,v,Empty) -> to_s v
      | Node(l,v,Empty) -> Printf.sprintf "%s, %s" (h l) (to_s v)
      | Node(Empty,v,r) -> Printf.sprintf "%s, %s" (to_s v) (h r)
      | Node(l,v,r) ->
	  Printf.sprintf "%s, %s, %s"
	    (h l) (to_s v) (h r)
    in "{" ^ (h t) ^ "}"

  let gen_ cmp (agen : ?size:int -> Random.State.t -> 'a) 
      ?(size=50) rs : 'a tree = 
    let num = Random.State.int rs size in
    let rec loop n t = 
      if n <= 0 then t
      else
	let t = from_cursor (add_at cmp (agen rs) (to_cursor t)) in
	  loop (n-1) t
    in
      loop num empty


end

module PolySet = struct
  include BaseSet
  type 'a t = 'a tree
  type 'a set = 'a t
  type ('a,'b) result = 'a * 'b t
  type ('a,'b) result_ = ('a,'b) result

  type 'a elt_ = 'a

  type 'a cursor = 'a curs
  type 'a cursor_ = 'a cursor

  let add x t = add Pervasives.compare x t
  let add_at x t = add_at Pervasives.compare x t

  let compare x y = compare_ Pervasives.compare x y
  let equal x y = compare x y = 0
  let mem x t = mem Pervasives.compare x t
  let remove x t = remove Pervasives.compare x t
  let union t1 t2 = union Pervasives.compare t1 t2
  let diff t1 t2 = diff Pervasives.compare t1 t2
  let inter t1 t2 = inter Pervasives.compare t1 t2
  let well_formed t = well_formed Pervasives.compare t

  let gen1 (agen : ?size:int -> Random.State.t -> 'a) ?size rs : 'a t =
    gen_ Pervasives.compare agen ?size rs

end

module MonoSet(C : Mono.Comparable) = struct
  include BaseSet
  type elt = C.t
  type 'a elt_ = elt

  type t = elt tree
  type 'a set = t

  type 'a result = 'a * t
  type ('a,'b) result_ = 'a result

  type cursor = elt curs
  type 'a cursor_ = cursor

  let add x t = add C.compare x t
  let mem x t = mem C.compare x t
  let remove x t = remove C.compare x t
  let union t1 t2 = union C.compare t1 t2
  let diff t1 t2 = diff C.compare t1 t2
  let inter t1 t2 = inter C.compare t1 t2

  let add_at x t = add_at C.compare x t

  let compare t1 t2 = compare_ C.compare t1 t2
  let equal t1 t2 = compare t1 t2 = 0
  let well_formed t = well_formed C.compare t

  let to_string s = to_string C.to_string s

  let gen1 (agen : ?size:int -> Random.State.t -> elt) ?size rs : t =
    gen_ C.compare agen ?size rs

end

module GenSet(C : Types.Mono.ArbitraryComparable) = struct
  include MonoSet(C)

  let gen ?size rs = gen1 C.gen ?size rs
end


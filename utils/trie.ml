(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: trie.ml,v 1.7 2008-07-21 14:53:06 filliatr Exp $ *)

(*s A trie is a tree-like structure to implement dictionaries over
    keys which have list-like structures. The idea is that each node
    branches on an element of the list and stores the value associated
    to the path from the root, if any. Therefore, a trie can be
    defined as soon as a map over the elements of the list is
    given. *)

module Make (M : Map.S) = struct

(*s Then a trie is just a tree-like structure, where a possible
    information is stored at the node (['a option]) and where the sons
    are given by a map from type [key] to sub-tries, so of type 
    ['a t M.t]. The empty trie is just the empty map. *)

  type key = M.key list

  type 'a t = Node of 'a option * 'a t M.t

  let empty = Node (None, M.empty)

(*s To find a mapping in a trie is easy: when all the elements of the
    key have been read, we just inspect the optional info at the
    current node; otherwise, we descend in the appropriate sub-trie
    using [M.find]. *)

  let rec find l t = match (l,t) with
    | [], Node (None,_)   -> raise Not_found
    | [], Node (Some v,_) -> v
    | x::r, Node (_,m)    -> find r (M.find x m)

  exception Found

  let find_maximal_from m f =
   let u = ref None in 
   let rec find_rec m =
     let f k = function
       | (Node (Some v, m)) when m = M.empty -> u := Some v; raise Found
       | (Node (None, m)) -> (try find_rec m; () with Not_found -> ())
       | (Node (Some v, m)) -> if f v then (u := Some v; raise Found) else try find_rec m; () with Not_found -> () in
     M.iter f m in
   try find_rec m; raise Not_found with Found -> match !u with Some v -> v | None -> assert false

  let find_maximal l t f =
    let rec find_rec vs l t = match (l, t) with 
      | [], Node (None, m) -> (*find_maximal_from m f*) assert false
      | [], Node (Some v, m) -> if f v then (vs, v) else (try (v :: vs, find_maximal_from m f) with Not_found -> (vs, v))
      | x :: r, Node (Some v, m) -> find_rec (v :: vs) r (M.find x m)
      | x :: r, Node (None, m) -> find_rec vs r (M.find x m) in
    find_rec [] l t

  let iter_path l t f =
    let rec iter_rec l t = match (l, t) with
      | [], Node (None, m) -> ()
      | [], Node (Some v, m) -> f v; ()
      | x :: r, Node (Some v, m) -> f v; iter_rec r (M.find x m); ()
      | x :: r, Node (None, m) -> iter_rec r (M.find x m); () in
    iter_rec l t

  let rec mem l t = match (l,t) with
    | [], Node (None,_)   -> false
    | [], Node (Some _,_) -> true
    | x::r, Node (_,m)    -> try mem r (M.find x m) with Not_found -> false

(*s Insertion is more subtle. When the final node is reached, we just
    put the information ([Some v]). Otherwise, we have to insert the
    binding in the appropriate sub-trie [t']. But it may not exists,
    and in that case [t'] is bound to an empty trie. Then we get a new
    sub-trie [t''] by a recursive insertion and we modify the
    branching, so that it now points to [t''], with [M.add]. *)

  let add l v t =
    let rec ins = function
      | [], Node (_,m) -> Node (Some v,m)
      | x::r, Node (v,m) ->
	  let t' = try M.find x m with Not_found -> empty in
	  let t'' = ins (r,t') in
	  Node (v, M.add x t'' m)
    in
    ins (l,t)

(*s When removing a binding, we take care of not leaving bindings to empty
    sub-tries in the nodes. Therefore, we test wether the result [t'] of 
    the recursive call is the empty trie [empty]: if so, we just remove
    the branching with [M.remove]; otherwise, we modify it with [M.add]. *)

  let rec remove l t = match (l,t) with
    | [], Node (_,m) -> Node (None,m)
    | x::r, Node (v,m) -> 
	try
	  let t' = remove r (M.find x m) in
	  Node (v, if t' = empty then M.remove x m else M.add x t' m)
	with Not_found ->
	  t

(*s The iterators [map], [mapi], [iter] and [fold] are implemented in
    a straigthforward way using the corresponding iterators [M.map],
    [M.mapi], [M.iter] and [M.fold]. For the last three of them,
    we have to remember the path from the root, as an extra argument
    [revp]. Since elements are pushed in reverse order in [revp],
    we have to reverse it with [List.rev] when the actual binding 
    has to be passed to function [f]. *)

  let rec map f = function
    | Node (None,m)   -> Node (None, M.map (map f) m)
    | Node (Some v,m) -> Node (Some (f v), M.map (map f) m)

  let mapi f t = 
    let rec maprec revp = function
    | Node (None,m) -> 
	Node (None, M.mapi (fun x -> maprec (x::revp)) m)
    | Node (Some v,m) -> 
	Node (Some (f (List.rev revp) v), M.mapi (fun x -> maprec (x::revp)) m)
    in
    maprec [] t

  let iter f t =
    let rec traverse revp = function
      | Node (None,m) -> 
	  M.iter (fun x -> traverse (x::revp)) m
      | Node (Some v,m) -> 
	  f (List.rev revp) v; M.iter (fun x t -> traverse (x::revp) t) m
    in
    traverse [] t

  let rec fold f t acc =
    let rec traverse revp t acc = match t with
      | Node (None,m) -> 
	  M.fold (fun x -> traverse (x::revp)) m acc
      | Node (Some v,m) -> 
	  f (List.rev revp) v (M.fold (fun x -> traverse (x::revp)) m acc)
    in
    traverse [] t acc

  let compare cmp a b =
    let rec comp a b = match a,b with
      | Node (Some _, _), Node (None, _) -> 1
      | Node (None, _), Node (Some _, _) -> -1
      | Node (None, m1), Node (None, m2) ->
	  M.compare comp m1 m2
      | Node (Some a, m1), Node (Some b, m2) ->
	  let c = cmp a b in
	  if c <> 0 then c else M.compare comp m1 m2
    in
    comp a b

  let equal eq a b =
    let rec comp a b = match a,b with
      | Node (None, m1), Node (None, m2) ->
	  M.equal comp m1 m2
      | Node (Some a, m1), Node (Some b, m2) ->
	  eq a b && M.equal comp m1 m2
      | _ -> 
	  false
    in
    comp a b

  (* The base case is rather stupid, but constructable *)
  let is_empty = function
    | Node (None, m1) -> M.is_empty m1
    | _ -> false

end

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

(*s Heaps *)

(* Heaps are encoded as complete binary trees, i.e., binary trees
   which are full expect, may be, on the bottom level.
   These trees also enjoy the heap property, namely the value of any node
   is greater or equal than those of its left and right subtrees.

   The representation invariant is the following: the number of nodes in
   the left subtree is equal to the number of nodes in the right
   subtree, or exceeds it by exactly once. In the first case, we use
   the constructor [Same] and in the second the constructor [Diff].
   Then it can be proved that [2^(h-1) <= n <= 2^h] when [n] is the
   number of elements and [h] the height of the tree. *)

type 'a t =
  | Empty
  | Same of 'a * 'a t * 'a t (* same number of elements on both sides *)
  | Diff of 'a * 'a t * 'a t (* left has [n+1] nodes and right has [n] *)

let rec set_of t = match t with
    Empty -> Myaset.empty
  | Same (x, l, r) -> Myaset.cup (Myaset.sng x) (Myaset.cup (set_of l) (set_of r))
  | Diff (x, l, r) -> Myaset.cup (Myaset.sng x) (Myaset.cup (set_of l) (set_of r))

let empty = Empty

let rec add x h = match h with
  | Empty ->
      Same (x, Empty, Empty)
        (* insertion to the left *)
  | Same (y, l, r) ->
      if x > y then Diff (x, add y l, r) else Diff (y, add x l, r)
        (* insertion to the right *)
  | Diff (y, l, r) ->
      if x > y then Same (x, l, add y r) else Same (y, l, add x r)
      
let maximum h = match h with
  | Empty -> assert false (* raise EmptyHeap *)
  | Same (x, l, r) -> (x, Same (x, l, r)) | Diff (x, l, r) -> (x, Diff (x, l, r))

(*let rec nfind x h = match h with
  | Empty -> ()
  | Same(d, l, r) -> let xx = nfind x l in let xx = nfind x r in ()
  | Diff(d, l, r) -> let xx = nfind x l in let xx = nfind x r in ()*)

(* extracts one element on the bottom level of the tree, while
   maintaining the representation invariant *)
let rec extract_last h = match h with
  | Empty -> assert false (* raise EmptyHeap *)
  | Same (x, Empty, Empty) -> (x, Empty)
  | Same (x, l, r) ->
      let y,r' = extract_last r in
        (y, Diff (x, l, r'))
  | Diff (x, l, r) ->
      let y,l' = extract_last l in
        (y, Same (x, l', r))

(*
(* needs proof of balance invariant to get set invariant *)
(* removes the topmost element of the tree and inserts a new element [x] *)
let rec descent x h = match h with
  | Empty ->
      assert false
  | Same (_, Empty, Empty) ->
      Same (x, Empty, Empty)
  | Diff (_, Same (z, ll, lr), Empty) ->
      if x > z then Diff (x, Same (z, ll, lr), Empty)
      else Diff (z, Same (x, Empty, Empty), Empty)
  | Same (_, l, r) ->
      let (ml, l) = maximum l in
      let (mr, r) = maximum r in
        if x > ml && x > mr then
	  Same (x, l, r)
	else
	  if ml > mr then
	    Same (ml, descent x l, r)
	  else
	    Same (mr, l, descent x r)
  | Diff (_, l, r) ->
      let (ml, l) = maximum l in
      let (mr, r) = maximum r in
	if x > ml && x > mr then
	  Diff (x, l, r)
	else
	  if ml > mr then
	    Diff (ml, descent x l, r)
	  else
	    Diff (mr, l, descent x r)

      
let remove = function
  | Empty -> assert false (* raise EmptyHeap *)
  | Same (x, Empty, Empty) -> Empty
  | h -> let y,h' = extract_last h in descent y h'

let rec iter f = function
  | Empty -> ()
  | Same (x, l, r) -> iter f l; f x; iter f r
  | Diff (x, l, r) -> iter f l; f x; iter f r

let rec fold f h x0 = match h with
  | Empty -> x0
  | Same (x, l, r) -> fold f l (fold f r (f x x0))
  | Diff (x, l, r) -> fold f l (fold f r (f x x0))
  *)

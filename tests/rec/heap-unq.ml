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

let rec sz h =
  match h with
  | Empty -> 0
  | Same (x, l, r) -> 1 + sz l + sz r
  | Diff (x, l, r) -> 1 + sz l + sz r

let max h =
  match h with
  | Empty -> assert false
  | Same (x, _, _) -> x
  | Diff (x, _, _) -> x

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
  | Empty -> let _ = assert (1 = 0) in assert false (* raise EmptyHeap *)
  | Same (x, l, r) -> (x, Same (x, l, r)) | Diff (x, l, r) -> (x, Diff (x, l, r))

  
(* extracts one element on the bottom level of the tree, while
   maintaining the representation invariant *)
let rec extract_last h = match h with
  | Empty -> let _ = assert (1 = 0) in assert false (* raise EmptyHeap *)
  | Same (x, l, r) ->
      (match l with
      | Empty ->
          (match r with
          | Empty -> (x, Empty)
          | Same (_, _, _) -> let _ = assert (1 = 0) in assert false
          | Diff (_, _, _) -> let _ = assert (1 = 0) in assert false)
      | Same (_, _, _) ->
          let y,r' = extract_last r in y, Diff (x, l, r')
      | Diff (_, _, _) ->
          let y,r' = extract_last r  in y, Diff (x, l, r'))
  | Diff (x, l, r) -> let y,l' = extract_last l in y, Same (x, l', r)


(* removes the topmost element of the tree and inserts a new element [x] *)
let rec descent x h = match h with
  | Empty ->
      assert false
  | Same (y, l, r) ->
      (match l with
      | Empty ->
          (match r with
          | Empty -> (*y, Same(x, Empty, Empty)*) assert false
          | Same(_, _, _) -> let _ = assert (1 = 0) in assert false
          | Diff(_, _, _) -> let _ = assert (1 = 0) in assert false) 
      | Same(_, _, _) ->
          let (ml, l) = maximum l in
          let (mr, r) = maximum r in
          let _ = assert (ml != mr) in
            if x > ml && x > mr then
              (y, Same(x, l, r))
            else if ml > mr then
              let (z, l') = descent x l in
              let _ = assert (z = ml) in
              let _ = assert (max l' < ml) in
              let _ = assert (x < ml) in
              (y, Same (ml, l', r))
            else
              let (z, r') = descent x r in
              let _ = assert (z = mr) in
              let _ = assert (max r' < mr) in
              let _ = assert (x < mr) in
              (y, Same (mr, l, r'))
      | Diff(_, _, _) -> assert false)
  | _ -> assert false
  (*| Same (_, Empty, Empty) ->
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
      *)

(*
let remove h = match h with
  | Empty -> assert false (* raise EmptyHeap *)
  | Same (x, Empty, Empty) -> Empty
  | h -> let y,h' = extract_last h in descent y h'

let rec iter f h = match h with
  | Empty -> ()
  | Same (x, l, r) -> iter f l; f x; iter f r
  | Diff (x, l, r) -> iter f l; f x; iter f r

let rec fold f h x0 = match h with
  | Empty -> x0
  | Same (x, l, r) -> fold f l (fold f r (f x x0))
  | Diff (x, l, r) -> fold f l (fold f r (f x x0))
  *)

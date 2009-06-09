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

(* Heaps *)

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

let rec sz t = match t with
    Empty -> 0
  | Same (_, l, r) -> 1 + sz l + sz r
  | Diff (_, l, r) -> 1 + sz l + sz r

let max h = match h with
    Empty -> assert false
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

let ck_assume_empty h = match h with
  | Empty -> h
  | Same(_, _, _) -> let _ = assert (1=0) in assert false
  | Diff(_, _, _) -> let _ = assert (1=0) in assert false

(* returns the topmost element for set invariant *)
(* removes the topmost element of the tree and inserts a new element [x] *)
let rec descent x h = match h with
  | Empty ->
      let _ = assert (1 = 0) in assert false
  | Diff (y, l, r) ->
      (match r with
      | Empty ->
          (match l with
          | Same (z, ll', lr') ->
              let ll = ck_assume_empty ll' in let _ = assert (ll = ll') in
              let lr = ck_assume_empty lr' in let _ = assert (lr = lr') in
              if x > z then (y, Diff (x, Same (z, ll, lr), Empty))
              else (y, Diff (z, Same (x, Empty, Empty), Empty))
          | Diff (_, _, _) -> let _ = assert (1=0) in assert false
          | Empty -> let _ = assert (1=0) in assert false)
      | Diff (_, _, _) ->  (* same code (and proof) for Same and Diff. the only purpose of matching a pattern is to assert !Empty *)
          let (ml, l) = maximum l in
          let (mr, r) = maximum r in
          if x > ml && x > mr then
            (y, Diff (x, l, r))
          else if ml > mr then
            let (z, l') = descent x l in
              (y, Diff (ml, l', r))
          else
          let (z, r') = descent x r in
            (y, Diff (mr, l, r'))
      | Same (_, _, _) ->
          let (ml, l) = maximum l in
          let (mr, r) = maximum r in
          if x > ml && x > mr then
            (y, Diff (x, l, r))
          else if ml > mr then
            let (z, l') = descent x l in
              (y, Diff (ml, l', r))
          else
          let (z, r') = descent x r in
            (y, Diff (mr, l, r')))
  | Same (y, l, r) ->
      (match l with
      | Empty ->
          (match r with
          | Empty -> (y, Same(x, Empty, Empty))
          | Same(_, _, _) -> let _ = assert (1 = 0) in assert false
          | Diff(_, _, _) -> let _ = assert (1 = 0) in assert false) 
      | Diff (_, _, _) -> (* same symmetry as above *)
          let (ml, l) = maximum l in
          let (mr, r) = maximum r in
           if x > ml && x > mr then
	           (y, Same (x, l, r))
          else if ml > mr then
            let (z, l) = descent x l in
	            (y, Same(ml, l, r))
	        else
            let (z, r) = descent x r in
	            (y, Same (mr, l, r))
      | Same (_, _, _) ->
          let (ml, l) = maximum l in
          let (mr, r) = maximum r in
           if x > ml && x > mr then
	           (y, Same (x, l, r))
          else if ml > mr then
            let (z, l) = descent x l in
	            (y, Same(ml, l, r))
	        else
            let (z, r) = descent x r in
	            (y, Same (mr, l, r)))

let remove h = match h with
  | Empty -> let _ = assert (1 = 0) in assert false (* raise EmptyHeap *)
  | Same (x, l, r) ->
      (match l with
      | Empty ->
          (match r with
          | Empty -> (x, Empty)
          | Same (_, _, _) -> let _ = assert (1 = 0) in assert false
          | Diff (_, _, _) -> let _ = assert (1 = 0) in assert false)
      | Diff (_, _, _) -> let y,h' = extract_last h in descent y h'
      | Same (_, _, _) -> let y,h' = extract_last h in descent y h')
  | Diff (x, l, r) -> let y,h' = extract_last h in let _ = (fun x -> x) h' in descent y h'

let rec iter f = function
  | Empty -> ()
  | Same (x, l, r) -> iter f l; f x; iter f r
  | Diff (x, l, r) -> iter f l; f x; iter f r

let rec fold f h x0 = match h with
  | Empty -> x0
  | Same (x, l, r) -> fold f l (fold f r (f x x0))
  | Diff (x, l, r) -> fold f l (fold f r (f x x0))
  

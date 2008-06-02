(* 
   Based on an example of Rowan Davies and Frank Pfenning and Joshua Dunfield
*)

type 'a dict =
    E1
  | Black of 'a * 'a dict * 'a dict
  | Black 'a * dict * dict

  (* Representation Invariants
     1. The tree is ordered: for every node (Red|Black)(key1, left, right),
        every key in left is less than key1 and
        every key in right is greater than key1.
     2. The children of a red node are black (color invariant).
     3. Every path from the root to a leaf has the same number of
        black nodes, called the black height of the tree.
  *)

  let lookup dict k =
    let rec lk = function 
      | Empty -> false
      | Red (k', l, r) 
      | Black (k', l, r) -> 
          if k = k' then true 
          else if k < k' then lk l 
          else lk r in
    lk dict

  let restore_right = function 
    | (e, Red lt, Red (rt as (_,Red _,_))) -> 
         Red(e, Black lt, Black rt)     (* re-color *)

(* EXAMPLE BUG:  Black lt   instead, above.  Not caught: black height same as correct version. *)
(* EXAMPLE BUG:  Empty   instead, above.  Caught. *)
     
    | (e, Red lt, Red (rt as (_,_,Red _))) -> 
        Red(e, Black lt, Black rt)     (* re-color *)
  
    | (e, l, Red(re, Red(rle, rll, rlr), rr)) ->
         (* l is black, deep rotate *)
         Black(rle, Red(e, l, rll), Red(re, rlr, rr))

    | (e, l, Red(re, rl, rr as Red _)) ->
         (* l is black, shallow rotate *)
         Black(re, Red(e, l, rl), rr)

    | x -> x

    let restore_left = function
      ((e, Red (lt as (_,Red _,_)), Red rt)) ->
         Red(e, Black lt, Black rt)     (* re-color *)
    | ((e, Red (lt as (_,_,Red _)), Red rt)) ->
         Red(e, Black lt, Black rt)     (* re-color *)
    | ((e, Red(le, ll as Red _, lr), r)) ->
         (* r is black, shallow rotate *)
         Black(le, ll, Red(e, lr, r))
    | ((e, Red(le, ll, Red(lre, lrl, lrr)), r)) ->
         (* r is black, deep rotate *)
         Black(lre, Red(le, ll, lrl), Red(e, lrr, r))
    | x -> x

  let insert (dict, key) =
    let rec ins1 = function  
    | Empty -> Red(key, Empty, Empty)
    | Black(key1, left, right) ->
        if key = key1 then Black(key, left, right)  
        (* EXAMPLE BUG: change `Black(...)' to `left' 
           (see redblack-full-bug1.rml *)
        else if key < key1 then restore_left (Black(key1, ins1 left, right))
        else restore_right (Black(key1, left, ins1 right))
    | Red(key1, left, right) ->
        if key = key1 then Red(key, left, right)
        else if key < key1 then Red(key1, ins1 left, right)
        else Red(key1, left, ins1 right) in
    match ins1 dict with
    | Red (t as (_, Red _, _)) -> Black t (* re-color *)
    | Red (t as (_, _, Red _)) -> Black t (* re-color *)
    | dict -> dict                        (* depend on sequential matching *)

(* redblack-full.rml

   Based on an example of Rowan Davies and Frank Pfenning
*)

(*[ datatype dict with nat

    datasort dict : 
          badLeft <= dict; badRoot <= dict; badRight <= dict;
           rbt <= badLeft;  rbt <= badRoot; rbt <= badRight;
                      red <= rbt;   black <= rbt

    datacon Empty : black(0)

    datacon Black : 
            -all h : nat- int * dict(h) * dict(h) -> dict(h+1)
                       &  int * rbt(h) * rbt(h) -> black(h+1)
                       &  int * badRoot(h) * rbt(h) -> badLeft(h+1)
                       &  int * rbt(h) * badRoot(h) -> badRight(h+1)

    datacon Red :
            -all h : nat- int * dict(h) * dict(h) -> dict(h)
                       &  int * black(h) * black(h) -> red(h)
                       &  int * rbt(h) * black(h) -> badRoot(h)
                       &  int * black(h) * rbt(h) -> badRoot(h)
]*)
datatype dict =
    Empty
  | Black of int * dict * dict
  | Red of int * dict * dict
;
  (* Representation Invariants

     1. The tree is ordered: for every node (Red|Black)(key1, left, right),
        every key in left is less than key1 and
        every key in right is greater than key1.

     2. The children of a red node are black (color invariant).

     3. Every path from the root to a leaf has the same number of
        black nodes, called the black height of the tree.
  *)

  (*[ val  lookup : rbt -> int -> bool ]*)
  fun lookup dict key =
    let
      (*[ val lk : rbt -> bool
        val lk' : int * rbt * rbt -> bool ]*)
      fun lk dict =
          case dict of
              Empty => false
            | Red tree => lk' tree
            | Black tree => lk' tree
      and lk' (key1, left, right) =
          if key = key1 then true
          else if key < key1 then lk left
          else lk right
    in
        lk dict
    end

(*[ val restore_right : -all h : nat- badRight(h) -> rbt(h)
]*)
  (* restore_right (Black(e,l,r)) ==> dict
     where (1) Black(e,l,r) is ordered,
           (2) Black(e,l,r) has black height n,
           (3) color invariant may be violated at the root of r:
               one of its children might be red.
     and dict is a re-balanced red/black tree (satisfying all invariants)
     and same black height n. *)

  fun restore_right arg = case arg of
       Black(e, Red lt, Red (rt as (_,Red _,_))) =>
         Red(e, Black lt, Black rt)     (* re-color *)

(* EXAMPLE BUG:  Black lt   instead, above.  Not caught: black height same as correct version. *)
(* EXAMPLE BUG:  Empty   instead, above.  Caught. *)
     
    | Black(e, Red lt, Red (rt as (_,_,Red _))) =>
        Red(e, Black lt, Black rt)     (* re-color *)
  
    | Black(e, l, Red(re, Red(rle, rll, rlr), rr)) =>
         (* l is black, deep rotate *)
         Black(rle, Red(e, l, rll), Red(re, rlr, rr))

    | Black(e, l, Red(re, rl, rr as Red _)) =>
         (* l is black, shallow rotate *)
         Black(re, Red(e, l, rl), rr)

    | dict => dict

(* restore_left is like restore_right, except
     the color invariant may be violated only at the root of left child *)
(*[ val restore_left : -all h : nat- badLeft(h) -> rbt(h) ]*)
  fun restore_left arg = case arg of
      (Black(e, Red (lt as (_,Red _,_)), Red rt)) =>
         Red(e, Black lt, Black rt)     (* re-color *)
    | (Black(e, Red (lt as (_,_,Red _)), Red rt)) =>
         Red(e, Black lt, Black rt)     (* re-color *)
    | (Black(e, Red(le, ll as Red _, lr), r)) =>
         (* r is black, shallow rotate *)
         Black(le, ll, Red(e, lr, r))
    | (Black(e, Red(le, ll, Red(lre, lrl, lrr)), r)) =>
         (* r is black, deep rotate *)
         Black(lre, Red(le, ll, lrl), Red(e, lrr, r))
   | dict => dict

  (*[ val insert : rbt * int -> rbt ]*)
  fun insert (dict, key) =
    let
      (* val ins1 : dict -> dict  inserts entry *)
      (* ins1 (Red _) may violate color invariant at root *)
      (* ins1 (Black _) or ins (Empty) will be valid red/black tree *)
      (* ins1 preserves black height *)
      (*[ val ins1 : -all h : nat- rbt(h) -> badRoot(h)
                                  &  black(h) -> rbt(h)
      ]*)  (* the second conjunct is needed for the recursive cases *)
      fun ins1 arg = case arg of
          Empty => Red(key, Empty, Empty)
        | Black(key1, left, right) =>
            if key = key1 then Black(key, left, right)  (* EXAMPLE BUG: change `Black(...)' to `left' (see redblack-full-bug1.rml *)

            else if key < key1 then restore_left (Black(key1, ins1 left, right))
            else restore_right (Black(key1, left, ins1 right))
        | Red(key1, left, right) =>
            if key = key1 then Red(key, left, right)
            else if key < key1 then Red(key1, ins1 left, right)
            else Red(key1, left, ins1 right)
    in
      case ins1 dict
        of Red (t as (_, Red _, _)) => Black t (* re-color *)
         | Red (t as (_, _, Red _)) => Black t (* re-color *)
         | dict => dict                        (* depend on sequential matching *)
    end

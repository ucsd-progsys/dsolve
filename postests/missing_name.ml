(* 
   Based on an example of Rowan Davies and Frank Pfenning and Joshua Dunfield
*)

type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict

let restore_right arg = match arg with
  | Black (e, lt, Red (re, rl, rr)) ->
      begin match (rl, rr) with
        | (Red (rle, rll, rlr), _) ->
            (* l is black, deep rotate *)
            Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
        | (_, Red _) ->
            (*                   (\* l is black, shallow rotate *\) *)
            Black(re, Red(e, lt, rl), rr)
        | _ -> arg
      end
  | d -> d

let restore_left arg = (* match arg with *)
(*   | Black (e, Red (le, ll, lr), rt) -> *)
(*       begin match ((le, ll, lr), rt) with *)
(*         | ((_, Red _, _), Red (re, rl, rr)) -> *)
(*             Red (e, Black (le, ll, lr), Black (re, rl, rr))     (\* re-color *\) *)
(*         | ((_, _, Red _), Red (re, rl, rr)) -> *)
(*             Red (e, Black (le, ll, lr), Black (re, rl, rr))     (\* re-color *\) *)
(*         | _ -> *)
(*             begin match (ll, lr) with *)
(*               | (Red _, _) -> *)
(*                   (\* r is black, shallow rotate *\) *)
(*                   Black (le, ll, Red (e, lr, rt)) *)
(*               | (_, Red (lre, lrl, lrr)) -> *)
(*                   (\* r is black, deep rotate *\) *)
(*                   Black (lre, Red (le, ll, lrl), Red(e, lrr, rt)) *)
(*               | _ -> arg *)
(*             end *)
(*       end *)
(*   | d -> d *)
  assert false

let rec ins1 key d = match d with
  | Black(key1, left, right) ->
      if key = key1 then Black (key, left, right)  
      else if key < key1 then restore_left (Black (key1, ins1 key left, right))
      else restore_right (Black (key1, left, ins1 key right))
  | Red (key1, left, right) ->
      assert false
(*       if key = key1 then Black (key, left, right) *)
(*       else if key > key1 then Black (key1, ins1 key left, right) *)
(*       else Black (key1, left, ins1 key right)  *)
  | Empty -> Red (key, Empty, Empty)

let insert dict key =
  let dict = ins1 key dict in
    match dict with
      | Red (d, lt, rt) ->
          begin match lt with
            | Red _ -> Black (d, lt, rt) (* re-color *)
            | _     ->
(*                 begin match rt with *)
(*                   | Red _ -> Black (d, lt, rt) (\* re-color *\) *)
(*                   | _     -> dict *)
(*                 end *)
                assert false
          end
      | dict -> dict                                    (* depend on sequential matching *)

(* let rec checker = function *)
(*   | Empty -> () *)
(*   | Red (v, l, r) -> *)
(*       let _ = match l with *)
(*         | Empty -> () *)
(*         | Black (v', _, _) -> assert (v' <= v) *)
(*         | Red (v', _, _) -> assert (v' <= v) *)
(*       in *)
(*       let _ = match r with *)
(*         | Empty -> () *)
(*         | Black (v', _, _) -> assert (v' >= v) *)
(*         | Red (v', _, _) -> assert (v' >= v) *)
(*       in *)
(*       let _ = checker l; checker r in () *)
(*   | Black (v, l, r) -> *)
(*       let _ = match l with *)
(*         | Empty -> () *)
(*         | Black (v', _, _) -> assert (v' <= v) *)
(*         | Red (v', _, _) -> assert (v' <= v) *)
(*       in *)
(*       let _ = match r with *)
(*         | Empty -> () *)
(*         | Black (v', _, _) -> assert (v' >= v) *)
(*         | Red (v', _, _) -> assert (v' >= v) *)
(*       in *)
(*       let _ = checker l; checker r in () *)

let test_insert x x' =
  let a = insert Empty x in
  let b = insert a x' in
    ()
(*     checker b *)

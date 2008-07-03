(* 
   Based on an example of Rowan Davies and Frank Pfenning and Joshua Dunfield
*)

type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict
  | PurpleL of 'a * 'a dict * 'a dict
  | PurpleR of 'a * 'a dict * 'a dict

let restore_right arg = match arg with
  | Black (e, lt, PurpleL (re, rl, rr)) ->
      begin match (lt, (re, rl, rr)) with
        | (Red (le, ll, lr), (_, Red _, _)) ->
            Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
        | _ ->
            begin match (rl, rr) with
              | (Red (rle, rll, rlr), _) ->
                  (* l is black, deep rotate *)
                  Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
              | _ -> arg
            end
      end
  | Black (e, lt, PurpleR (re, rl, rr)) ->
      begin match (lt, (re, rl, rr)) with
        | (Red (le, ll, lr), (_, _, Red _)) ->
            Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
        | _ ->
            (* l is black, shallow rotate *)
            Black(re, Red(e, lt, rl), rr)
      end
  | d -> d

let restore_left arg = match arg with
  | Black (e, PurpleL (le, ll, lr), rt) ->
      begin match ((le, ll, lr), rt) with
        | ((_, Red _, _), Red (re, rl, rr)) ->
            Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
        | _ ->
            (* r is black, shallow rotate *)
            Black (le, ll, Red (e, lr, rt))
      end
  | Black (e, PurpleR (le, ll, lr), rt) ->
      begin match ((le, ll, lr), rt) with
        | ((_, _, Red _), Red (re, rl, rr)) ->
            Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
        | _ ->
            begin match (ll, lr) with
              | (_, Red (lre, lrl, lrr)) ->
                  (* r is black, deep rotate *)
                  Black (lre, Red (le, ll, lrl), Red(e, lrr, rt))
              | _ -> arg
            end
      end
  | d -> d

let rec ins1 key d = match d with
  | Black(key1, left, right) ->
      if key = key1 then Black (key, left, right)
      else if key < key1 then restore_left (Black (key1, ins1 key left, right))
      else restore_right (Black (key1, left, ins1 key right))
  | Red (key1, left, right) ->
      if key = key1 then Red (key, left, right)
      else if key < key1 then
        let l' = ins1 key left in
          (match l' with Red _ -> PurpleL (key, l', right) | _ -> Red (key, l', right))
      else
        let r' = ins1 key right in
          (match r' with Red _ -> PurpleR (key, left, r') | _ -> Red (key, left, r'))
  | Empty -> Red (key, Empty, Empty)
  | _ -> assert false (* todo: prove these don't happen *)

let insert dict key =
  let dict = ins1 key dict in
    match dict with
      | PurpleL (d, lt, rt) -> Black (d, lt, rt) (* re-color *)
      | PurpleR (d, lt, rt) -> Black (d, lt, rt) (* re-color *)
      | dict -> dict                             (* depend on sequential matching *)

(* 
   Based on an example of Rowan Davies and Frank Pfenning and Joshua Dunfield
*)

type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict
  | PurpleL of 'a * 'a dict * 'a dict
  | PurpleR of 'a * 'a dict * 'a dict

let color = function
  | Empty -> 0
  | Black (a, b, c) -> 1
  | Red (a, b, c) -> 2
  | PurpleL (a, b, c) -> 3
  | PurpleR (a, b, c) -> 4

let show x = x

let restore_right e lt r =
  match r with
  | PurpleL (re, rl, rr) ->
      begin match lt with
        | Red (le, ll, lr) ->
            begin match rl with
              | Red _ ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            begin match rl with
              | Red (rle, rll, rlr) ->
                  (* l is black, deep rotate *)
                  Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | PurpleL _ -> assert (0 = 1); assert false
        | PurpleR _ -> assert (0 = 1); assert false
      end
  | PurpleR (re, rl, rr) ->
      begin match lt with
        | Red (le, ll, lr) ->
            begin match rr with
              |  Red _ ->
                   Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            (* l is black, shallow rotate *)
            Black(re, Red(e, lt, rl), rr)
        | PurpleL _ -> assert (0 = 1); assert false
        | PurpleR _ -> assert (0 = 1); assert false
      end
  | Red _ -> Black (e, lt, r)
  | Black _ -> Black (e, lt, r)

let restore_left e l rt =
  match l with
  | PurpleL (le, ll, lr) ->
      begin match rt with
        | Red (re, rl, rr) ->
            begin match ll with
              | Red _ ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            (* r is black, shallow rotate *)
            Black (le, ll, Red (e, lr, rt))
        | PurpleL _ -> assert (0 = 1); assert false
        | PurpleR _ -> assert (0 = 1); assert false
      end
  | PurpleR (le, ll, lr) ->
      begin match rt with
        | Red (re, rl, rr) ->
            begin match lr with
              | Red _ ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            begin match lr with
              | Red (lre, lrl, lrr) ->
                  (* r is black, deep rotate *)
                  Black (lre, Red (le, ll, lrl), Red(e, lrr, rt))
              | Black _ -> assert (0 = 1); assert false
              | PurpleL _ -> assert (0 = 1); assert false
              | PurpleR _ -> assert (0 = 1); assert false
            end
        | PurpleL _ -> assert (0 = 1); assert false
        | PurpleR _ -> assert (0 = 1); assert false
      end
  | Red _ -> Black (e, l, rt)
  | Black _ -> Black (e, l, rt)

(*
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
*)

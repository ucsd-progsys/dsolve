type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict
  | Purple of 'a * 'a dict * 'a dict

let color = function
  | Empty -> 0
  | Black (a, b, c) -> 1
  | Red (a, b, c) -> 2
  | Purple (a, b, c) -> 3
(*let rec height = function
  | Empty -> 0
  | Red (_, l, r) -> (max (height l) (height r))
  | Black (_, l, r) -> (max (height l) (height r)) + 1
  | Purple (_, l, r) -> (max (height l) (height r))*)

let restore_right e lt r =
  match r with
  | Purple (re, rl, rr) ->
      begin match lt with
        | Red (le, ll, lr) ->
            begin match rl with
              | Red _ ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _   ->
                  begin match rr with
                    |  Red _ ->
                         Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
                    | Black _   -> assert (0 = 1); assert false
                    | Purple _ -> assert (0 = 1); assert false
                  end
              | Purple _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            begin match rl with
              | Red (rle, rll, rlr) ->
                  Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
              | Black _   ->
                  Black(re, Red(e, lt, rl), rr)
              | Purple _ -> assert (0 = 1); assert false
            end
        | Purple _ -> assert (0 = 1); assert false
      end
  | Red _   -> Black (e, lt, r)
  | Black _ -> Black (e, lt, r)

let restore_left e l rt =
  match l with
  | Purple (le, ll, lr) ->
      begin match rt with
        | Red (re, rl, rr) ->
            begin match lr with
              | Red _ ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black _   ->
                  begin match ll with
                    | Red _ ->
                        Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
                    | Black _   -> assert (0 = 1); assert false
                    | Purple _ -> assert (0 = 1); assert false
                  end
              | Purple _ -> assert (0 = 1); assert false
            end
        | Black _ ->
            begin match lr with
              | Red (lre, lrl, lrr) ->
                  Black (lre, Red (le, ll, lrl), Red(e, lrr, rt))
              | Black _   ->
                  Black (le, ll, Red (e, lr, rt))
              | Purple _ -> assert (0 = 1); assert false
            end
        | Purple _ -> assert (0 = 1); assert false
      end
  | Red _   -> Black (e, l, rt)
  | Black _ -> Black (e, l, rt)

let rec ins1 key d = match d with
  | Black(key1, left, right) ->
      if key = key1 then Black (key, left, right)
      else if key < key1 then restore_left key1 (ins1 key left) right
      else restore_right key1 left (ins1 key right)
  | Red (key1, left, right) ->
      if key = key1 then Red (key, left, right)
      else if key < key1 then
        let l' = ins1 key left in
          begin match l' with
            | Red _     -> Purple (key1, l', right)
            | Black _   -> Red (key1, l', right)
            | Empty     -> Red (key1, l', right)
            | Purple _ -> assert (0 = 1); assert false
          end
      else
        let r' = ins1 key right in
          begin match r' with
            | Red _     -> Purple (key1, left, r')
            | Black _   -> Red (key1, left, r')
            | Empty     -> Red (key1, left, r')
            | Purple _ -> assert (0 = 1); assert false
          end
  | Empty     -> Red (key, Empty, Empty)
  | Purple _ -> assert (0 = 1); assert false

let insert dict key =
  let dict = ins1 key dict in
    match dict with
      | Purple (d, lt, rt) -> Black (d, lt, rt) (* re-color *)
      | Red (d, lt, rt)     -> Red (d, lt, rt)
      | Black (d, lt, rt)   -> Black (d, lt, rt)
      | Empty               -> Empty

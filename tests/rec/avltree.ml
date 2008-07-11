type 'a avl = 
  | E 
  | Bl of 'a * 'a avl * 'a avl * int (* lh bigger *)
  | Br of 'a * 'a avl * 'a avl * int (* rh bigger *)

let rec len = function
  | E -> 0
  | Bl (_, l, r, h) -> 1 + len l + len r
  | Br (_, l, r, h) -> 1 + len l + len r

let rec height = function
  | E -> 0
  | Bl (_, _, _, h) -> h
  | Br (_, _, _, h) -> h



let left_rotate e l r =
  match l with
  | E -> assert false
  | Bl (le, ll, lr, lh) ->
      let lrh = height lr in
      Br (le, ll, Bl (e, lr, r, lrh+1), lrh+2)
  | Br (le, ll, lr, lh) ->
      let llh = height ll in
      let lrh = height lr in
      if llh < lrh then 
        match lr with
        | E -> assert false
        | Bl (lre, lrl, lrr, _) -> 
            Br (lre, Bl (le, ll, lrl, llh+1), Br (e, lrr, r, lh-1), lh)
        | Br (lre, lrl, lrr, _) ->
            Br (lre, Bl (le, ll, lrl, llh+1), Br (e, lrr, r, lh-1), lh)
      else Br (le, ll, Bl (e, lr, r, lrh+1), lrh+2)

let right_rotate e l r =
  match r with
  | E  -> assert false
  | Br (re, rl, rr, rh) ->
      let rlh = height rl in
      Bl (re, Br (e, l, rl, rlh+1), rr, rlh+2)
  | Bl (re, rl, rr, rh) ->
      let rlh = height rl in
      let rrh = height rr in
      if rlh > rrh then 
        match rl with
        | E -> assert false
        | Bl (rle, rll, rlr, _) ->
            Bl (rle, Bl (e, l, rll, rh-1), Br (re, rlr, rr, rrh+1), rh)
        | Br (rle, rll, rlr, _ ) -> 
            Bl (rle, Bl (e, l, rll, rh-1), Br (re, rlr, rr, rrh+1), rh)
      else Bl (re, Br (e, l, rl, rlh+1), rr, rlh+2)
  
exception Item_is_found

let rec insert e t = 
  match t with 
  | E -> Bl (e, E, E, 1)
  | Bl (e', l, r, h) ->
      if e < e' then
        let l' = insert e l in
        let lh' = height l' in
        let rh = height r in
        if lh' <= rh then Br (e', l', r, rh+1)
        else if lh' <= rh+1 then Bl (e', l', r, lh'+1)
        else left_rotate e' l' r
      else if e > e' then
        let lh = height l in
        let r' = insert e r in
        let rh' = height r' in
        if rh' <= lh then Bl (e', l, r', lh+1)
        else Br (e', l, r', rh'+1)
      else t
  | Br (e', l, r, h) ->
      if e < e' then 
        let l' = insert e l in
        let lh' = height l' in
        let rh = height r in
        if lh' <= rh then Br (e', l', r, rh+1)
        else Bl (e', l', r, lh'+1)
      else if e > e' then 
        let lh = height l in
        let r' = insert e r in
        let rh' = height r' in
        if rh' <= lh then Bl (e', l, r', lh+1)
        else if rh' <= lh+1 then Br (e', l, r', rh'+1)
        else right_rotate e' l r'
      else t

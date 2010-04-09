type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict

let restore_right arg = match arg with
  | Black (e, lt, Red (re, rl, rr)) -> Black(re, Red(e, lt, rl), rr)
  | d -> assert false

let rec ins1 key d = match d with
  | Black(key1, left, right) -> restore_right (Black (key1, left, ins1 key right))
  | Red _                    -> assert false
  | Empty                    -> Red (key, Empty, Empty)

let insert dict key =
  let dict = ins1 key dict in
    match dict with
      | Red (d, lt, rt) -> Black (d, lt, rt)
      | dict -> assert false

let test_insert x x' =
  let a = insert Empty x in
  let b = insert a x' in
    ()

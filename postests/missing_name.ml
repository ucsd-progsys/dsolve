type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let restore_right arg = match arg with
  | Black ( lt, Red (rl, rr)) -> Black(Red(lt, rl), rr)
  | d -> assert false

let rec ins1 key d = match d with
  | Black(left, right) -> restore_right (Black (left, ins1 key right))
  | Red _              -> assert false
  | Empty              -> Red (Empty, Empty)

let insert dict key =
  let dict = ins1 key dict in
    match dict with
      | Red (lt, rt) -> Black (Empty, rt)
      | dict -> assert false

let test_insert x x' =
  let a = Black (Empty, Empty) in
  let b = insert a x' in
    ()

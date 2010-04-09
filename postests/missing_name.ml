type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let restore_right arg = match arg with
  | Black (lt, Red (rl, rr)) -> Black(Red(lt, rl), Empty)
  | d -> assert false

let rec ins1 d = match d with
  | Black(left, right) -> restore_right (Black (left, ins1 right))
  | _                  -> Red (Empty, Empty)

let dict = ins1 (Black (Empty, Empty))
let _ =
  match dict with
    | Red (_, rt) -> rt
    | dict        -> assert false

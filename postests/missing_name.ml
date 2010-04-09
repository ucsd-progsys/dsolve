type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let restore_right arg = match arg with
  | Black (lt, Red (rl, _)) -> Black (Red (lt, rl), Empty)
  | _                       -> assert false

let rec ins1 d = match d with
  | Black (_, right) -> restore_right (Black (Empty, ins1 right))
  | _                -> Red (Empty, Empty)

let _ = ins1 (Black (Empty, Empty))

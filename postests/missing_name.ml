(* DSOLVE -dontgenmlq -no-anormal *)

type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let rec ins d =
  if d then
    match Black (Empty, ins false) with
      | Black (lt, Red (rl, _)) -> Black (Red (lt, rl), Empty)
  else
    Red (Empty, Empty)

let _ = ins true

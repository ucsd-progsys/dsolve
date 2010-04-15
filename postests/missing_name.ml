(* DSOLVE -dontgenmlq -no-anormal *)

type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let rec ins d =
  if d then
    match Black (Empty, ins false) with
      | Black (_, Red (rl, lt)) -> Red (lt, rl)
  else
    Red (Empty, Empty)

let _ = ins true

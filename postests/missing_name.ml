(* DSOLVE -dontgenmlq -no-anormal *)

type dict =
    Empty
  | Black of dict * dict

let rec ins d =
  if d then
    match Black (Empty, ins false) with
      | Black (_, Black (rl, lt)) -> Black (lt, rl)
  else
    Black (Empty, Empty)

let _ = ins true

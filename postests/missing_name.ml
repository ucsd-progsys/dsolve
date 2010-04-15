(* DSOLVE -dontgenmlq -no-anormal *)

type dict =
    Empty
  | Black of dict * dict

let _ =
  match Black (Empty, Black (Empty, Empty)) with
    | Black (_, Black (rl, lt)) -> Black (lt, rl)

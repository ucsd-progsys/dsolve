type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let rec ins d =
  if d > 0 then
      begin match Black (Empty, ins (d - 1)) with
        | Black (lt, Red (rl, _)) -> Black (Red (lt, rl), Empty)
      end
  else
    Red (Empty, Empty)

let _ = ins 10

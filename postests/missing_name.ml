type dict =
    Empty
  | Black of dict * dict
  | Red of dict * dict

let rec ins d = match d with
  | Black (_, right) ->
      begin match (Black (Empty, ins right)) with
        | Black (lt, Red (rl, _)) -> Black (Red (lt, rl), Empty)
      end
  | _ -> Red (Empty, Empty)

let _ = ins (Black (Empty, Empty))

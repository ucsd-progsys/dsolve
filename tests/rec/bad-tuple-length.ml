let rec length x = match x with
  | []      -> 0
  | _ :: xs -> 1 + length xs

let rec testlength xs = match (xs, 3) with
  | (x :: y :: zs, _) ->
      let s = length zs in
      assert (length xs >= 3)
  | _ -> ()

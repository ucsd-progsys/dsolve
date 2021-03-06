let rec length x = match x with
  | []      -> 0
  | _ :: xs -> 1 + length xs

let testlength xs = match xs with
  | x :: y :: zs ->
      let s = length zs in
      assert (length xs >= 3)
  | _ -> ()

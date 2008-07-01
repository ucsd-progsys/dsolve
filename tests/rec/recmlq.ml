let rec len xs =
  match xs with
  | [] -> 0
  | x::xs' -> 1 + len xs'

let test xs =
  assert (len xs = 15);
  match xs with
  | [] -> ()
  | x :: xs ->
      match xs with
        | [] -> ()
        | y :: _ -> assert (y > x)

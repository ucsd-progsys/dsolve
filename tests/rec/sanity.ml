let _ = assert (1=2) (* NOK *)

let rec len xs =
  match xs with
      [] -> 0
    | x :: ss -> 1 + len ss

let xs = [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]

let _ = assert (len xs = 15) (* line 10 OK *)
let _ = assert (len xs = 14) (* line 11 NOK *)

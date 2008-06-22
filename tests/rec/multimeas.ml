let rec len x =
  match x with
    | [] -> 0
    | x :: xs -> 1 + len xs

let rec llen x =
  match x with
    | [] -> 0
    | x :: xs -> len x + llen xs

let check xs =
  llen xs

let ys = [1;2;3;4]
let zs = [ys;ys;ys]
           
let _ = assert (len ys = 4)
let _ = assert (len ys = 5) (* NOK *)
let _ = assert (llen zs = 12)
let _ = assert (llen zs = 13) (* NOK *)

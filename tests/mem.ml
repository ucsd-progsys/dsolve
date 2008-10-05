let f xs = match xs with
  | y::ys -> assert (Mylist.mem y xs)
  | []    -> ()

let xs1 = []

let xs2 = 1 :: xs1
let s = assert (Mylist.mem 1 xs2)

let t = (fun (x: int list) -> x) xs2

let xs3 = 2 :: xs2

let t = (fun (x: int list) -> x) xs3

let s = assert (Mylist.mem 1 xs3)
let s = assert (Mylist.mem 2 xs3)


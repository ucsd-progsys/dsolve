let rec len (x: int list) = 
  match x with
      [] -> 0 
    | s :: ss -> 1 + (len ss)

let y = [] 
let z = 1 :: y
let zzz = 1 :: z
let b = (fun x -> x) len

let b = assert (len z = 1)
let b = assert (len y = 0)
let b = assert (len zzz = 2)
let b = assert (len zzz = 1)


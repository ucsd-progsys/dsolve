let rec len (x: int list) = 
  match x with
      [] -> 0 
    | s :: ss -> 1 + (len ss)

let y = []
let zz = (fun x -> x) (len y)
let z = 1 :: y
let zzz = 1 :: z

let x = (fun x -> x) (len z)
 
let blah = assert (len z = 1)
let blah2 = assert (len y = 0)
let blah3 = assert (len zzz = 2)
(*let blah4 = assert (len zzz = 1a)*)


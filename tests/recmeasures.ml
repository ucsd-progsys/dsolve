let rec len (x: int list) = 
  match x with
      [] -> let x = 0 in x + 0
    | s :: ss -> 1 + (len ss)

let y = []
let zz = (fun x -> x) (len y)
let z = 1 :: y

let x = (fun x -> x) (len z)
 
(*let blah = assert (len y = 0)*)


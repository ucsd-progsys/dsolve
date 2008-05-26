let rec len (x: int list) =
  match x with
      [] -> let x = 0 in x
    | s :: ss -> 1 + (len ss)

let y = 1 :: 2 :: []

let x = (fun x -> x) y
 
let blah = assert (len y = 2)


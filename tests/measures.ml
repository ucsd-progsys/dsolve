let h (x: int option) = 
  match x with
    Some u -> 1
  | None -> 0

let x = Some 1
let y = None

let c = (fun x -> x) (h x)
let x' = (fun x -> x) h x
let y' = (fun x -> x) (h y)

let z = assert (h x = 1)

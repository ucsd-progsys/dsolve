let h (x: int option) = 
  match x with
    Some u -> let y = 1 in let y = (fun x -> x) y in y
  | None -> let y = 0 in y

let x = Some 1
let y = None

let c = (fun x -> x) h x
let x' = (fun x -> x) h x
let y' = (fun x -> x) h y

let z = assert (h x = 1)

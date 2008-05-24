let h (x: int option) = 
  match x with
    Some x -> 1
  | None -> 0

let x = Some 1
let y = None

(*let c = (fun x -> x) h
let x = (fun x -> x) x*)

let z = assert (h x = 1)

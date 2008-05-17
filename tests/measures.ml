(*let h (x: int option) = let y = 5 in 2/y

let a x = 
  let y = Some 1 in
  let yy = (fun x -> x) in
  let y = yy y in
  let z = yy h in
    0(* assert (h y = 1)*)*)
let h (x: int option) = let y = 5 in 2/y
let x = Some 1

let z = assert (h x = 1)

(*let i = None
let j = 5
let b = false 
let a = "fat"*)

let g (x: int) = x
let y = Dep.f g

let z = Dep.f

let f p = 1
let h = f g


let hh = Dep.g


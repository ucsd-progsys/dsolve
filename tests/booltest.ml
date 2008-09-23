let b = true
let _ = assert (b)
let b = 1 = 1
let _ = assert (b)
let _ = if true then assert (true) else assert (false)
let _ = if false then assert (false) else assert (true)
let _ = if 1 = 1 then assert (true) else assert (false)
let _ = assert (1=1)
let c = (fun x -> x) 1 = 1

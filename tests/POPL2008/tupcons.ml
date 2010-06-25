let ys = 0::[]
let _  = match ys with y::[] -> assert (y >= 0)
let xs = (0,0)::[]
let _  = match xs with q::[] -> let (c,d) = q in assert (c >= 0)
let _  = List.iter (fun p -> let (a,b) = p in assert (a >= 0)) xs

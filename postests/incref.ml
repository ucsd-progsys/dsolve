
let x = ref 0
let _ = x := !x (* comment this out and it works *) 
let _ = assert (!x >= 0)

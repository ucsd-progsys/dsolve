let inc x = x + 1

let p = if 0 < 1 then assert false else inc

let _ = p

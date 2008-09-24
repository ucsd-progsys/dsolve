let show x = ()

let rec inc x = 
  if read_int () < 0 
  then [] 
  else  x::(inc (x+1))

let t1 = inc 10

let _  = match t1 with [] -> () | a::b -> let _ = show a in let _ = show b in ()



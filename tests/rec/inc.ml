let rec inc x = 
  if read_int () < 0 
  then [] 
  else  x::(inc (x+1))

let _ = inc 10

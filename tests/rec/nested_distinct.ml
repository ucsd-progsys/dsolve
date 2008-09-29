type 'a nl = C of 'a * 'a bl  
and  'a bl = P of 'a * 'a nl | N

let rec make n = 
  if n < 0 then [] else n :: make (n-1)

let rec maken n = 
  C (n, makeb (n-1)) 
and makeb n = 
  if n < 0 then N else P (n, maken (n-1))

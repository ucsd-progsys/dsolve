type var = string

type indexedvar = var * int * int 

type constr = 
  | Bottom of var * int * int   (* x[i:j] = \bot *)

let refine b g t c = 
  match c with
  | Bottom (x,i,j) -> 
      (b, g, t) 

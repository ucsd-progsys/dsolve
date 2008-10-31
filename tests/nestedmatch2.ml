type var = string
type indexedvar = var * int * int 
type constr = 
  | Bottom of indexedvar	                (* x[i:j] = \bot *)
  | IndexedEq of indexedvar * indexedvar        (* x[i:j] = x'[i':j'] *)
  | IndexedLeq of indexedvar * indexedvar       (* x[i:j] <= x'[i':j'] *)

let refine b g t c = 
  match c with
  | Bottom (x,i,j) -> 
      (b, g, t) 
  | IndexedEq (z,z') ->
      (b, g, t)
  | IndexedLeq (z,z') ->
      (b, g, t)

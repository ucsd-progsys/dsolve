let rec unb arr2 m n i j =
  if i = 0 then
    if j < n-1 then
      if Bigarray.Array2.get arr2 0 j < 0 then unb arr2 m n (i+1) j
      else unb arr2 m n 0 (j+1)
    else false
  else
    if i < m then
      if Bigarray.Array2.get arr2 i j < 0 then unb arr2 m n (i+1) j
      else unb arr2 m n 0 (j+1)
    else true
in

(Random.self_init ();
 let arr =
   Bigarray.Array2.create
     Bigarray.int Bigarray.c_layout
     (Random.int 10 + 1) (Random.int 20 + 1) in
 let m = Bigarray.Array2.dim1 arr in
 let n = Bigarray.Array2.dim2 arr in
   unb arr m n 0 1);;

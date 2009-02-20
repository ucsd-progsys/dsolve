let check (m, fs) =
  assert (Mystore.get m (List.hd fs) = 0)

let (m, fs) = 
  let a  = Mystore.make 255 0 in
  let fs = if Mystore.get a 0 = 0 then 0 :: [] else [] in
    (a, fs)

let _ = check (m, fs)

(* Oddly, this one is ok!

  let x = ...

  let _ = check x

*)

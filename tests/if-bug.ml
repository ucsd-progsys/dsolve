let show = fun x -> x

let rec spin x = spin x

let f0 () = 0

let make_int n =
  if n = 0 then show (* comment out "show" and assert fails! *)  (f0 ()) else
      spin ()

let _ = show make_int

let check n = 
  let _ = assert (n = make_int n) in
  ()


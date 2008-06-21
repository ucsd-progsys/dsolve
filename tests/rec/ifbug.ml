let rec spin () = spin ()

let rec y x =
  let f () = x + 1 in
  if x = 0 then f () else
    if x = 1 then f () else
      spin ()

let check x = 
  assert (y x = x + 1)

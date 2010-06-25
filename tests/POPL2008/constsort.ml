let show x = x 

let g x = 
  let x1 = x + 1 in
  let x2 = x + 2 in
  let x3 = x + 3 in
  let x4 = x + 4 in
  let xs = [] in
  let xs = x4::[] in
  let xs = x3::xs in
  let xs = x2::xs in
  let xs = x1::xs in
  let _  = match xs with x1::t -> (match t with x2::t' ->  assert (x1 < x2)) in
  xs

let rec f1 x =
  if read_int () > 10 then [] else 
    let xs = x::(f1 (x+1)) in
    let _  = match xs with x1::t -> (match t with x2::t' ->  assert (x1 < x2)) in
    xs

let f a =
  let rv = 
    let x = read_int () in 
    (x, f1 x) in
  rv

let test x = 
  let (y,ys) = f () in
  let _  = 
    match ys with y1::y2::ys' -> 
      let _ = show y1 in
      let _ = show y2 in
      let _ = show ys' in 
      let _ = List.iter (fun p -> assert ((show p) != y1)) ys' in
      let _ = List.iter (fun p -> assert ((show p) != y2)) ys' in
      () in
  ()
  (* | y::ys'      -> let _ = show ys' in
                   let _ = List.iter (fun p -> assert ((show p) != y)) ys' in () 
                   *)

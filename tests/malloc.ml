let show x = x

let rec del x ys = 
  match ys with
  | []          -> []
  | y::ys'      -> if x = y then ys' else y::(del x ys')

let rec checkdiff xs = 
  match xs with 
  | []          -> () 
  | x::xs'      -> let _ = List.iter (fun x' -> assert (x != x')) xs' in
                   checkdiff xs'

let check x =
  let (m, us, fs) = x in
  checkdiff us;
  checkdiff fs;
  List.iter (fun p -> assert (Mystore.get m p != 0)) us;
  List.iter (fun p -> assert (Mystore.get m p = 0)) fs

let init size = 
  let a   = Mystore.make 255 0 in
  let _   = assert (0 = Mystore.get a 5) in
  let rec f k us fs = 
    if k >= size then (a, us, fs) else
      if Mystore.get a k = 0 
      then (assert (Mystore.get a k = 0); f (k+1) us (k::fs))
      else (assert (Mystore.get a k != 0); f (k+1) (k::us) fs) in
  let w   = f 0 [] [] in
  let _   = check w in
  w

let malloc x =
  let (mem, us, fs) = x in
  let (f, w') =
    match fs with
    | []          -> assert false
    | f::fs'      -> let mem' = Mystore.set mem f 1 in
                     let w'   = (mem', f::us, fs') in
                     let _    = check w' in
                     let _    = show w' in
                     (f, w') in
  let _ = check w' in 
  (f, show w')

let free x p =
  let (mem, us, fs) = x in
  if (Mystore.get mem p != 0) then
    let mem' = Mystore.set mem p 0 in
    let us'  = del (show p) (show us) in
    (mem', us', p::fs) 
  else (mem, us, fs)

let _ =
  let (m,us,fs) = init 1000 in
  let _         = show fs in
  let fs'       = del (read_int ()) fs in
  ()

let _ =
  let (m,us,fs) = init 1000 in
  let _         = check (m, us, fs) in
  let (_, w')   = malloc (m, us, fs) in
  let _         = check w' in 
  let w''       = free w' (read_int ()) in
  let _         = check w'' in
  ()

let main =
  let rec spin w = 
    let ch = read_int () in
    if ch = 0 then 
      let (_,w') = malloc w in
      spin w'
    else 
      let w' = free w (read_int ()) in
      spin w' in
  spin (init 1000)

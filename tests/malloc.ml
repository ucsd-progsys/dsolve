let show x = x

(* to appease the Z3 gods *)
let test0 = 
  let a = Mystore.make 10 0 in
  assert (0 = Mystore.get a 5)

  (*
let xs0 = []
let xs1 = 10::xs0
let xs2 =  9::xs1
let xs3 =  8::xs2

let _   = show xs0
let _   = show xs1
let _   = show xs2
let _   = show xs3
*)

let make_list m =
  let rec f n xs = 
    if n >= 0 then f (n-1) (n::xs) else xs in
  f m []

let rec insert x ys = 
  match ys with
  | []          -> [x]
  | y::ys'      -> if (read_int () > 0) then x::y::ys' else y::(insert x ys')

let rec shuffle xs = 
  match xs with
  | []          -> []
  | x::xs'      -> insert x (shuffle xs')

let make_shuffled n = 
  shuffle (make_list n)

(*************************************************************************)
let rec diverge x = diverge x

let rec del x ys = 
  match ys with
  | []          -> []
  | y::ys'      -> if x = y then del x ys' else y::(del x ys)

let check (m, us, fs) =
  List.iter (fun p -> assert (Mystore.get m p != 0)) us;
  List.iter (fun p -> assert (Mystore.get m p = 0)) fs;
  match fs with f1::f2::fs' ->
    let () = assert (f1 != f2) in
    let () = assert (Mystore.get m  f1 = 0) in
    let () = assert (Mystore.get m  f2 = 0) in
    let m' = Mystore.set m f1 1 in
    let () = assert (Mystore.get m' f2 = 0) in
    (m, us, fs)

let init size = 
  let a   = Mystore.make 255 0 in
  let _   = assert (0 = Mystore.get a 5) in
  let rec f k us fs = 
    if k >= size then check (a, us, fs) else
      if Mystore.get a k = 0 
      then (assert (Mystore.get a k = 0); f (k+1) us (k::fs))
      else (assert (Mystore.get a k != 0); f (k+1) (k::us) fs) in
  f 0 [] []

let malloc (mem, us, fs) = 
  match fs with
  | []          -> diverge ()
  | f::fs'      -> let mem' = Mystore.set mem f 1 in
                   (f, (mem', f::us, fs'))
  
let (m,us,fs)   = init 1000
let _           = show fs 
let _           = 
  match fs with f1::f2::fs' ->
    let _  = show f1 in
    let _  = show f2 in
    let () = assert (f1 != f2) in
    let () = assert (Mystore.get m  (show f1) = 0) in
    let () = assert (Mystore.get m  f2 = 0) in
    let m' = Mystore.set m f1 1 in
    let () = assert (Mystore.get m' f2 = 0) in
    ()



(*
let p,(m,us,fs) = malloc w 
let _           = show p
let _           = show m
let _           = show us
let _           = show fs

let free (mem, us, fs) p =
  if (Mystore.get mem p != 0) then
    let mem' = Mystore.set mem p 0 in
    let us'  = del (show p) (show us) in
    (mem', us', p::fs) 
  else (mem, us, fs)

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

(* For your viewing pleasure *)
let _ = make_shuffled
let _ = init
let _ = malloc
let _ = free

*)

let (_,_,fs) = init 1000
let fs'      = del 10 fs
let fs''     = del 20 fs'
let _        = fs''



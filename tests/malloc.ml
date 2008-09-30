let show x = x
let xs0 = []
let xs1 = 10::xs0
let xs2 =  9::xs1
let xs3 =  8::xs2


let _   = show xs0
let _   = show xs1
let _   = show xs2
let _   = show xs3

let size   = 1000

let make_list m =
  let rec f n xs = 
    if n >= 0 then f (n-1) (n::xs) else xs in
  f m []

let _ = make_list 100

(*
let rec init i us fs = 
  let mem = Mystore.make size 0 in
  let rec f i us fs = 
    if i >= size then (us, fs, mem) 
    else if mem.(i) then init (i+1) (i::us) fs 
    else init (i+1) us (i::fs) 

let us,fs                               = init 0 [] []
let useds (*: {not mem.(v)} list ref *) = ref us 
let frees (*: {mem.(v)}     list ref *) = ref fs 

let malloc () = 
  match !frees with 
  | [] -> assert false 
  | p::ps -> 
      let _ = frees := ps in
      let _ = Array.set mem p true in
      let _ = useds := p::!useds in p

let rec del x = function 
  | []          -> []
  | y::ys       -> if x=y then del x ys else y::(del x ys)

let free p = 
  if mem.(p) then
    let _ = useds:= del p !useds in
    let _ = Array.set mem p false in
    let _ = frees:= p::!frees in ()
    *)

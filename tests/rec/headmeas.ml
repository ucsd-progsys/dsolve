let show x = x

let head xs = 
  match xs with
  | []  -> 0
  | h::_-> h

let _ = head
(*
let bob0 = ([] : int list)
let _    = show bob0
let bob1 = 1::bob0
let _    = show bob1
*)

let rec make n = 
  if n <= 0 then 
    let t = [] in
    let _ = show t in
    t
  else 
    let t = make (n-1) in
    let _ = show t in
    (n :: t)

let _ = make

let _ = 
  let n0 = read_int () in
  if n0 > 0 then
    let l0 = make n0 in
    assert (n0 >= head l0)
  else ()

  (*
let rec filter (xs : int list) =
  match xs with
  | []          -> show []
  | h::t        -> let _ = show t in 
                   if read_int () > 0 then 
                     let s = filter t in
                     let _ = show s in
                     h::s 
                   else 
                     let s = filter t in
                     let _ = show s in
                     s

let _ = 
  let xs = make 100 in
  let ys = filter xs in
  show ys
  *)


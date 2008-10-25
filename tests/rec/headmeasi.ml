type ilist = Nil | Cons of int * ilist

let show x = x

let head xs = 
  match xs with
  | Nil         -> 0
  | Cons (h,_)  -> h

let _ = head

let rec checker xs = 
  match xs with
  | Nil -> 
      ()
  | Cons (h,t) -> 
      let _ = assert (h > head t) in
      checker t

let rec make n = 
  if n <= 0 then 
    let t = Nil in
    let _ = show t in
    t
  else 
    let t = make (n-1) in
    let _ = show t in
    Cons (n,t)

let _ = make

let _ = 
  let n0 = read_int () in
  if n0 > 0 then
    let l0 = make n0 in
    assert (n0 >= head l0)
  else ()

let rec filter xs =
  match xs with
  | Nil         -> show Nil 
  | Cons(h,t)   -> let _ = show t in 
                   if read_int () > 0 then 
                     let s = filter t in
                     let _ = show s in
                     Cons (h,s) 
                   else 
                     let s = filter t in
                     let _ = show s in
                     s

let _ = 
  let xs = make 100 in
  let ys = filter xs in
  let _  = show ys in
  checker ys



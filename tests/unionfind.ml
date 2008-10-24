

let create n = 
  (Pa.init n (fun i -> 1), (* rank *) 
   Pa.init n (fun i -> i)) (* parent *)

let rec find_aux p i = 
  let pi = Pa.get p i in
  if pi == i then 
    (p, i)
  else 
    let p', i' = find_aux p pi in 
    let p''    = Pa.set p' i i' in
    (p'', i')
      
let find h i =
  let (p,_)  = h in
  let p',i'  = find_aux p i in 
  (p', i')
  
let union h x y =
  let (p,_) = h in
  let (p', rx) = find p  x in
  let (p'',ry) = find p' y in
  (* HERE *)
  if rx != ry then begin
    let rxc = Pa.get h.c rx in
    let ryc = Pa.get h.c ry in
    if rxc > ryc then
      { h with father = Pa.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Pa.set h.father rx ry }
    else
      { c = Pa.set h.c rx (rxc + 1);
	father = Pa.set h.father ry rx }
  end else
    h

(*
(* Union-Find using Tarjan's algorithm by J.C. Fillaitre *)
type t = { 
  mutable father: Pa.t; (* mutable to allow path compression *)
  c: Pa.t;              (* ranks *)
}

let create n = 
  { c = Pa.create n 0;
    father = Pa.init n (fun i -> i) }
    
let rec find_aux f i = 
  let fi = Pa.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = Pa.set f i r in
    f, r
      
let find h x = 
  let f,rx = find_aux h.father x in 
  h.father <- f; rx
  
let union h x y = 
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    let rxc = Pa.get h.c rx in
    let ryc = Pa.get h.c ry in
    if rxc > ryc then
      { h with father = Pa.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Pa.set h.father rx ry }
    else
      { c = Pa.set h.c rx (rxc + 1);
	father = Pa.set h.father ry rx }
  end else
    h
*)

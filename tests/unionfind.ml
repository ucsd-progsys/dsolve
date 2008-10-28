let show x = x

(*let init_rank n = 
  let rec init_rec m i =
    if i >= n then m else init_rec (Store.set m i 1) (i+1) in
  init_rec Store.empty 0

let init_parent n = 
  let rec init_rec m i =
    if i >= n then m else init_rec (Store.set m i i) (i+1) in
  init_rec Store.empty 0

let create n = 
  (init_rank n, init_parent n)*)

let rec find_aux (r: (int, int) Store.t) (p: (int, int) Store.t) (i: int) = 
  let pi = Store.get p i in
  if pi = i then 
    (p, i)
  else
    let (p', i') = find_aux r p pi in 
    (*let p''      = Store.set p' i i' in*)
    (p'(*'*), i')

let find (r: (int, int) Store.t) (p: (int, int) Store.t) (i: int) =
  find_aux r p i 
  
let union (h: (int, int) Store.t * (int, int) Store.t) (x: int) (y: int) =
  let (r, p)   = h         in
  let (p', x') = find r p x in
  let (p'',y') = find r p' y in
  let _ = assert (Store.get p' x' = Store.get p'' x') in
  if x' != y' then begin
    let rx' = Store.get r x' in
    let ry' = Store.get r y' in
    if rx' > ry' then
      (*r, Store.set p'' y' x'*) assert false
    else if rx' < ry' then
      (*r, Store.set p'' x' y'*) assert false
    else 
      let r' = Store.set r x' (rx' + 1) in
      let _ = show x' in
      (* let b = Store.get r' x' > Store.get r' y' in
         let c = show b in*) 
      let p''' = Store.set p'' y' x' in
      (r', p''') (r, p)
  end else
    (*r, p''*) assert false

(*************************************************)
(*
let check h = 
  let (r,p) = h in
  Store.iter p (fun i v -> assert (v=i || Store.get r i < Store.get r v))

let rec tester h = 
  if read_int () > 0 then h else 
    let x  = read_int () in
    let y  = read_int () in
    let h' = union h x y in
    tester h


let n  = read_int ()
let h0 = create n 
let h  = tester h0
let _  = check h*)

(*
(* Union-Find using Tarjan's algorithm by J.C. Fillaitre *)
type t = { 
  mutable father: Store.t; (* mutable to allow path compression *)
  c: Store.t;              (* ranks *)
}

let create n = 
  { c = Store.create n 0;
    father = Store.init n (fun i -> i) }
    
let rec find_aux f i = 
  let fi = Store.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = Store.set f i r in
    f, r
      
let find h x = 
  let f,rx = find_aux h.father x in 
  h.father <- f; rx
  
let union h x y = 
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    let rxc = Store.get h.c rx in
    let ryc = Store.get h.c ry in
    if rxc > ryc then
      { h with father = Store.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Store.set h.father rx ry }
    else
      { c = Store.set h.c rx (rxc + 1);
	father = Store.set h.father ry rx }
  end else
    h
*)

let init_rank n = 
  let rec init_rec m i =
    if i >= n then m else init_rec (Store.set m i 1) (i+1) in
  init_rec Store.empty 0

let init_parent n = 
  let rec init_rec m i =
    if i >= n then m else init_rec (Store.set m i i) (i+1) in
  init_rec Store.empty 0

let create n = 
  (init_rank n, init_parent n)

let rec find_aux (r: (int, int) Store.t) (p: (int, int) Store.t) (i: int) = 
  let pi = Store.get p i in
  if pi = i then 
    (p, i)
  else
    let (p',i') = find_aux r p pi in
    let p''     = Store.set p' i i' in
    (p'', i')

let find (r: (int, int) Store.t) (p: (int, int) Store.t) (i: int) =
  find_aux r p i 

let union (r: (int, int) Store.t) (p: (int, int) Store.t) (x: int) (y: int) =
  let (p', x')  = find r p  x in
  let (p'',y')  = find r p' y in
  let _         = assert (Store.get p'' x' = x') in
  let _         = assert (Store.get p'' y' = y') in 
  if x' != y' then begin
    let rx' = Store.get r x' in
    let ry' = Store.get r y' in
    if rx' > ry' then
      (r, Store.set p'' y' x') 
    else if rx' < ry' then
      (r, Store.set p'' x' y') 
    else 
      let r = Store.set r x' (rx' + 1) in (* COMMENT TO SEE BUG *)
      let p''' = Store.set p'' y' x' in
      (r, p''') 
  end else
    (r, p'') 

let check r p = 
  Store.iter p (fun i v -> assert (v=i || Store.get r i < Store.get r v))

let rec tester r p = 
  if read_int () > 0 then (r, p) else 
    let x       = read_int () in
    let y       = read_int () in
    let (r',p') = union r p x y in
    tester r' p'

let _ = 
  let n         = read_int () in
  let r         = init_rank n in
  let p         = init_parent n in
  let _         = show r in
  let _         = show p in
  let _         = check r p in
  let (r', p')  = tester r p in 
  check r' p'

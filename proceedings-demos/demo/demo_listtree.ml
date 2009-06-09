let show x = ()

type 'a t = Leaf of 'a | Node of ('a t) list

let rec fold f b t = 
  match t with
  | Leaf x  -> f b x
  | Node ts -> List.fold_left (fold f) b ts

let rec map f t = 
  match t with
  | Leaf x  -> Leaf (f x)
  | Node ts -> Node (List.map (map f) ts)

let rec iter f t = 
  match t with
  | Leaf x  -> f x
  | Node ts -> List.iter (iter f) ts

let rec build2 f b d =
  if d <= 0 then (Leaf b, f b) else
    let (t1,b1) = build2 f b  (d-1) in
    let (t2,b2) = build2 f b1 (d-1) in
    (Node [t1;t2], b2)

(**********************************************************)

let rec make n v = 
  if n <= 0 then Leaf v else
     let t1 = make (n-1) (v+1) in
     let t2 = make (n-2) (v+2) in
     let t3 = make (n-3) (v+3) in
     Node [t1;t2;t3]

let demo0 n v = 
  let t = make n v in
  iter (fun x -> assert (v <= x)) t

(*********************************************************)

let demo1 d k =
  if k < 0 then () else
    let (t,_) = build2 ((+) 1) (k+1) d in
    let x     = fold (+) k t in
    assert (x >= k)

(*********************************************************)

let mmin x y = 
  if x <= y then x else y

let mmax x y = 
  if x <= y then y else x

(* let lub (p: int*int) (p':int*int) = *)
let lub p  p' = 
  let (x, y)  = p in
  let (x',y') = p' in
  let x''     = mmin x x' in
  let y''     = mmax y y' in
  (x'', y'')

let demo2 d k = 
  if k < 0 then () else 
    let (t,_) = build2 ((+) 1) k d in
    let t1    = map (fun x -> (x,2*x + 1)) t in
    let (x,y) = fold lub (k,k) t1 in
    iter (fun x -> let (a,b) = x in assert (k <= a && k <= b)) t1; 
    assert (x <= y)

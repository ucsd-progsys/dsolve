let myassert b = 
  ()

let myfail s =
  print_string s; 
  assert false 

(***********************************************************************)
(****************** Graph API ******************************************)
(***********************************************************************)

let new_node g n = 
  let _ = myassert (not (Myhash.mem g n)) in
  Myhash.set g n []

let succs g n = 
  let _  = myassert (Myhash.mem g n) in
  Myhash.get g n

let link g n n' l' = 
  let _  = myassert (Myhash.mem g n) in
  let _  = myassert (Myhash.mem g n') in
  let v  = succs g n in
  Myhash.set g n ((l',n')::v)

let has_child g n = 
  succs g n = []


let check_dag b g t =
 Myhash.iter (fun v n -> assert (0 <= n)) t;
 Myhash.iter (fun v n -> assert (n <= b)) t;
 Myhash.iter 
   (fun i js -> 
     assert (i <= b);
     List.iter (fun z -> let (_,j) = z in assert (i < j)) js; 
     List.iter (fun z -> let (_,j) = z in assert (j <= b)) js) g

(***********************************************************************)
(****************** AST ************************************************)
(***********************************************************************)

type var = string 
type indexedvar = var * int * int 

(* type block = int                                (* id *) 
type sblock = int * int
type bvtype = (int * int) list *)

type splitres = 
  | A of ((int * int) list * (int * int) list)  
  | B of (int * int) * int * int * (int * int) list

type constr = 
  | Bottom of var * int * int                           (* x[i:j] = \bot *)
  | IndexedEq of (var * int * int) * (var * int * int)  (* x[i:j] = x'[i':j'] *)
  | IndexedLeq of (var * int * int) * (var * int * int)  (* x[i:j] = x'[i':j'] *)
(*| IndexedLeq of indexedvar * indexedvar               (* x[i:j] <= x'[i':j']*) *)

let size = 64
let bot = 0
let get_new_block b = 
  b + 1

(***********************************************************************)
(****************** Data Structures ************************************)
(***********************************************************************)

type elabel_t = Outer | Inner of (int * int) (* Inner (posn,size) *)

type node = int

let new_bvtyping sz = 
  let t = Myhash.create sz in 
  let g = Myhash.create sz in
  let g = new_node g bot in
  (bot, g, t)

let add_new_block b g =
  let b1 = get_new_block b in
  let g1 = new_node g b1 in
  (b1, g1)

let add_new_var b g t v =
  let (b1,g1) = add_new_block b g in
  let t1      = Myhash.set t v b1 in
  (b1,g1,t1)

let get_var_node b g t v =
  let (b1,g1,t1) = if not (Myhash.mem t v) then add_new_var b g t v else (b,g,t) in
  (b1, g1, t1, Myhash.get t1 v)

let get_edge_size e =
  match e with Outer -> -1 | Inner (_,i) -> i

let get_edge_posn e = 
  match e with Outer -> -1 | Inner (i,_) -> i

let cmpe e e' = 
  compare (get_edge_posn (fst e)) (get_edge_posn (fst e'))

let e_sort es =
  List.rev (List.sort cmpe es)

let is_leaf_block g n = 
  (succs g n) = []
 
let rec fold2 f b1 b2 xs ys =
  match xs, ys with
  | (x::xs', y::ys') -> 
      let (b1', b2') = f b1 b2 x y in
      fold2 f b1' b2' xs' ys'
  | _ -> b1 

let substitute g n ns' ss' =
  let _ = myassert (List.length ns' = List.length ss' + 1) in
  let _ = myassert (List.for_all (is_leaf_block g) (n::ns')) in
  let _ = myassert (n != bot) in
  match ns' with 
  | [] -> myfail ("error: ns' must be nonempty")
  | (n':: ns') -> 
      let g'      = link g n n' Outer in
      fold2 
        (fun g i n' s' -> 
          let g' = link g n n' (Inner (i,s')) in
          (g', i+1))
        g' 0 ns' ss'
  (* let rec hookup g n i ns' ss' =
    match (ns', ss') with 
    | (n'::ns'',s'::ss'') -> 
        let g' = link g n n' (Inner (i,s')) in
        hookup g' n (i+1) ns'' ss''
    | _ -> g in 
  match ns' with 
  | [] -> myfail ("error: ns' must be nonempty")
  | (n':: ns') -> 
      let g' = link g n n' Outer in
      hookup g n 0 ns' ss'
  let hookup n g i n' s' =  
    let g' = link g n n' (Inner (i,s')) in
    (g', i+1) in 
  let (g'',j) = fold2 (hookup n) g' 0 ns' ss' in g'' 
 *)
 

(* INV: Graph is acyclic *)
let rec desc g n s = 
  if s <= 0 then 
    [] 
  else 
    match succs g n with
    | [] -> [(n, s)]
    | es -> proc g s (e_sort es) 
and proc g s es  =
  if s <= 0 then 
    [] 
  else 
    match es with 
    | [] -> [] 
    | (l,n')::es' ->
        let sz = get_edge_size l in
        if sz  = -1 || sz >= s then 
          desc g n' s
        else 
          ((proc g (s-sz) es') @ (desc g n' sz))
  
let get_bvtype b g t x =
  let (b1,g1,t1,nx) = get_var_node b g t x in
  let bs            = desc g nx size in
  let _             = myassert (List.for_all (fun z -> let (n,_) = z in is_leaf_block g n) bs) in
  let _             = myassert (List.fold_left (+) 0 (List.map snd bs) = size) in
  (b1, g1, t1, bs)

(***********************************************************************)
let rec twosplit pre post d = 
  if d = 0 then 
    A (List.rev pre, post)
  else begin
    match post with
     | [] -> 
         myfail "error in twosplit"
     | (name,s)::post' -> 
        if d >= s then 
          twosplit ((name,s)::pre) post' (d-s)
        else 
          B ((name,s),d,s-d,post')
  end

let ac_break b g t x d = 
  let (b0, g0, t0, bvt) = get_bvtype b g t x in
  match twosplit [] bvt d with 
  | A _ -> 
      (b0, g0, t0) 
  | B ((n,_), s1, s2, _) -> 
      let (b1, g1) = add_new_block b0 g0 in
      let (b2, g2) = add_new_block b1 g1 in
      let g3       = substitute g2 n [b1;b2] [s2] in
      (b2, g3, t0)

let break b g t x (i,j) = 
  let (b1, g1, t1) = ac_break b  g  t  x (size-i-1) in
  let (b2, g2, t2) = ac_break b1 g1 t1 x (size-j) in
  (b2, g2, t2)

(* INV: x broken at j *)
let project b g t x (i,j) =
  let (b1, g1, t1, bvt) = get_bvtype b g t x in
  let post = 
    match twosplit [] bvt (size-i-1) with
    | A (_,x)             -> x
    | B ((n,_),_,s,post') -> ((n,s)::post')
  in 
  let res = 
    match (twosplit [] post (i-j+1)) with
    | A (x,_) -> x
    | _       -> myfail "x not broken at j" in
  (b1, g1, t1, res)


(* From constraint: x[i:j] <= x'[i',j'] *)
(* INV: x broken at ij, x' broken at j' *)
let rec subalign b0 g0 t0 z z' =
  let (x, i, j ) = z  in
  let (x',i',j') = z' in
  if i < j then (b0, g0, t0) else 
    let (b1, g1, t1, bv)  = project b0 g0 t0 x  (i,j)   in
    let (b2, g2, t2, bv') = project b1 g1 t1 x' (i',j') in
    let b                 = b2 in
    let g                 = g2 in
    let t                 = t2 in
    match (bv, bv') with
    | ((n,s)::l,(n',s')::l') ->
        if (n = bot && n' = bot) then 
          subalign b g t (x,i-1,j) (x',i'-1,j')
        else if n' = bot then
          myfail "phase1 invariant nonzero-flow broken"
        else if n  = bot then
          subalign b g t (x,i-1,j) (x',i'-1,j')
        else 
          let ((n,s), (n',s')) = 
            if s > s' then ((n',s'), (n,s)) else ((n,s), (n',s')) in
          if s = s' then
            if n = n' then 
              subalign b g t (x,i-s,j) (x',i'-s,j') 
            else 
              let (b1, g1) = add_new_block b g in
              let g2       = substitute g1 n  [b1] [] in 
              let g3       = substitute g2 n' [b1] [] in 
              subalign b1 g3 t (x,i-s,j) (x',i'-s,j')
          else 
            let _ = myassert (s < s') in 
            if n = n' then 
              let (b1, g1) = add_new_block b g in 
              let g2       = substitute g1 n' [b1;b1] [s'-s] in
              subalign b1 g2 t (x,i,j) (x',i',j')
            else
              let (b1, g1) = add_new_block b  g  in
              let (b2, g2) = add_new_block b1 g1 in
              let g3       = substitute g2 n  [b1]    []     in
              let g4       = substitute g3 n' [b1;b2] [s'-s] in
              subalign b2 g4 t (x,i,j) (x',i',j')

let refine b g t c = 
  match c with
  | Bottom (x,i,j) -> 
      (b, g, t) 
  | IndexedEq (z,z') ->
      let (x,i,j)      = z  in
      let (x',i',j')   = z' in
      let _            = myassert (size > i && i >= j && j >= 0) in
      let _            = myassert (size > i' && i' >= j' && j' >= 0) in
      let _            = myassert (i-j = i'-j') in
      let (b1, g1, t1) = break b  g  t  x (i ,j)  in 
      let (b2, g2, t2) = break b1 g1 t1 x'(i',j') in
      let (b3, g3, t3) = subalign b2 g2 t2 (x,i,j) (x',i',j') in
      let (b4, g4, t4) = subalign b3 g3 t3 (x',i',j') (x,i,j) in
      (b4, g4, t4) 
 | IndexedLeq (z,z') ->
      let (x,i,j)      = z  in
      let (x',i',j')   = z' in
      let _            = myassert (size > i && i >= j && j >= 0) in
      let _            = myassert (size > i' && i' >= j' && j' >= 0) in
      let _            = myassert (i-j = i'-j') in
      let (b1, g1, t1) = break b  g  t  x  (i,j) in
      let (b2, g2, t2) = break b1 g1 t1 x' (i',j') in
      let (b3, g3, t3) = subalign b2 g2 t2 (x,i,j) (x',i',j') in
      (b3, g3, t3)

let rec solver b g t cs = 
  match cs with
  | [] -> 
      (b,g,t)
  | c::cs' ->
      let (b1,g1,t1) = refine b g t c in
      solver b1 g1 t1 cs'

let solve cs = 
  let (b, g, t) = new_bvtyping 17 in
  let _         = check_dag b g in
  let (b1,g1,t1)= solver b g t cs  in
  let _         = check_dag b1 g1 in
  (b1, g1, t1)

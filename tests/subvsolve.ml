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


let check_dag b g = 
 Myhash.iter 
   (fun i js -> 
     assert (i <= b);
     List.iter (fun z -> let (_,j) = z in assert (i < j)) js; 
     List.iter (fun z -> let (_,j) = z in assert (j <= b)) js) g

(***********************************************************************)
(****************** AST ************************************************)
(***********************************************************************)

type var = int 
type interval = int * int
type indexedvar = var * interval
type block = int                                (* id *) 
type sblock = block * int
type bvtype = sblock list
type splitres = 
  | A of (sblock list * sblock list)  
  | B of sblock * int * int * sblock list

type constr = 
  | Bottom of indexedvar	                (* x[i:j] = \bot *)
  | IndexedEq of indexedvar * indexedvar        (* x[i:j] = x'[i':j'] *)
  | IndexedLeq of indexedvar * indexedvar       (* x[i:j] <= x'[i':j'] *)

let size = 64
let bot = 0
let get_new_block b = 
  b + 1

(***********************************************************************)
(****************** Data Structures ************************************)
(***********************************************************************)

type elabel_t = Outer | Inner of (int * int) (* Inner (posn,size) *)

type node = int

type bvtyping = {
  var_node_map_t                : (var,node) Hashtbl.t;
}

let new_bvtyping () = 
  let t = {var_node_map_t = Hashtbl.create 31} in
  let g = Myhash.create 17 in
  let g = new_node g bot in
  (bot, g, t)

let add_new_block b g t  =
  let b' = get_new_block b in
  let g' = new_node g b' in
  (b', g')

let add_new_var b g t v =
  let (b',g') = add_new_block b g t in
  Hashtbl.replace (t.var_node_map_t) v b';
  (b',g')

let get_var_node b g t v =
  let (b',g') = if not (Hashtbl.mem t.var_node_map_t v) then add_new_var b g t v else (b, g) in
  (b', g', Hashtbl.find (t.var_node_map_t) v)

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
  
let substitute g n ns' ss' =
  assert (List.length ns' = List.length ss' + 1);
  assert (List.for_all (is_leaf_block g) (n::ns'));
  assert (n != bot);
  let hookup n (g, i) n' s' =  
    let g' = link g n n' (Inner (i,s')) in
    (g', i+1) in
  match ns' with 
  | [] -> myfail ("error: ns' must be nonempty")
  | (n':: ns') -> 
      let g'      = link g n n' Outer in
      let (g'',_) = List.fold_left2 (hookup n) (g',0) ns' ss' in
      g''

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
  let (b1,g1,nx) = get_var_node b g t x in
  let bs = desc g nx size in
  let _  = myassert (List.for_all (fun z -> let (n,_) = z in is_leaf_block g n) bs) in
  let _  = myassert (List.fold_left (+) 0 (List.map snd bs) = size) in
  (b1, g1, (bs : sblock list))

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

let _break b g t x d = 
  let (b0, g0, bvt) = get_bvtype b g t x in
  match twosplit [] bvt d with 
  | A _ -> 
      (b0, g0) 
  | B ((n,_), s1, s2, _) -> 
    let (b1, g1) = add_new_block b0 g0 t  in
    let (b2, g2) = add_new_block b1 g1 t  in
    let g3       = substitute g2 n [b1;b2] [s2] in
    (b2, g3)

let break b g t x (i,j) = 
  let (b1, g1)   = _break b  g  t x (size-i-1) in
  let (b2, g2)   = _break b1 g1 t x (size-j) in
  (b2, g2)

(* INV: x broken at j *)
let project b g t x (i,j) =
  let (b1, g1, bvt) = get_bvtype b g t x in
  let post = 
    match twosplit [] bvt (size-i-1) with
    | A (_,x)             -> x
    | B ((n,_),_,s,post') -> ((n,s)::post')
  in 
  let res = 
    match (twosplit [] post (i-j+1)) with
    | A (x,_) -> x
    | _       -> myfail "x not broken at j" in
  (b1, g1, res)


let rec clone e i = if i <= 0 then [] else (e::(clone e (i-1)))

(* From constraint: x[i:j] <= x'[i',j'] *)
(* INV: x broken at ij, x' broken at j' *)
let rec subalign b0 g0 t (x,(i,j)) (x',(i',j')) =
  if i < j then (b0, g0) else 
    let (b1, g1, bv)  = project b0 g0 t x  (i,j)   in
    let (b2, g2, bv') = project b1 g1 t x' (i',j') in
    let b             = b2 in
    let g             = g2 in
    match (bv, bv') with
    | ((n,s)::l,(n',s')::l') ->
        if (n = bot && n' = bot) then 
          subalign b g t (x,(i-1,j)) (x',(i'-1,j'))
        else if n' = bot then
          myfail "phase1 invariant nonzero-flow broken"
        else if n  = bot then
          subalign b g t (x,(i-1,j)) (x',(i'-1,j'))
        else 
          let ((n,s), (n',s')) = 
            if s > s' then ((n',s'), (n,s)) else ((n,s), (n',s')) in
          if s = s' then
            if n = n' then 
              subalign b g t (x,(i-s,j)) (x',(i'-s,j')) 
            else 
              let (b1, g1) = add_new_block b g t in
              let g2       = substitute g1 n  [b1] [] in 
              let g3       = substitute g2 n' [b1] [] in 
              subalign b1 g3 t (x,(i-s,j)) (x',(i'-s,j'))
          else 
            let _ = myassert (s < s') in 
            if n = n' then 
              let (b1, g1) = add_new_block b g t in 
              let g2       = substitute g1 n' [b1;b1] [s'-s] in
              subalign b1 g2 t (x,(i,j)) (x',(i',j'))
            else
              let (b1, g1) = add_new_block b  g  t in
              let (b2, g2) = add_new_block b1 g1 t in
              let g3       = substitute g2 n  [b1]    []     in
              let g4       = substitute g3 n' [b1;b2] [s'-s] in
              subalign b2 g4 t (x,(i,j)) (x',(i',j'))
  | _ -> 
      myfail "assert-fail: error in subalign" 

let refine b g t c = 
  match c with
  | Bottom (x,ij) -> (b, g) 
  | IndexedEq ((x,(i,j)),(x',(i',j'))) ->
      let _        = myassert (size > i && i >= j && j >= 0) in
      let _        = myassert (size > i' && i' >= j' && j' >= 0) in
      let _        = myassert (i-j = i'-j') in
      let (b1, g1) = break b  g  t x (i ,j)  in 
      let (b2, g2) = break b1 g1 t x'(i',j') in
      let (b3, g3) = subalign b2 g2 t (x,(i,j)) (x',(i',j')) in
      let (b4, g4) = subalign b3 g3 t (x',(i',j')) (x,(i,j)) in
      (b4, g4)
 | IndexedLeq ((x,(i,j)),(x',(i',j'))) ->
      let _        = assert (size > i && i >= j && j >= 0) in
      let _        = assert (size > i' && i' >= j' && j' >= 0) in
      let _        = assert (i-j = i'-j') in
      let (b1, g1) = break b  g  t x  (i,j) in
      let (b2, g2) = break b1 g1 t x' (i',j') in
      let (b3, g3) = subalign b2 g2 t (x,(i,j)) (x',(i',j')) in
      (b3, g3)
  | _ -> 
      myfail "Not handled"

let rec solver b g t cs = 
  match cs with
  | [] -> 
      (b,g)
  | c::cs' ->
      let (b1,g1) = refine b g t c in
      solver b1 g1 t cs'

let solve cs = 
  let (b, g, t) = new_bvtyping () in
  let _         = check_dag b g in
  let (b', g')  = solver b g t cs  in
  let _         = check_dag b' g' in
  (b', g')


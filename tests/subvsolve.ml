


type ('a,'b) graph = ('a * ('b * 'a) list) list 

let create () = []

let create_root g n = (n,[])::g 

let rec succs g n =
  match g with 
  | []          -> assert false
  | (k,v)::g'   -> if n = k then v else succs g' n 

let link g n n' l' =
  let v = succs g n in
  (n, (l',n')::v)::g

let has_child g n = 
  succs g n = []

(***********************************************************************)
(****************** AST ************************************************)
(***********************************************************************)

type var = int 
type interval = int * int
type indexedvar = var * interval
type constr = 
  | Bottom of indexedvar	                (* x[i:j] = \bot *)
  | Eq of var * var	                        (* x = y : for pointers *)
  | IndexedEq of indexedvar * indexedvar        (* x[i:j] = x'[i':j'] *)
  | IndexedLeq of indexedvar * indexedvar       (* x[i:j] <= x'[i':j'] *)

type block = int                                (* id *) 
type bvtype = block list

let size = 64
let bot = 0
  
let get_new_block = 
  let xr = ref 0 in
  fun s -> 
    let _ = incr xr in 
    ("a"^(string_of_int !xr), s)


(***********************************************************************)
(****************** Data Structures ************************************)
(***********************************************************************)

type elabel_t = Outer | Inner of (int * int) (* Inner (posn,size) *)

type node = int (* (block,elabel_t) G.node *)

(* INV: Forall nodes n, snd (n.label) = size *)

type bvtyping = {
  block_cons_map_t              : (block,constr option) Hashtbl.t;
  block_node_map_t              : (block,node) Hashtbl.t;
  block_prettyblock_map_t       : (block,block) Hashtbl.t;
  var_node_map_t                : (var,node) Hashtbl.t;
}


let new_bvtyping () = 
  let t = 
    { block_cons_map_t = Hashtbl.create 31;
      block_node_map_t = Hashtbl.create 31;
      block_prettyblock_map_t = Hashtbl.create 31;
      var_node_map_t = Hashtbl.create 31}
  in
  let g  = G.create () in
  let g' = G.create_root g bot in
  Hashtbl.replace (t.block_node_map_t) bot bot;
  (g, t)

let cur_constraint_ref = ref None
  
let add_new_block t  =
  let b = get_new_block size in
  let n = G.create_root b in
  Hashtbl.replace (t.block_node_map_t) b n;
  Hashtbl.replace (t.block_cons_map_t) b (!cur_constraint_ref);
  b
 
let get_block_node t b = 
  try Hashtbl.find (t.block_node_map_t) b 
  with Not_found -> failwith ("block not in table")

let get_block_constraint t b = 
  try Hashtbl.find (t.block_cons_map_t) b
  with Not_found -> None

let get_prettyblock t (n,s) = 
  try (fst (Hashtbl.find (t.block_prettyblock_map_t) (n,size)),s) 
  with Not_found -> (n,s)

let add_new_var t v =
  let b = add_new_block t in
  let n = get_block_node t b in
  Hashtbl.replace (t.var_node_map_t) v n;
  ()

let get_var_node t v =
  if not (Hashtbl.mem t.var_node_map_t v) then add_new_var t v;
  Hashtbl.find (t.var_node_map_t) v 
 
let get_edge_size e =
  let (e,_) = e in
  match e with Outer -> -1 | Inner (_,i) -> i

let get_edge_posn e = 
  match e with Outer -> -1 | Inner (i,_) -> i
  

let e_sort e_l =
  List.rev (List.sort (fun e e' -> compare (get_edge_posn e) (get_edge_posn e')) e_l)

let is_leaf_block t b = 
  let n = get_block_node t b in
  (G.succs n) = []
  
let substitute t b b'_l s'_l =
  assert (List.length b'_l = List.length s'_l + 1);
  assert (List.for_all (is_leaf_block t) (b::b'_l));
  assert (b != bot);
  let link n i b' s' =  
    let n_b' = get_block_node t b' in
    G.link n n_b' (Inner (i,s'));
    i+1
  in
  let n_b = get_block_node t b in
  match b'_l with [] -> failwith ("error: b'_l must be nonempty")
  | (b'_out :: b'_in_l) -> 
      let n_b'_out = get_block_node t b'_out in
      G.link n_b n_b'_out Outer;
      ignore(List.fold_left2 (link n_b) 0 b'_in_l s'_l);
      ()

(* INV: Graph is acyclic *)
let rec desc n s = 
  let rec proc s e_l  =
    if (s <= 0) then [] else match e_l with [] -> [] | e::e_l' ->
      let (n',_) = e in
      let size   = get_edge_size e in
      if size = -1 || size >= s then desc n' s
      else ((proc (s-size) e_l') @ (desc n' size))
  in
  if (s <= 0) then [] else
  match e_sort (G.succs n) with
    [] -> [(fst n, s) ]
  | e_l -> proc s e_l

  
let get_bvtype t x : bvtype = 
  let b_l = desc (get_var_node t x) size in
  assert (List.for_all (is_leaf_block t) b_l);
  assert (List.fold_left (+) 0 (List.map snd b_l) = size);
  b_l


(***********************************************************************)

exception BreakExn of (block * int * int * (block list))

let rec twosplit pre post d = 
  if d = 0 then (List.rev pre,post)
  else 
    (match post with
      [] -> failwith "error in twosplit"
    | (name,s)::post' -> 
        if d >= s then twosplit ((name,s)::pre) post' (d-s)
        else raise (BreakExn ((name,s),d,s-d,post')))   
    
let break t x (i,j) = 
  let _acb t x d = 
    let bvt = get_bvtype t x in
    try ignore(twosplit [] bvt d) 
    with BreakExn(b,s1,s2,_) -> 
      let b1 = add_new_block t  in
      let b2 = add_new_block t  in
      substitute t b [b1;b2] [s2]
  in
  _acb t x (size-i-1);
  _acb t x (size-j)

(* INV: x broken at j *)
let project t x (i,j) =
  let bvt = get_bvtype t x in
  let post = 
    try snd(twosplit [] bvt (size-i-1)) 
    with BreakExn((n,_),_,s,post') -> ((n,s)::post')
  in 
  fst (twosplit [] post (i-j+1))

let rec clone e i = if i <= 0 then [] else (e::(clone e (i-1)))

(* From constraint: x[i:j] <= x'[i',j'] *)
(* INV: x broken at ij, x' broken at j' *)
let rec subalign t (x,(i,j)) (x',(i',j')) =
  if (i>=j) then 
  let bv = project t x (i,j) in
  let bv' = project t x' (i',j') in
   match (bv,bv') with
    (h::l,h'::l') when (h=bot && h'=bot) -> subalign t (x,(i-1,j)) (x',(i'-1,j'))
  | (_,h'::_) when (h' = bot) -> failwith "assert-fail: phase1 invariant nonzero-flow broken"
  | (h::l,_) when (h=bot) -> subalign t (x,(i-1,j)) (x',(i'-1,j'))
  | (b::l, b'::l') ->
      (* neither is bot, flip so that b' is the larger block *)
      let (b,b') = if (snd b) > (snd b') then (b',b) else (b,b') in 
      let ((n,s),(n',s')) = (b,b') in
      if (s = s') then
        (if (n <> n') then substitute t b [b'] [];
        subalign t (x,(i-s,j)) (x',(i'-s,j')))
      else 
        (assert (s < s'); 
         let b0 = add_new_block t in 
         let b1 = if (n <> n') then b else b0 in 
         substitute t b' [b1;b0] [s'-s];
         subalign t (x,(i,j)) (x',(i',j')))
  | _ -> failwith "assert-fail: error in subalign" 
    
(* From constraint: x[i:j] = x'[i':j']  *)    
(* INV: x broken at ij, x' broken at ij' *)
let rec align t (x,(i,j)) (x',(i',j')) =
  if (i<j) then () 
  else
    let bv = project t x (i,j) in
    let bv' = project t x' (i',j') in
    match (bv,bv') with
    (b::_, b'::_) -> 
       let ((n,s),(n',s')) = (b,b') in
       let s0 = min s s' in
       if n  <> n' then
         (if s = s' then 
           if (b <> bot) then substitute t b [b'] [] else substitute t b' [b] []
           else 
            let b0 = add_new_block t in
            let s0 = abs (s-s') in
            if s < s' then substitute t b' [b;b0] [s0]
            else substitute t b [b';b0] [s0]);
       align t (x,(i-s0,j)) (x',(i'-s0,j'))
    | _ -> failwith "error in align"

let refine vmap t c = 
  cur_constraint_ref := Some c;
  match c with
  | Bottom (x,ij) -> ()
  | IndexedEq ((x,(i,j)),(x',(i',j'))) ->
      begin
        assert (size > i && i >= j && j >= 0);
        assert (size > i' && i' >= j' && j' >= 0);
        assert (i-j = i'-j');
        break t x (i,j); 
        break t x'(i',j');
        (* align t (x,(i,j)) (x',(i',j')); *)
        subalign t (x,(i,j)) (x',(i',j'));
        subalign t (x',(i',j')) (x,(i,j));
      end
 | IndexedLeq ((x,(i,j)),(x',(i',j'))) ->
      begin
        assert (size > i && i >= j && j >= 0);
        assert (size > i' && i' >= j' && j' >= 0);
        assert (i-j = i'-j');
        break t x (i,j); 
        break t x'(i',j');
        subalign t (x,(i,j)) (x',(i',j'));
      end
  | _ -> failwith "Not handled"

let solve vmap cs = 
  let t = new_bvtyping () in
  let _ = List.iter (refine vmap t) cs in
  t

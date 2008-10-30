
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
type block = int                                (* id *) 
type bvtype = block list

type constr = 
  | Bottom of indexedvar	                (* x[i:j] = \bot *)
  | IndexedEq of indexedvar * indexedvar        (* x[i:j] = x'[i':j'] *)
  | IndexedLeq of indexedvar * indexedvar       (* x[i:j] <= x'[i':j'] *)

let size = 64
let bot = 0

(* 
let get_new_block = 
  let xr = ref 0 in
  fun s -> 
    let _ = incr xr in 
    !xr (* ("a"^(string_of_int !xr), s) *)
*)

let get_new_block b = 
  b + 1

(***********************************************************************)
(****************** Data Structures ************************************)
(***********************************************************************)

type elabel_t = Outer | Inner of (int * int) (* Inner (posn,size) *)

type node = int

type bvtyping = {
  block_cons_map_t              : (block,constr option) Hashtbl.t;
(*  block_prettyblock_map_t       : (block,block) Hashtbl.t; *)
  var_node_map_t                : (var,node) Hashtbl.t;
}

let new_bvtyping () = 
  let t = 
    { (* block_cons_map_t = Hashtbl.create 31;
         block_prettyblock_map_t = Hashtbl.create 31; *)
         var_node_map_t = Hashtbl.create 31}
  in
  let g = G.create () in
  let g = G.create_root g bot in
  (* Hashtbl.replace (t.block_node_map_t) bot bot; *)
  (g, t)

(* let cur_constraint_ref = ref None *)

let add_new_block b g t  =
  let b' = get_new_block b in
  let g' = G.create_root g b' in
  (* Hashtbl.replace (t.block_node_map_t) b n; 
     Hashtbl.replace (t.block_cons_map_t) b' (!cur_constraint_ref); *)
  (b', g')

(* 
let get_block_node t b = 
  try Hashtbl.find (t.block_node_map_t) b 
  with Not_found -> failwith ("block not in table")

let get_block_constraint t b = 
  if Hashtbl.mem (t.block_cons_map_t) then 
    Hashtbl.find (t.block_cons_map_t) b
  else None

let get_prettyblock t (n,s) = 
  try (fst (Hashtbl.find (t.block_prettyblock_map_t) (n,size)),s) 
  with Not_found -> (n,s)
*)

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

let e_sort es =
  List.rev (List.sort (fun e e' -> compare (get_edge_posn e) (get_edge_posn e')) es)

let is_leaf_block g n = 
  (G.succs g n) = []
  
let substitute g n ns' ss' =
  assert (List.length ns' = List.length ss' + 1);
  assert (List.for_all (is_leaf_block g) (n::ns'));
  assert (n != bot);
  let link n (g, i) n' s' =  
    let g' = G.link g n n' (Inner (i,s')) in
    (g', i+1) in
  match ns' with 
  | [] -> myfail ("error: ns' must be nonempty")
  | (n':: ns') -> 
      let g'      = G.link g n n' Outer in
      let (g'',_) = List.fold_left2 (link n) (g',0) ns' ss' in
      g''

(* INV: Graph is acyclic *)
let rec desc g n s = 
  if s <= 0 then 
    [] 
  else 
    match G.succs g n with
    | [] -> [(n, s)]
    | es -> proc s (e_sort es) 
and proc s es  =
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
          ((proc (s-sz) es') @ (desc g n' sz))
  
let get_bvtype g t x = 
  let bs = desc g (get_var_node t x) size in
  assert (List.for_all (is_leaf_block g) bs);
  assert (List.fold_left (+) 0 (List.map snd bs) = size);
  bs 

(***********************************************************************)

exception BreakExn of (block * int * int * (block list))

let rec twosplit pre post d = 
  if d = 0 then 
    (List.rev pre, post)
  else 
    (match post with
     | [] -> myfail "error in twosplit"
     | (name,s)::post' -> 
        if d >= s then twosplit ((name,s)::pre) post' (d-s)
        else raise (BreakExn ((name,s),d,s-d,post')))   
  
let _break b g t x d = 
  let bvt = get_bvtype g t x in
  try ignore(twosplit [] bvt d) 
  with BreakExn(n,s1,s2,_) -> 
    let (b1, g1) = add_new_block b  g  t  in
    let (b2, g2) = add_new_block b1 g1 t  in
    let g3       = substitute g2 n [b1;b2] [s2] in
    (b2, g3)

let break b g t x (i,j) = 
  let (b1, g1)   = breaker b g t x (size-i-1) in
  let (b2, g2)   = breaker b1 g1 t x (size-j) in
  (b2, g2)


HEREHEREHEREHERE

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
  let bv  = project t x (i,j) in
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
        let _ = 
          if n = n' then () else 
            let b0 = add_new_block t in
            let _  = substitute t b  [b0] [] in 
            let _  = substitute t b' [b0] [] in () in
        subalign t (x,(i-s,j)) (x',(i'-s,j')) 
        (* (if (n <> n') then substitute t b [b'] [];
        subalign t (x,(i-s,j)) (x',(i'-s,j'))) *)
      else 
        (assert (s < s'); 
         let _ =  
           if n = n' then 
             let b0 = add_new_block t in 
             substitute t b' [b0;b0] [s'-s]
           else
             let b0 = add_new_block t in
             let b1 = add_new_block t in
             substitute t b  [b1]    [];
             substitute t b' [b1;b0] [s'-s] in
         (* let b1 = if (n <> n') then b else b0 in 
            substitute t b' [b1;b0] [s'-s]; *)
         subalign t (x,(i,j)) (x',(i',j')))
  | _ -> failwith "assert-fail: error in subalign" 

let refine vmap t c = 
  (* cur_constraint_ref := Some c; *)
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

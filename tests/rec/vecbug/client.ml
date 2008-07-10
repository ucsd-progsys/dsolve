open Vec

(* ***** somewhat misshapen gadgets ***** *)

(* joins two balanced trees if the right tree is willing to play nice *)
let comb t1 t2 = concat t1 (insert 0 0 t2)

let rec pow n = if n = 0 then 1 else 2 * pow (n-1)

(* create a balanced tree of height n *)
let createh n = create 0 (pow (n-1))

(* **** demo of apparent bug ***** *)

(* somewhat obtuse demonstration of "bad" tree. recursively balancing t1 with t7 yields
 * a tree of height 6, which is then joined with a tree of height 10 *)
(* note that t7 could also be created from the empty tree by repeated appends *)
let mk_t11 () = 
    let t3 = createh 3 in
    let t5 = createh 5 in
    let t6 = comb t5 t3 in
    let t4 = createh 4 in
    let t7 = comb t6 t4 in
    let t9 = createh 9 in
    let t10 = comb t7 t9 in
    let t8 = createh 8 in
      comb t10 t8
 

let mk_tree () =
   let t11 = mk_t11 () in
   let t1 = singleton 0 in
      comb t1 (insert 0 1 t11)
 
(* here's where we'll depart from the existing vec functions. concat2 in the modified
 * vec.ml uses a fixed recbal2. if we build a duplicate tree but join t11 and t1 at the
 * end with the fixed recbal, we should get a balanced tree *)
let mk_fixed_tree () =
   let t11 = mk_t11 () in
   let t1 = singleton 0 in
      concat2 t1 (insert 0 0 (insert 0 1 t11))

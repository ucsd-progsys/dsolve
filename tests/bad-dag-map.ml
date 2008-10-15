(* let i = () *)

(**************************************************************)
(************************ List Graph API **********************)
(**************************************************************)

let hassert x = ()

let show x = x

let flap f xs   = List.flatten (List.map f xs)

(**************************************************************)
(************************ List Graph API **********************)
(**************************************************************)

(* API *)
let new_node n g = 
  let _ = hassert (not (Myhash.mem g n)) in 
  Myhash.set g n []

(* API *)
let new_edge g n n' = 
  let _  = show g in
  let _  = hassert (Myhash.mem g n) in
  let _  = hassert (Myhash.mem g n') in
  let ns = Myhash.get g n in
  Myhash.set g n (n'::ns)

let _ = new_edge

(* API *)
let succs g n = 
  let _  = hassert (Myhash.mem g n) in
  Myhash.get g n

(* API *)
let choose_node g = 
  let n = 
    Myhash.fold 
      (fun k v a -> if read_int () > 1 then Some k else a) 
      g None in
  match n with None -> assert false | Some n -> n



(* API *)
let check_dag n g = 
 Myhash.iter 
   (fun i js -> 
     assert (i <= n);
     List.iter (fun j -> assert (i < j)) js; 
     List.iter (fun j -> assert (j <= n)) js) g

(**************************************************************)
(************************ DAG Building ************************)
(**************************************************************)

let fresh n = n + 1

let rec leaves g n =
  match succs g n with 
  | [] -> [n] 
  | ns -> flap (leaves g) ns

let rec build_dag n g =
  let _ = check_dag n g in
  let t = read_int () in
  if t = 0 then 
    (n, g)
  else if t = 1 then
    let n' = fresh n in
    let g' = new_node n' g in
    build_dag n' g'
  else 
    let n1s = leaves g (choose_node g) in
    let n2s = leaves g (choose_node g) in
    let ns  = n1s  @ n2s  in
    let n'  = fresh n in
    let g'  = new_node n' g in 
    let g'' = List.fold_left (fun g m -> new_edge g (show m) (show n')) (show g') (show ns) in
    build_dag n' g''

let n0     = 0            
let g0     = Myhash.create 17 (* n0 [] *)
let (n,g)  = build_dag n0 g0
let _      = show g
let _      = check_dag n g

(**************************************************************)

(* 
let n1 = fresh n0             
let n2 = fresh n1
let n3 = fresh n2
let _  = show n0
let _  = show n1
let _  = show n2
let _  = assert (n1 <= n2)

let g0 = new_node n0 (Myhash.create 37)

let _  = Myhash.itert g0 (fun i js -> assert (i <= n0))

let _  = check_dag n1 (show g0)
let g1 = new_node n1 g0  
let _  = check_dag n1 (show g1)

(*let g1'= new_node n1 g1       
let _  = show g1'              
let _  = check_dag n1 g1'
*)

let _  = show n2

let g2 = new_node n2 g1       
let _  = show g2
let _  = check_dag n2 g2      
let m  = choose_node g2       
let _  = assert (m <= n2)     
let _  = show m  
let g3 = new_node n3 g2  
let g3'= new_edge g3 m n3
let _  = check_dag n3 g3'      
let g3'' = List.fold_left (fun g m -> new_edge g (show m) (show n3)) (show g3) (show [m]) 
let _  = check_dag n3 g3''

 *)

  (*
let g = []
let g = new_node 0 g
let g = new_node 0 g 
let _ = show g
let z = choose_node g
let _ = assert (0 <= z)
let _ = 
  let m = read_int () in
  let gm= [] in
  (*let gm= new_node m gm in *)
  let gm= new_node m gm in
  let _ = show gm in
  let zm= choose_node gm in
  let _ = assert (m <= zm) in
  ()
  *)

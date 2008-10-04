let foo a b = 
  (a,[])::b

let _ = foo

(*
let _ = 
  let n0 = 0 in
  let l0 = [] in
  let l1 = foo n0 l0 in
  let n1 = n0 + 1 in
  let l2 = foo n2 l2
*)

let rec build n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo n xs in
    build n' xs'

let _ = build 0 []


let show x = x

let flap f xs   = List.flatten (List.map f xs)

(**************************************************************)
(************************ List Graph API **********************)
(**************************************************************)

type id  = int
type g_t = id * id list list

let empty = []

let rec find n = function 
  | [] -> None 
  | (n',v')::ns' -> if n = n' then Some v' else find n ns'

let check_node g n = 
  match find n g with 
  | None -> assert false 
  | _ -> ()

(* API *)
let new_node n g = 
  (n,[])::g

(* API *)
let new_edge g n n' = 
  let _ = check_node g n  in
  let _ = check_node g n' in
  match find n g with 
  | None        -> assert false 
  | Some ns     -> (n, n'::ns)::g

let _ = new_edge

(* API *)
let succs g n = 
  let _ = check_node g n in
  match find n g with 
  | None        -> assert false
  | Some ns     -> ns 

(* API *)
let choose_node g = 
  let n = List.fold_left (fun a x -> let (k, _) = x in if read_int () > 1 then Some k else a) None g  in
  match n with None -> assert false | Some n -> n

(* API *)
let i = ()

let check_dag n g = 
 List.iter 
    (fun x -> 
      let (i,js) = x in
      let _      = assert (i <= n) in
      List.iter (fun j -> assert (i < j && j <= n)) js) 
    g

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
    build_dag n' (new_node n' g) 
  else
    let n1s = leaves g (choose_node g) in
    let n2s = leaves g (choose_node g) in
    let ns  = n1s @ n2s in
    let n'  = fresh n in
    let g'  = new_node n' g in 
    let g'' = List.fold_left (fun g m -> new_edge g m n') g' ns in
    build_dag n' g''

let _ = build_dag 0 []

let _ =
  let n0 = 0 in
  let g0 = [] in
  let n1 = fresh n0 in
  let g1 = new_node n1 g0 in
  let _  = show g1 in
  let _  = check_dag n1 g1 in
  let n2 = fresh n1 in
  let g2 = new_node n2 g1 in
  check_dag n2 g2 

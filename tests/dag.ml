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
let new_node g n = 
  (n,[])::g

(* API *)
let new_edge g n n' = 
  let _ = check_node g n  in
  let _ = check_node g n' in
  match find n g with 
  | None        -> assert false 
  | Some ns     -> (n, n'::ns)::g

(* API *)
let succs g n = 
  let _ = check_node g n in
  match find n g with 
  | None        -> assert false
  | Some ns     -> ns 

(* API *)
let choose_node g = 
  let n = List.fold_left (fun a (k,_) -> if read_int () > 1 then Some k else a) None g  in
  match n with None -> assert false | Some n -> n

(* API *)
let check_dag (g, n) = 
  List.iter 
    (fun (i,js) -> 
      let _ = assert (i <= n) in
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

let rec build_dag (g, n) =
  let _ = check_dag (g, n) in
  match read_int () with 
  | 0 -> 
      (g, n)
  | 1 ->
      let n' = fresh n in
      build_dag (new_node g n', n') 
  | _ -> 
      let n1s = leaves g (choose_node g) in
      let n2s = leaves g (choose_node g) in
      let ns  = n1s @ n2s in
      let n'  = fresh n in
      let g'  = new_node g n' in 
      let g'' = List.fold_left (fun g m -> new_edge g m n') g' ns in
      build_dag (g'', n')

let _ = build_dag ([],0)



module Misc = QpMisc
module Eq = QpEquality
open QpDag

let nb_instance = ref 0

type instance = Eq.instance

let is_atom = function
  | Equality (_,_),_ | Not (Equality (_,_),_),_ 
  | Leq (_,_),_ | Not (Leq (_,_),_),_ 
  | True,_ | False,_ -> true
  | _ -> false

let rec atoms p = 
  match p with 
  | _ when is_atom p -> [p]
  | And ps,_ -> Misc.flap atoms ps  
  | _ -> [] 

let push me p =
  assert (is_atom p);
  Eq.push me p

let is_valid me q = 
  if is_atom q then Eq.is_valid me q else false

let check_imp p =
  let me = incr nb_instance; Eq.new_instance (fun _ _ _ -> ()) in   
  let _ = List.iter (push me) (atoms p) in
  fun q -> is_valid me q

let check_imps p = List.map (check_imp p)

let print_stats () = 
  Printf.printf "QProver Instances = %d \n" !nb_instance

(* {{{
let check_imps_dag p qs =
  let me = Eq.new_instance (fun _ _ _ -> ()) in   
  List.iter (push me) (atoms p);
  List.map (is_valid me) qs

let check_imps p qs = 
  check_imps_dag (pred_dag_of_tree p) (List.map pred_dag_of_tree qs) 
}}} *)



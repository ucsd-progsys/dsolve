module ComparablePath = struct
  type t = Path.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

let flap f xs = 
  List.flatten (List.map f xs)

let flap2 f xs ys = 
  List.flatten (List.map2 f xs ys)

let rec expand f xs ys =
  match xs with
  | [] -> ys
  | x::xs ->
      let (xs',ys') = f x in
      expand f (xs' @ xs) (ys'@ys)

let rec expand2 f ys z = function 
  | [] -> (ys,z)
  | x::xs ->
      let (xs',ys',z') = f x z in
      expand2 f (ys'@ys) z' (xs' @ xs) 

let do_catch s f x =
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); raise ex) 

let do_catch_ret s f x y = 
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); y) 

let rec map_partial f = function 
  | [] -> [] | x::xs -> 
      (match f x with 
       | None -> map_partial f xs 
       | Some y -> y::(map_partial f xs))

let mapfold f xs b = 
  let rec _mf ys b = function
    | [] -> 
        (List.rev ys,b)
    | x::xs -> 
        let (y',b') = f x b in
        _mf (y'::ys) b' xs in
  _mf [] b xs

let incpp ir = 
  incr ir;!ir

let array_to_index_list a = 
  List.rev (snd 
    (Array.fold_left (fun (i,rv) v -> (i+1,(i,v)::rv)) (0,[]) a))

(****************************************************************)
(************* SCC Ranking **************************************)
(****************************************************************)

module Int = struct
  type t = int let compare = compare 
  let hash = Hashtbl.hash let equal = (=)
end
    
module G = Graph.Imperative.Digraph.Concrete(Int)
module SCC = Graph.Components.Make(G)    
 
(* Given list [(u,v)] returns a numbering [(ui,ri)] s.t. 
 * 1. if ui,uj in same SCC then ri = rj
 * 2. if ui -> uj then ui >= uj *)

let scc_rank uvs = 
  Printf.printf "scc_rank: ";
  List.iter (fun (u,v) -> Printf.printf "(%d,%d)" u v) uvs;
  let g = G.create () in
  let _ = List.iter (fun (u,v) -> G.add_edge g u v) uvs in
  let sccs = array_to_index_list (SCC.scc_array g) in
  flap (fun (i,vs) -> List.map (fun v -> (v,i)) vs) sccs

(*
let g1 = [(1,2);(2,3);(3,1);(2,4);(3,4);(4,5)];;
let g2 = [(0,1);(1,2);(2,0);(1,3);(4,3);
          (5,6);(5,7);(6,9);(7,9);(7,8);(8,5)];;
let g3 = (6,2)::g2;;
let g4 = (2,6)::g2;;
  
let n1 = make_scc_num g1 ;;
let n2 = make_scc_num g2 ;;
let n3 = make_scc_num g3 ;;
let n4 = make_scc_num g4 ;; *)


let asserts s b = 
  try assert b with ex -> 
    Printf.printf "Common.asserts failure: %s " s; raise ex

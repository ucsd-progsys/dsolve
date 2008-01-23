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

(* orig 
module Int = struct
  type t = int let compare = compare 
  let hash = Hashtbl.hash let equal = (=)
end    
module G = Graph.Imperative.Digraph.Concrete(Int)
*)

module Int : Graph.Sig.COMPARABLE with type t = int =
struct
   type t = int
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

module G = Graph.Imperative.Digraph.Concrete(Int)

module SCC = Graph.Components.Make(G)    

 (* Use of Graphviz *)

module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name v = string_of_int v
   let vertex_attributes v = let s = string_of_int v in [`Label s]
   let default_edge_attributes g = []
   let edge_attributes e = []
   let get_subgraph v = None
end

module Dot = Graph.Graphviz.Dot(DotGraph) 

  let dump_graph g = 
    let oc = open_out "constraints.dot" in
    Dot.output_graph oc g;
    close_out oc

 
(* Given list [(u,v)] returns a numbering [(ui,ri)] s.t. 
 * 1. if ui,uj in same SCC then ri = rj
 * 2. if ui -> uj then ui >= uj *)


let ints_to_string xs =
  "["^(String.concat "," (List.map string_of_int xs))^"]"

let scc_rank uvs = 
  let g = G.create () in
  let _ = List.iter (fun (u,v) -> G.add_edge g u v) uvs in
  let _ = dump_graph g in
  let a = SCC.scc_array g in
  let _ = Printf.printf "dep graph: vertices =  %d, sccs = %d \n" (G.nb_vertex g) (Array.length a) in
  let _ = Printf.printf "scc sizes: " in
  let _ = Array.iteri (fun i xs -> Printf.printf "%d : %s \n" i (ints_to_string xs)) a in 
  let _ = Printf.printf "\n" in
  let sccs = array_to_index_list a in
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

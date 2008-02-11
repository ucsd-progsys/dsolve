module StringMap = Map.Make(struct type t = string let compare = compare end)

module ComparablePath = struct
  type t = Path.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module PathMap = Map.Make(ComparablePath)

let rec map3 f xs ys zs = match (xs, ys, zs) with
  | ([], [], []) -> []
  | (x :: xs, y :: ys, z :: zs) -> f x y z :: map3 f xs ys zs
  | _ -> assert false

let flap f xs = 
  List.flatten (List.map f xs)

let flap2 f xs ys = 
  List.flatten (List.map2 f xs ys)

let flap3 f xs ys zs =
  List.flatten (map3 f xs ys zs)

let rec expand f xs ys =
  match xs with
  | [] -> ys
  | x::xs ->
      let (xs',ys') = f x in
      expand f (xs' @ xs) (ys'@ys)

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
(************* Output levels ************************************)
(****************************************************************)

(* verbosity levels by purpose *)
let ol_always = 0
let ol_solve_error = 1
let ol_warning = 1
let ol_solve_master = 2
let ol_solve_stats = 2
let ol_timing = 2
let ol_default = 2
let ol_normalized = 3
let ol_solve = 10 
let ol_refine = 11 
let ol_scc =12 

let verbose_level = ref ol_default
let null_formatter = Format.make_formatter (fun a b c -> ()) ignore
let nprintf a = Format.fprintf null_formatter a
let ck_olev l = l <= !verbose_level

let cprintf l = if ck_olev l then Format.printf else nprintf
let ecprintf l = if ck_olev l then Format.eprintf else nprintf

let fcprintf ppf l = if ck_olev l then Format.fprintf ppf else nprintf

let icprintf printer l ppf = if ck_olev l then printer ppf else printer null_formatter

let cprintln l s = if ck_olev l then Printf.ksprintf (Format.printf "@[%s@\n@]") s else nprintf

(****************************************************************)
(************* SCC Ranking **************************************)
(****************************************************************)

module Int : Graph.Sig.COMPARABLE with type t = int * string =
struct
   type t = int * string 
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

module G = Graph.Imperative.Digraph.Concrete(Int)

module SCC = Graph.Components.Make(G)    

 (* Use of Graphviz *)

let io_to_string = function 
  | Some i -> string_of_int i 
  | None -> "*"

let xs_to_string f xs =
  "["^(String.concat "," (List.map f xs))^"]"

module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name (i,s) = Printf.sprintf "(%d,%s)" i s 
   let vertex_attributes v = [`Label (vertex_name v)]
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
let scc_rank f ijs = 
  let g = G.create () in
  let _ = List.iter (fun (i,j) -> G.add_edge g (i,(f i)) (j,(f j))) ijs in
  let _ = dump_graph g in
  let a = SCC.scc_array g in
  let _ = cprintf ol_scc "@[dep@ graph:@ vertices@ =@ @ %d,@ sccs@ =@ %d@ @\n@]" 
          (G.nb_vertex g) (Array.length a);
          cprintf ol_scc "@[scc@ sizes:@\n@]";
          let int_s_to_string (i,s) = Printf.sprintf "(%d,%s)" i s in
          Array.iteri 
          (fun i xs -> 
          cprintf ol_scc "@[%d@ :@ %s@ @\n@]" i (xs_to_string int_s_to_string xs)) a;
          cprintf ol_scc "@[@\n@]" in
  let sccs = array_to_index_list a in
  flap (fun (i,vs) -> List.map (fun (j,_) -> (j,i)) vs) sccs

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



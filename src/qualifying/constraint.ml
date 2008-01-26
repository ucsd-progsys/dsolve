open Format
open Wellformed
module F = Frame
module Le = Lightenv
module Pat = Pattern
module P = Predicate
module TP = TheoremProver

module C = Common
module VM = C.PathMap 
module Sol = Hashtbl.Make(C.ComparablePath)
module SM = C.StringMap
module Cf = Clflags

(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type fc_id = int option 
type subref_id = int 

module SIM = Map.Make(struct type t = subref_id let compare = compare end)

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of F.t Le.t * guard_t * F.t * F.t * origin * fc_id
  | WFFrame of F.t Le.t * F.t * origin * fc_id 

and origin =
  | Loc of Location.t 
  | Assert of Location.t 
  | Cstr of frame_constraint 

type refinement_constraint =
  | SubRef of F.t Le.t * guard_t * F.refinement * F.refinement * (subref_id option) 
  | WFRef of F.t Le.t * F.refinement * (subref_id option) 

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

let get_fc_id = function 
  | SubFrame (_,_,_,_,_,io) | WFFrame (_,_,_,io) -> io

(* Unique variable to qualify when testing sat, applicability of qualifiers...
 * this is passed into the solver *)
let qual_test_var = Path.mk_ident "AA"

let is_simple_constraint = function 
  SubRef (_, _, ([],F.Qvar _), ([], F.Qvar _), _) -> true | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let solution_map s k = 
  C.do_catch 
    (Printf.sprintf "ERROR: solution_map couldn't find: %s" (Path.name k))
    (Sol.find s) k  

(**************************************************************)
(**************************** Stats ***************************)
(**************************************************************)

let stat_refines = ref 0
let stat_solved_constraints = ref 0
let stat_valid_constraints = ref 0
let stat_matches = ref 0 

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

(* FIX: 1. arg orders, 2. print index *)

let guard_predicate () g = 
  P.big_and 
    (List.map 
      (fun (v,b) -> 
         let p = P.equals (P.Var v, P.PInt 1) in 
         if b then p else P.Not p) 
      g)

let environment_predicate s env =
  P.big_and (Le.maplist (F.predicate (solution_map s)) env)

let pprint_local_binding ppf = function
  | (Path.Pident _ as k, v) -> fprintf ppf "@[%s@;=>@;<1 2>%a@],@;<1 2>" (Path.unique_name k) F.pprint v
  | _ -> ()

let pprint_env_pred so ppf env =
  match so with
  | Some s -> P.pprint ppf (environment_predicate s env)
  | _ -> Le.iter (fun x t -> pprint_local_binding ppf (x, t)) env

let pprint ppf = function
  | SubFrame (_,_,f1,f2,_,_) ->
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (_,f,_,_) ->
      F.pprint ppf f

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = function
  | SubRef (env,g,r1,r2,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@;<1 0>|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_env_pred so) env P.pprint (guard_predicate () g) 
      F.pprint_refinement r1 F.pprint_refinement r2 
  | WFRef (env,r,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_env_pred so) env F.pprint_refinement r 


(**************************************************************)
(************* Constraint Simplification & Splitting **********) 
(**************************************************************)

let simplify_frame gm x f = 
  if not (Le.mem x gm) then f else
    let pos = Le.find x gm in
    match f with 
    | F.Fconstr (a,b,(subs,F.Qconst[(v1,v2,P.Iff (P.Var v3,p))])) when v2 = v3 ->
        let p' = if pos then p else P.Not p in
        F.Fconstr (a,b,(subs,F.Qconst[(v1,v2,p')])) 
    | F.Frecord (a,b,(subs,F.Qconst[(v1,v2,P.Iff (P.Var v3,p))])) when v2 = v3 ->
        let p' = if pos then p else P.Not p in
        F.Frecord (a,b,(subs,F.Qconst[(v1,v2,p')])) 
    | _ -> f

let simplify_env env g =
  let gm = List.fold_left (fun m (x,b)  -> Le.add x b m) Le.empty g in
  Le.fold 
    (fun x f env' -> 
      match f with | F.Fconstr _ | F.Frecord _ -> 
        Le.add x (simplify_frame gm x f) env' 
      | _ -> env')
    env Le.empty

let simplify_fc  c = 
  match c with 
  | WFFrame _ -> c 
  | SubFrame (env,g,a,b,c,d) -> SubFrame(simplify_env env g, g, a,b,c,d)

(* Notes:
  * 1. If the labels disagree, we don't resolve. Instead, we 
  * proceed with the best information we have, probably losing 
  * chances to assert qualifiers along the way. 
  * 2. pmr: because we were only filtering through invariant types 
  * anyway, we might as well just use invariants until we start 
  * getting problems from it --- for now, it's too much trouble 
  * to work around all the BigArray stuff
  *)

let lequate_cs eqb env g c f1 f2 =
  let fci = get_fc_id c in
  if eqb 
  then [SubFrame(env,g,f1,f2,Cstr c,fci); SubFrame(env,g,f2,f1,Cstr c,fci)]
  else [SubFrame(env,g,f1,f2,Cstr c,fci)]

let match_and_extend env (l1,f1) (l2,f2) = 
  match (l1,l2) with
  | (Some p, None) | (None, Some p) -> Pat.env_bind env p f2
  | (Some p1, Some p2) when Pat.same p1 p2 -> Pat.env_bind env p1 f2
  | _  -> env (* 1 *)
 
let is_mutable m = 
  m = Asttypes.Mutable

let split_sub = function WFFrame _ -> assert false | SubFrame (env,g,f1,f2,_,_) as c -> 
  match (f1, f2) with
  | (F.Farrow (l1, f1, f1'), F.Farrow (l2, f2, f2')) -> 
      let env' = match_and_extend env (l1,f1) (l2,f2) in 
      ((lequate_cs false env g c f2 f1) @ (lequate_cs false env' g c f1' f2'),
       [])
  | (F.Fvar _, F.Fvar _) | (F.Funknown, F.Funknown) ->
      ([],[]) 
  | (F.Fconstr (p1, f1s, r1), F.Fconstr(p2, f2s, r2)) ->  (* 2 *)
      (C.flap2 (lequate_cs true env g c) f1s f2s,
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Ftuple f1s, F.Ftuple f2s) ->
      (C.flap2 (lequate_cs false env g c) f1s f2s,
       [])
  | (F.Frecord (_, fld1s, r1), F.Frecord (_, fld2s, r2)) ->
      (C.flap2 
         (fun (f1',_,m) (f2',_,_) -> lequate_cs (is_mutable m) env g c f1' f2') 
         fld1s fld2s, 
       if List.exists (fun (_, _,m) -> is_mutable m) fld1s 
       then [(Cstr c, SubRef (env,g,r1,r2,None)); (Cstr c, SubRef (env,g,r2,r1,None))]
       else [(Cstr c, SubRef (env,g,r1,r2,None))])
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf = function SubFrame _ -> assert false | WFFrame (env,f,_,_) as c -> 
  match f with
  | F.Fconstr (_, l, r) ->
      (List.map (fun li -> WFFrame (env, li, Cstr c, None)) l,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Farrow (l, f, f') ->
      let env' = match l with None -> env | Some p -> Pat.env_bind env p f in
      ([WFFrame (env, f, Cstr c, None); WFFrame (env', f', Cstr c, None)],
       [])
  | F.Ftuple fs ->
      (List.map (fun f' -> WFFrame (env, f', Cstr c, None)) fs,
       [])
  | F.Frecord (_, fs, r) ->
      (List.map (fun (f',_,_) -> WFFrame (env, f', Cstr c, None)) fs,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Fvar _ | F.Funknown ->
      ([],[]) 

let split cs =
  assert (List.for_all (fun c -> None <> get_fc_id c) cs);
  C.expand 
    (fun c -> match c with SubFrame _ -> split_sub c | WFFrame _ -> split_wf c) 
    cs [] 

(**************************************************************)
(********************* Constraint Indexing ********************) 
(**************************************************************)

module WH = 
  Heap.Functional(
    struct 
      type t = subref_id * (int * bool * fc_id) 
      let compare (_,(i,j,k)) (_,(i',j',k')) = 
        if i <> i' then compare i i' else 
          if j <> j' then compare j j' else compare k' k
    end)

type ref_index = 
  { orig: frame_constraint SIM.t;       (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;           (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  (* C.do_catch "get_rank" (SIM.find (get_ref_id c)) sri.rank *)
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,(_,qe),_,_) ->
  let ks = Le.fold (fun _ f l -> F.refinement_vars f @ l) env [] in
  match qe with F.Qvar k -> k::ks | _ -> ks 

let rhs_k = function
  | SubRef (_,_,_,(_,F.Qvar k),_) -> Some k
  | _ -> None

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm VM.empty in
  let (dm,deps) = 
    SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, List.rev_append deps deps'))
      cm (SIM.empty,[]) in
  let flabel i = C.io_to_string (get_fc_id (SIM.find i om)) in
  let rm = 
    List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = get_fc_id (SIM.find id om) in 
        SIM.add id (r,b,fci) rm)
      SIM.empty (C.scc_rank flabel deps) in
  (dm,rm)

let fresh_refc = 
  let i = ref 0 in
  fun c -> 
    let i' = incr i; !i in
    match c with  
    | WFRef (env,r,None) -> WFRef (env,r,Some i')
    | SubRef (env,g,r1,r2,None) -> SubRef (env,g,r1,r2,Some i')
    | _ -> assert false

(* API *)
let make_ref_index ocs = 
  let ics = List.map (fun (o,c) -> (o,fresh_refc c)) ocs in
  let (om,cm) = 
    List.fold_left 
      (fun (om,cm) (o,c) ->
        let o = match o with Cstr fc -> fc | _ -> assert false in
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = make_rank_map om cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig
(* API *)
let get_ref_deps sri c =
  let is' = try SIM.find (get_ref_id c) sri.depm with Not_found -> [] in
  List.map (get_ref_constraint sri) is'

(* API *)
let get_ref_constraints sri = 
  SIM.fold (fun _ c cs -> c::cs) sri.cnst [] 

(* API *)
let iter_ref_constraints sri f = 
  SIM.iter (fun _ c -> f c) sri.cnst

(* API *)
let push_worklist sri w cs =
  List.fold_left 
    (fun w c -> 
      let id = get_ref_id c in
      let _ = C.cprintf C.ol_solve "@[Pushing@ %d@\n@]" id in 
      if Hashtbl.mem sri.pend id then w else 
        let _ = Hashtbl.replace sri.pend id () in
        WH.add (id,get_ref_rank sri c) w)
    w cs

(* API *)
let pop_worklist sri w =
  try 
    let id = fst (WH.maximum w) in
    let _ = Hashtbl.remove sri.pend id in
    (Some (get_ref_constraint sri id), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let make_initial_worklist sri =
  let cs = List.filter is_subref_constraint (get_ref_constraints sri) in
  push_worklist sri WH.empty cs 

(**************************************************************)
(************************** Refinement ************************)
(**************************************************************)

let pred_to_string p = Format.sprintf "@[%a@]" P.pprint p 

let refine_simple s k1 k2 =
  let q1s  = Sol.find s k1 in
  let q2s  = Sol.find s k2 in
  let q2s' = List.filter (fun q -> List.mem q q1s) q2s in
  let _    = Sol.replace s k2 q2s' in
  let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length q2s) (List.length q2s') in
  List.length q2s' <> List.length q2s

let qual_implied s lhs lhsm rhs_subs q =
  let rhs = F.refinement_predicate (solution_map s) qual_test_var (rhs_subs, F.Qconst [q]) in
  let (cached, cres) = if !Cf.cache_queries then TP.check_table lhs rhs else (false, false) in
  if cached then cres else 
    if (not !Cf.no_simple_subs) && SM.mem (pred_to_string rhs) lhsm then (incr stat_matches; true) else
      let rv = Bstats.time "refinement query" (TP.implies lhs) rhs in
      let _ = incr stat_solved_constraints in
      let _ = if rv then incr stat_valid_constraints in
      rv 

let qual_wf s env subs q =
  refinement_well_formed env (solution_map s) (subs,F.Qconst [q]) qual_test_var

let refine sri s c =
  let _ = incr stat_refines in
  match c with
  | SubRef (_, _, ([], F.Qvar k1), ([], F.Qvar k2), _)
    when not (!Cf.no_simple || !Cf.verify_simple) -> 
      refine_simple s k1 k2
  | SubRef (env,g,r1, (rhs_subs, F.Qvar k2), _)  ->
      let lhs = 
        let gp = Bstats.time "make guardp" (guard_predicate ()) g in
        let envp = Bstats.time "make envp" (environment_predicate s) env in
        let r1p = Bstats.time "make r1p" (F.refinement_predicate (solution_map s) qual_test_var) r1 in
        P.big_and [envp;gp;r1p] in
      let q2s  = solution_map s k2 in
      let q2s' = List.filter (qual_implied s lhs [] rhs_subs) q2s in 
      let _ = Sol.replace s k2 q2s' in
      let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length q2s) (List.length q2s') in
      (List.length q2s <> List.length q2s')
  | WFRef (env,(subs, F.Qvar k),_) -> 
      let qs  = solution_map s k in
      let qs' = List.filter (qual_wf s env subs) qs in
      let _   = Sol.replace s k qs' in
      (List.length qs <> List.length qs')
  | _ -> false


(**************************************************************)
(********************** Constraint Satisfaction ***************)
(**************************************************************)

let sat s = function
  | SubRef (env, g, r1, r2, _) ->
      let gp = Bstats.time "make guardp" (guard_predicate ()) g in
      let envp = environment_predicate s env in
      let p1 = F.refinement_predicate (solution_map s) qual_test_var r1 in
      let p2 = F.refinement_predicate (solution_map s) qual_test_var r2 in
        TP.backup_implies (P.big_and [envp; gp; p1]) p2
  | WFRef (env, r, _) as c -> 
      let rv = refinement_well_formed env (solution_map s) r qual_test_var in
         C.asserts (Printf.sprintf "ERROR: wf is unsat! (%d)" (get_ref_id c)) rv;
         rv 

let unsat_constraints sri s =
  C.map_partial
    (fun c -> if sat s c then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(**************************************************************)
(************************ Initial Solution ********************)
(**************************************************************)

(* If a variable only ever appears on the left hand side, the variable is
 * unconstrained; this generally indicates an uncalled function.
 * When we have such an unconstrained variable, we simply say we don't
 * know anything about its value.  For uncalled functions, this will give
 * us the type which makes the least assumptions about the input. *)

let make_initial_solution sri qs =
  let s = Sol.create 17 in
  let addrv = function
  | ((_, F.Qconst _),_) -> ()
  | ((_, F.Qvar k),true) -> Sol.replace s k qs
  | ((_, F.Qvar k),false) -> if not (Sol.mem s k) then Sol.replace s k [] in
  SIM.iter 
    (fun _ c -> match c with 
    | SubRef (_, _, r1, r2, _) -> addrv (r1,false); addrv (r2,true)
    | WFRef (_, r, _) -> addrv (r,false)) sri.cnst;
  s

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)
 
let dump_constraints sri = 
  if !Cf.dump_constraints then
  (printf "@[Refinement Constraints@.@\n@]";
  iter_ref_constraints sri
  (fun c -> printf "@[%a@.@]" (pprint_ref None) c))
    (* let cs = get_ref_constraints sri in
    Oprint.print_list (pprint_ref None) (fun ppf -> fprintf ppf "@.@.")
    std_formatter cs *)

let dump_solution_stats s = 
  let kn  = Sol.length s in
  let (sum, max, min) =   
    (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
     Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
     Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
  C.cprintf C.ol_solve_stats "@[Quals:@\n\tTotal:@ %d@\n\tAvg:@ %f@\n\tMax:@ %d@\n\tMin:@ %d@\n@\n@]"
  sum ((float_of_int sum) /. (float_of_int kn)) max min;
  print_flush ()
  
let dump_solving qs sri s step =
  if step = 0 then 
    let cs = get_ref_constraints sri in 
    let qn  = List.length qs in
    let kn  = Sol.length s in
    let wcn = List.length (List.filter is_wfref_constraint cs) in
    let rcn = List.length (List.filter is_subref_constraint cs) in
    let scn = List.length (List.filter is_simple_constraint cs) in
    (dump_constraints sri;
     C.cprintf C.ol_solve_stats "@[%d@ instantiated@ qualifiers@\n@\n@]" qn; 
     C.cprintf C.ol_solve_stats "@[%d@ variables@\n@\n@]" kn;
     C.cprintf C.ol_solve_stats "@[%d@ total@ quals@\n@\n@]" (kn * qn); 
     C.cprintf C.ol_solve_stats "@[%d@ split@ wf@ constraints@\n@\n@]" wcn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ subtyping@ constraints@\n@\n@]" rcn;
     C.cprintf C.ol_solve_stats "@[%d@ simple@ subtyping@ constraints@\n@\n@]" scn;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (C.cprintf C.ol_solve_stats "@[solution@ refinement@ completed:@\n\t%d@ iterations@ of@ refine@\n@\n@]" !stat_refines;
     C.cprintf C.ol_solve_stats "@[Solved@ %d@ constraints;@ %d@ valid@]@.@." 
     !stat_solved_constraints !stat_valid_constraints;
     TP.dump_simple_stats ();
     C.cprintf C.ol_solve_stats "@[Solved@ %d@ constraints@ by@ matching@\n@]" !stat_matches;
     dump_solution_stats s;
     flush stdout)

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

let rec solve_sub sri s w = 
  let _ = if !stat_refines mod 100 = 0 then C.cprintf C.ol_solve "@[num@ refines@ =@ %d@\n@]" !stat_refines in
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let (r,b,fci) = get_ref_rank sri c in
    let _ = C.cprintf C.ol_solve "@[Refining:@ %d@ in@ scc@ (%d,%b,%s):@]"
            (get_ref_id c) r b (C.io_to_string fci) in
    let w' = if refine sri s c then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub sri s w'

let solve_wf sri s = 
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine sri s c) | _ -> ())

let solve qs cs = 
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let sri = make_ref_index (split cs) in
  let s = make_initial_solution sri qs in
  let _ = dump_solving qs sri s 0  in 
  let _ = Bstats.time "solving wfs" (solve_wf sri) s in
  let _ = dump_solving qs sri s 1 in
  let w = make_initial_worklist sri in
  let _ = Bstats.time "solving sub" (solve_sub sri s) w in
  let _ = dump_solving qs sri s 2 in
  let _ = TP.clear_cache () in
  let unsat = Bstats.time "testing solution" (unsat_constraints sri) s in
  let _ = if List.length unsat != 0 then 
          begin
            C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
            List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat
          end in
  (solution_map s, (List.map (fun (a, b) -> b)  unsat))

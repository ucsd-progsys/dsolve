open Format
module F = Frame
module Le = Lightenv
module Pat = Pattern
module P = Predicate
module TP = TheoremProver
module VM = Map.Make(Common.ComparablePath)
module Sol = Hashtbl.Make(Common.ComparablePath)
module C = Common

(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type subref_id = int 

module SIM = Map.Make(struct type t = subref_id let compare = compare end)

module WH = 
  Heap.Functional(
    struct 
      type t = subref_id * int 
      let compare (_,i) (_,i') = compare i i' 
    end)

type frame_constraint =
  | SubFrame of F.t Le.t * P.t * F.t * F.t * origin
  | WFFrame of F.t Le.t * F.t * origin

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of frame_constraint

type refinement_constraint =
  | SubRef of F.t Le.t * P.t * F.refinement * F.refinement * (subref_id option) 
  | WFRef of F.t Le.t * F.refinement * (subref_id option) 

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

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
  Common.do_catch 
    (Printf.sprintf "solution_map couldn't find: %s" (Path.name k))
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
  | SubFrame (_, _, f1, f2, _) ->
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (_, f, _) ->
      F.pprint ppf f

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = function
  | SubRef (env,g,r1,r2,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@;<1 0>|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_env_pred so) env P.pprint g F.pprint_refinement r1 F.pprint_refinement r2 
  | WFRef (env,r,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_env_pred so) env F.pprint_refinement r 
(**************************************************************)
(********************* Constraint Splitting *******************) 
(**************************************************************)

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
  if eqb 
  then [SubFrame(env,g,f1,f2,Cstr c); SubFrame(env,g,f2,f1,Cstr c)]
  else [SubFrame(env,g,f1,f2,Cstr c)]

let match_and_extend env (l1,f1) (l2,f2) = 
  match (l1,l2) with
  | (Some p, None) | (None, Some p) -> Pat.env_bind env p f2
  | (Some p1, Some p2) when Pat.same p1 p2 -> Pat.env_bind env p1 f2
  | _  -> env (* 1 *)
 
let is_mutable m = 
  m = Asttypes.Mutable

let split_sub = function WFFrame _ -> assert false | SubFrame (env,g,f1,f2,_) as c -> 
  match (f1, f2) with
  | (F.Farrow (l1, f1, f1'), F.Farrow (l2, f2, f2')) -> 
      let env' = match_and_extend env (l1,f1) (l2,f2) in 
      ((lequate_cs false env g c f2 f1) @ (lequate_cs false env' g c f1' f2'),
       [])
  | (F.Fvar _, F.Fvar _) | (F.Funknown, F.Funknown) ->
      ([],[]) 
  | (F.Fconstr (p1, f1s, r1), F.Fconstr(p2, f2s, r2)) ->  (* 2 *)
      (Common.flap2 (lequate_cs true env g c) f1s f2s,
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Ftuple f1s, F.Ftuple f2s) ->
      (Common.flap2 (lequate_cs false env g c) f1s f2s,
       [])
  | (F.Frecord (_, fld1s, r1), F.Frecord (_, fld2s, r2)) ->
      (Common.flap2 
         (fun (f1',_,m) (f2',_,_) -> lequate_cs (is_mutable m) env g c f1' f2') 
         fld1s fld2s, 
       if List.exists (fun (_, _,m) -> is_mutable m) fld1s 
       then [(Cstr c, SubRef (env,g,r1,r2,None)); (Cstr c, SubRef (env,g,r2,r1,None))]
       else [(Cstr c, SubRef (env,g,r1,r2,None))])
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf = function SubFrame _ -> assert false | WFFrame (env,f,_) as c -> 
  match f with
  | F.Fconstr (_, l, r) ->
      (List.map (fun li -> WFFrame (env, li, Cstr c)) l,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Farrow (l, f, f') ->
      let env' = match l with None -> env | Some p -> Pat.env_bind env p f in
      ([WFFrame (env, f, Cstr c); WFFrame (env', f', Cstr c)],
       [])
  | F.Ftuple fs ->
      (List.map (fun f' -> WFFrame (env, f', Cstr c)) fs,
       [])
  | F.Frecord (_, fs, r) ->
      (List.map (fun (f',_,_) -> WFFrame (env, f', Cstr c)) fs,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Fvar _ | F.Funknown ->
      ([],[]) 

let split cs = 
  Common.expand 
    (fun c -> match c with SubFrame _ -> split_sub c | WFFrame _ -> split_wf c) 
    cs [] 

(**************************************************************)
(********************* Constraint Indexing ********************) 
(**************************************************************)

type ref_index = 
  { orig: frame_constraint SIM.t;                 (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: int SIM.t;                    (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)}

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  (* Common.do_catch "get_rank" (SIM.find (get_ref_id c)) sri.rank *)
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "@[No@ rank@ for:@ %a@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  Common.do_catch  "get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,(_,qe),_,_) ->
  let ks = Le.fold (fun _ f l -> F.refinement_vars f @ l) env [] in
  match qe with F.Qvar k -> k::ks | _ -> ks 

let rhs_k = function
  | SubRef (_,_,_,(_,F.Qvar k),_) -> Some k
  | _ -> None

let make_rank_map cm =
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
        match rhs_k c with None -> (dm,(id,id)::deps) | Some k -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, List.rev_append deps deps'))
      cm (SIM.empty,[]) in
  let ranks = Common.scc_rank deps in
  (dm,List.fold_left (fun rm (id,r) -> SIM.add id r rm) SIM.empty ranks)

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
  let (dm,rm) = make_rank_map cm in
  {orig = om; cnst = cm; rank = rm; depm = dm}

let get_ref_orig sri c = 
  Common.do_catch "get_ref_orig" (SIM.find (get_ref_id c)) sri.orig
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
  List.fold_left (fun w c -> WH.add (get_ref_id c,get_ref_rank sri c) w) w cs

(* API *)
let pop_worklist sri w =
  try (Some (get_ref_constraint sri (fst(WH.maximum w))), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let make_initial_worklist sri =
  let cs = List.filter is_subref_constraint (get_ref_constraints sri) in
  push_worklist sri WH.empty cs 

(**************************************************************)
(************************** Refinement ************************)
(**************************************************************)

let refine_simple s k1 k2 =
  let q1s  = Sol.find s k1 in
  let q2s  = Sol.find s k2 in
  let q2s' = List.filter (fun q -> List.mem q q1s) q2s in
  let _    = Sol.replace s k2 q2s' in
  List.length q2s' <> List.length q2s

let qual_implied s lhs lhs_ps rhs_subs q =
  let rhs = F.refinement_predicate (solution_map s) qual_test_var (rhs_subs, F.Qconst [q]) in
  let (cached, cres) = if !Clflags.cache_queries then TP.check_table lhs rhs else (false, false) in
  if cached then cres else 
    if (not !Clflags.no_simple_subs) && List.mem rhs lhs_ps then (incr
    stat_matches; true) else
      let rv = Bstats.time "refinement query" (TP.implies lhs) rhs in
      let _ = incr stat_solved_constraints in
      let _ = if rv then incr stat_valid_constraints in
      rv 

let qual_wf s env subs q =
  F.refinement_well_formed env (solution_map s) (subs,F.Qconst [q]) qual_test_var

let refine s c = 
  let _ = incr stat_refines in
  match c with
  | SubRef (_, _, ([], F.Qvar k1), ([], F.Qvar k2), _)
    when not (!Clflags.no_simple || !Clflags.verify_simple) -> 
      refine_simple s k1 k2
  | SubRef (env,g,r1, (rhs_subs, F.Qvar k2), _)  ->
      let lhs = 
        let envp = Bstats.time "make envp" (environment_predicate s) env in
        let r1p = Bstats.time "make r1p" (F.refinement_predicate (solution_map s) qual_test_var) r1 in
        P.big_and [envp;g;r1p] in
      let q2s  = solution_map s k2 in
      let q2s' = List.filter (qual_implied s lhs [] rhs_subs) q2s in 
      let _    = Sol.replace s k2 q2s' in
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
  | SubRef (env, guard, r1, r2, _) ->
      let envp = environment_predicate s env in
      let p1 = F.refinement_predicate (solution_map s) qual_test_var r1 in
      let p2 = F.refinement_predicate (solution_map s) qual_test_var r2 in
        TP.backup_implies (P.big_and [envp; guard; p1]) p2
  | WFRef (env, r, _) ->
      let rv = F.refinement_well_formed env (solution_map s) r qual_test_var in
      Common.asserts "wf is unsat!" rv; rv

let unsat_constraints sri s =
  Common.map_partial
    (fun c -> if sat s c then None else Some (get_ref_orig sri c))
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
  (* if !Clflags.dump_constraints then*)
  printf "Refinement Constraints @.";
  iter_ref_constraints sri 
  (fun c -> printf "@[%a@.@]" (pprint_ref None) c)
    (* let cs = get_ref_constraints sri in 
    Oprint.print_list (pprint_ref None) (fun ppf -> fprintf ppf "@.@.") 
    std_formatter cs *)

let dump_solution_stats s = 
  let kn  = Sol.length s in
  let (sum, max, min) =   
    (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
     Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
     Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
  Printf.printf "Quals:\n\tTotal: %d\n\tAvg: %f\n\tMax: %d\n\tMin: %d\n\n"
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
     Printf.printf "%d instantiated qualifiers\n\n" qn; 
     Printf.printf "%d variables\n\n" kn;
     Printf.printf "%d total quals\n\n" (kn * qn); 
     Printf.printf "%d split wf constraints\n\n" wcn;
     Printf.printf "%d split subtyping constraints\n\n" rcn;
     Printf.printf "%d simple subtyping constraints\n\n" scn;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (Printf.printf "solution refinement completed:\n\t%d iterations of refine\n\n" !stat_refines;
     Format.printf "@[Solved@ %d@ constraints;@ %d@ valid@]@.@." 
     !stat_solved_constraints !stat_valid_constraints;
     TP.dump_simple_stats ();
     Format.printf "@[Solved@ %d@ constraints@ by@ matching@\n@]" !stat_matches;
     flush stdout)

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

let rec solve_sub sri s w = 
  let _ = if !stat_refines mod 100 = 0 then Printf.printf "num refines = %d \n" !stat_refines in
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let w' = if refine s c then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub sri s w'

let solve_wf sri s = 
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine s c) | _ -> ()) 

let solve qs cs = 
  let _ = Printf.printf "This is the new solver \n" in
  let sri = make_ref_index (split cs) in
  let s = make_initial_solution sri qs in
  let _ = dump_solving qs sri s 0  in 
  let _ = Bstats.time "solving wfs" (solve_wf sri) s in
  let _ = dump_solving qs sri s 1 in
  let w = make_initial_worklist sri in
  let _ = Bstats.time "solving sub" (solve_sub sri s) w in
  let _ = TP.clear_cache () in
  let unsat = Bstats.time "testing solution" (unsat_constraints sri) s in
  (solution_map s,unsat)

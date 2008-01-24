(* Common theorem prover interface *)

(********************************************************************************)
(************************** Unbreaking Division *********************************)
(********************************************************************************)

let rec fixdiv p = 
  let expr_isdiv = 
    function Predicate.Binop(_, Predicate.Div, _) -> true
      | _ -> false in 
  let pull_const =
    function Predicate.PInt(i) -> i
      | _ -> 1 in
  let pull_divisor =
    function Predicate.Binop(_, Predicate.Div, d1) ->
      pull_const d1 
      | _ -> 1 in
  let rec apply_mult m e =
    match e with
        Predicate.Binop(n, Predicate.Div, Predicate.PInt(d)) ->
          (*let _ = assert ((m/d) * d = m) in*)
            Predicate.Binop(Predicate.PInt(m/d), Predicate.Times, n) 
      | Predicate.Binop(e1, rel, e2) ->
          Predicate.Binop(apply_mult m e1, rel, apply_mult m e2) 
      | Predicate.PInt(i) -> Predicate.PInt(i*m)
      | e -> Predicate.Binop(Predicate.PInt(m), Predicate.Times, e)
  in
  let rec pred_isdiv = 
    function Predicate.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
      | Predicate.Iff (px, q) -> expr_isdiv px || pred_isdiv q
      | Predicate.And(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | Predicate.Or(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | Predicate.True -> false
      | Predicate.Not p -> pred_isdiv p in
  let calc_cm e1 e2 =
    pull_divisor e1 * pull_divisor e2 in
    if pred_isdiv p then
      match p with
          Predicate.Atom(e, r, e') -> 
            let m = calc_cm e e' in
            let e'' = Predicate.Binop(e', Predicate.Minus, Predicate.PInt(1)) in
            let bound (e, r, e', e'') = 
              Predicate.And(Predicate.Atom(apply_mult m e, Predicate.Gt, apply_mult m e''),
                            Predicate.Atom(apply_mult m e, Predicate.Le, apply_mult m e'))
            in
              (match (e, r, e') with
                   (Predicate.Var v, Predicate.Eq, e') ->
                     bound (e, r, e', e'')
                 | (Predicate.PInt v, Predicate.Eq, e') ->
                     bound (e, r, e', e'')
                 | _ -> p) 
        | Predicate.And(p1, p2) -> 
            let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
            let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
              Predicate.And(p1, p2)      
        | Predicate.Or(p1, p2) ->
            let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
            let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
              Predicate.Or(p1, p2) 
        | Predicate.Not p1 -> Predicate.Not(fixdiv p1) 
        | p -> p
    else p

module DefaultProver = TheoremProverYices.Prover
module BackupProver = TheoremProverYices.Prover

exception Provers_disagree of bool * bool

(* Check that the default and backup provers both provide the same
   result to a query *)
let check_result f g arg =
  if not !Clflags.check_queries then Bstats.time "calling PI" f arg
  else
    let fres = f arg in
    let gres = g arg in
      if fres != gres then raise (Provers_disagree (fres, gres))
      else fres

let do_both_provers f g arg =
  f arg; if !Clflags.check_queries then g arg else ()

let push p = do_both_provers DefaultProver.push BackupProver.push (fixdiv p)

let pop () = do_both_provers DefaultProver.pop BackupProver.pop ()

let valid p = check_result DefaultProver.valid BackupProver.valid (fixdiv p)

let num_queries = ref 0
let hits = ref 0
let dump_interval = 10000
let qcache = Hashtbl.create 10000

let dump_simple_stats () =
  Format.printf "@[Prover cache stats:@ %d@ queries,@ %d@ cache@ hits@\n@]" !num_queries !hits; flush stdout

let clear_cache () =
  Hashtbl.clear qcache; num_queries := 0; hits := 0

let check_table p q =
  let ipq = Predicate.implies (p, q) in
    if Hashtbl.mem qcache ipq then (incr hits;(true, Hashtbl.find qcache ipq))
                              else (false, false)

let check_implies default backup p q =
  
  let _ = incr num_queries in
  let _ = if (!num_queries mod dump_interval) = 0 then dump_simple_stats () else () in 
  let use_cache = !Clflags.cache_queries in
  let ipq = Predicate.implies (p, q) in

  let cached = (Bstats.time "cache lookup" (Hashtbl.mem qcache) ipq) && use_cache in
  let _ = if cached then incr hits in

  let (p, q) = Bstats.time "fixing div" (fun () -> (fixdiv p, fixdiv q)) () in
  let res = if cached then Bstats.time "finding in cache " (Hashtbl.find qcache) ipq
                else check_result default backup (p, q) in
    if !Clflags.dump_queries then
      Format.printf "@[%s%a@;<1 0>=>@;<1 0>%a@;<1 2>(%B)@]@.@."
        (if cached then "cached:" else "") Predicate.pprint p Predicate.pprint q res;
    (if cached then () else if use_cache then Hashtbl.replace qcache ipq res); res

let implies p q =
  if !Clflags.always_use_backup_prover then
    Bstats.time "TP.ml prover query" (check_implies BackupProver.implies DefaultProver.implies p) q
  else
    Bstats.time "TP.ml prover query" (check_implies DefaultProver.implies BackupProver.implies p) q

let backup_implies p q = check_implies BackupProver.implies DefaultProver.implies p q

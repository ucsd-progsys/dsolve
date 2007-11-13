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
          let _ = assert ((m/d) * d = m) in
            Predicate.Binop(Predicate.PInt(m/d), Predicate.Times, n) 
      | Predicate.Binop(e1, rel, e2) ->
          Predicate.Binop(apply_mult m e1, rel, apply_mult m e2) 
      | Predicate.PInt(i) -> Predicate.PInt(i*m)
      | e -> Predicate.Binop(Predicate.PInt(m), Predicate.Times, e)
  in
  let rec pred_isdiv = 
    function Predicate.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
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

module DefaultProver = TheoremProverSimplify.Prover
module BackupProver = TheoremProverYices.Prover

exception Provers_disagree of bool * bool

(* Check that the default and backup provers both provide the same
   result to a query *)
let check_result f g arg =
  if not !Clflags.check_queries then
    f arg
  else
    let fres = f arg in
    let gres = g arg in
      if fres != gres then
        raise (Provers_disagree (fres, gres))
      else fres

let do_both_provers f g arg =
  f arg;
  if !Clflags.check_queries then
    g arg
  else
    ()

let push p =
  do_both_provers DefaultProver.push BackupProver.push (fixdiv p)

let pop () =
  do_both_provers DefaultProver.pop BackupProver.pop ()

let valid p =
  check_result DefaultProver.valid BackupProver.valid (fixdiv p)

let check_implies default backup p q =
  let (p, q) = (fixdiv p, fixdiv q) in
  let res = check_result default backup (fixdiv p, fixdiv q) in
    if !Clflags.dump_queries then
      Format.printf "@[%a@;<1 0>=>@;<1 0>%a@;<1 2>(%B)@]@.@."
        Predicate.pprint p Predicate.pprint q res;
    res

let implies p q =
  if !Clflags.always_use_backup_prover then
    check_implies BackupProver.implies DefaultProver.implies p q
  else
    check_implies DefaultProver.implies BackupProver.implies p q

let backup_implies p q =
  check_implies BackupProver.implies DefaultProver.implies p q

open OUnit
open Type
open Expr
open Infer


let typs_equal t1 t2 =
  let rec typs_eq_rec vars = function
      (GenTy(_, t1), GenTy(_, t2)) ->
        typs_eq_rec vars (t1, t2)
    | (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
        let (vars', eq') = typs_eq_rec vars (t1, t2) in
          if not eq' then
            (vars', false)
          else
            typs_eq_rec vars' (t1', t2')
    | (List t1, List t2) ->
        typs_eq_rec vars (t1, t2)
    | (TyVar a, TyVar b) ->
        begin try
          if List.assoc b vars = a then
            (vars, true)
          else
            (vars, false)
        with Not_found ->
          ((b, a)::vars, true)
        end
    | (Int, Int) ->
        (vars, true)
    | _ ->
        (vars, false)
  in let (_, eq) = typs_eq_rec [] (t1, t2) in eq


let infer_exp_typ e = ExpMap.find e (infer_shapes e)


let mklet (x, e1, e2) = Exp.Let(x, None, e1, e2, get_next_exp_id())
let mkfun (x, e) = Exp.Abs(x, None, e, get_next_exp_id())
let mkapp (e1, e2) = Exp.App(e1, e2, get_next_exp_id())
let mkvar x = Exp.Var(x, get_next_exp_id())
let mknum n = Exp.Num(n, get_next_exp_id())


let test_unif_scaffold () =
  assert_bool "Int and arrow types equal"
    (not (typs_equal Int (Arrow("x", Int, Int))))


let test_unif_scaffold_generic () =
  let t1 = GenTy(["a"], Arrow("x", TyVar "a", TyVar "a")) in
  let t2 = GenTy(["b"], Arrow("x", TyVar "b", TyVar "b")) in
    assert_bool "Generic types not equal after renaming" (typs_equal t1 t2)


let test_unif_scaffold_generic_neq () =
  let t1 = GenTy(["a"; "c"], Arrow("x", TyVar "a", TyVar "c")) in
  let t2 = GenTy(["b"], Arrow("x", TyVar "b", TyVar "b")) in
    assert_bool "Unequal generic types equal after renaming" (not (typs_equal t1 t2))


let test_int_inference () =
  let e = Exp.Num(2, get_next_exp_id()) in
    assert_bool "2 does not have int type" (typs_equal Int (infer_exp_typ e))


let test_id_inference () =
  let e = mklet("id", mkfun("x", mkvar "x"), mkvar "id") in
  let t = Arrow("x", TyVar "a", TyVar "a") in
    assert_bool "id function not 'a -> 'a" (typs_equal t (infer_exp_typ e))


let test_instanitation_type () =
  let einst = mkvar "id" in
  let e = mklet("id", mkfun("x", mkvar "x"), mkapp(einst, mknum 2)) in
  let smap = infer_shapes e in
    assert_bool "id instantation does not have type int -> int"
      (typs_equal (ExpMap.find einst smap) (Arrow("x", Int, Int)))


let suite = "Test ML Type Inference" >:::
  ["Test type equality scaffold" >:: test_unif_scaffold;
   "Test type equality with generic types" >:: test_unif_scaffold_generic;
   "Test type disequality with generic types" >:: test_unif_scaffold_generic_neq;
   "Test int type inference" >:: test_int_inference;
   "Test id fun type inference" >:: test_id_inference;
   "Test instantiation type" >:: test_instanitation_type;
  ]

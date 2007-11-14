open Longident
open Predicate
open Asttypes

let rec mk_longid = function
  | [] -> assert false
  | [id] -> Lident id
  | id :: idrem -> Ldot (mk_longid idrem, id)

let find_path id env =
  let (path, _) = Env.lookup_value (mk_longid id) env in path

let find_type_path id env =
  try
    let (path, _) = Env.lookup_type (mk_longid id) env in path
  with Not_found ->
    Printf.printf "Couldn't load %s!\n" (String.concat " " id);
    assert false

let qfalse = (Path.mk_ident "FALSE", Path.mk_ident "x",
              Predicate.Not Predicate.True)

let qsize rel x y z = (Path.mk_ident ("SIZE_" ^ (Predicate.pprint_rel rel)), y,
						Predicate.Atom(Predicate.Var z, rel, Predicate.FunApp("Array.length", Predicate.Var x)))

let samesize y =
  let x = Path.mk_ident "x" in
    (Path.mk_ident ("SAME_SIZE_AS_" ^ (Path.name y)),
     x,
     Predicate.Atom(Predicate.FunApp("Array.length", Predicate.Var x),
                    Predicate.Eq,
                    Predicate.FunApp("Array.length", Predicate.Var y)))

let qdim rel dim x y z =
  let dimstr = string_of_int dim in
    (Path.mk_ident ("DIM" ^ dimstr ^ (Predicate.pprint_rel rel)), y,
     Predicate.Atom(Predicate.Var z, rel,
                    Predicate.FunApp("Bigarray.Array2.dim" ^ dimstr, Predicate.Var x)))

let qint rel i y = (Path.mk_ident (Printf.sprintf "INT_%s%d" 
		(Predicate.pprint_rel rel) i), y, Predicate.Atom(Predicate.Var y, rel, Predicate.PInt i))

let qrel rel x y = (Path.mk_ident (Printf.sprintf "_%s%s%s_" 
                                      (Path.name x) (Predicate.pprint_rel rel) (Path.name y)), 
                                      x, Predicate.Atom(Predicate.Var x, rel, Predicate.Var y))

let qbool_rel qname rel (x, y, z) =
  let truepred = Predicate.Atom (Predicate.Var x, rel, Predicate.Var y) in
  (Path.mk_ident qname, z, 
              Predicate.Or (Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 1),
                                           truepred),
                            Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 0),
                                           Predicate.Not truepred))) 

let quals = [
  qfalse;
]

let mk_tyvar () = Frame.Fvar(Path.mk_ident "'a") 

let mk_tyvarb () = Frame.Fvar(Path.mk_ident "'b")

let mk_int qs = Frame.Fconstr(Predef.path_int, [], ([], Frame.Qconst qs))

let mk_float = Frame.Fconstr(Predef.path_float, [], ([], Frame.Qconst []))

let mk_bool qs = Frame.Fconstr(Predef.path_bool, [], ([], Frame.Qconst qs))

let mk_array f qs = Frame.Fconstr(Predef.path_array, [f], ([], Frame.Qconst qs))

let mk_named path fs qs env = Frame.Fconstr(find_type_path path env, fs, ([], Frame.Qconst qs))

let mk_ref f env = Frame.Frecord (find_type_path ["ref"; "Pervasives"] env, [(f, "contents", Mutable)], ([], Frame.Qconst []))

let mk_bigarray_kind a b qs env = mk_named ["kind"; "Bigarray"] [a; b] qs env

let mk_bigarray_layout a qs env = mk_named ["layout"; "Bigarray"] [a] qs env

let mk_bigarray_type a b c qs env = mk_named ["t"; "Array2"; "Bigarray"] [a; b; c] qs env

let mk_unit () = Frame.Fconstr(Predef.path_unit, [], ([], Frame.Qconst []))

let mk_fun (lab, f, f') = Frame.Farrow (Some lab, f, f')

let mk_int_equals x i = Predicate.equals(Predicate.Var x, Predicate.PInt i)

let fun_frame path (x, y) qual =
  (path, mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_int [qual])))

let rel_fun_frame path (x, y) (t1, t2) qual =
  (path, mk_fun (x, t1, mk_fun (y, t2, mk_bool [qual])))

let poly_rel_fun_frame path (x, y) tyvar qs =
  (path, mk_fun (x, tyvar, mk_fun (y, tyvar, mk_bool [qs])))

let fresh_idents () = (Path.mk_ident "x", Path.mk_ident "y", Path.mk_ident "z")
let fresh_2_idents () = (Path.mk_ident "x", Path.mk_ident "y")

let op_frame path qname op =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.mk_ident qname,
              z,
              Predicate.equals
                (Predicate.Var z,
                 Predicate.Binop (Predicate.Var x, op, Predicate.Var y))) in
    fun_frame path (x, y) qual

let uninterp_float_binop path =
  let (x, y, z) = fresh_idents () in
  (path, mk_fun(x, mk_float, mk_fun(y, mk_float, mk_float)))
let uninterp_int_binop path =
  let (x, y, z) = fresh_idents () in
  (path, mk_fun(x, mk_int [], mk_fun(y, mk_int [], mk_int [])))
let uninterp_float_unop path =
  let (x, y) = fresh_2_idents () in
  (path, mk_fun(x, mk_float, mk_float))
let float_to_int_unop path =
  let (x, y) = fresh_2_idents () in
  (path, mk_fun(x, mk_float, mk_int []))

let lsr_frame =
  let (x, y, z) = fresh_idents () in
    (["lsr"; "Pervasives"],
     mk_fun(x, mk_int [],
            mk_fun (y, mk_int [],
                    mk_int [qint Predicate.Ge 0 z])))

let land_frame =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.mk_ident "land",
              z,
              Predicate.implies
                (Predicate.And (Predicate.Atom (Predicate.Var x, Predicate.Ge, Predicate.PInt 0),
                                Predicate.Atom (Predicate.Var y, Predicate.Ge, Predicate.PInt 0)),
                 Predicate.big_and
                   [Predicate.Atom (Predicate.PInt 0, Predicate.Le, Predicate.Var z);
                    Predicate.Atom (Predicate.Var z, Predicate.Le, Predicate.Var x);
                    Predicate.Atom (Predicate.Var z, Predicate.Le, Predicate.Var y);])) in
    (["land"; "Pervasives"],
     mk_fun(x, mk_int [],
            mk_fun (y, mk_int [],
                    mk_int [qual])))

let mod_frame =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.mk_ident "mod",
              z,
              Predicate.implies
                (Predicate.And (Predicate.Atom (Predicate.Var x, Predicate.Ge, Predicate.PInt 0),
                                Predicate.Atom (Predicate.Var y, Predicate.Ge, Predicate.PInt 0)),
                 Predicate.big_and
                   [Predicate.Atom (Predicate.PInt 0, Predicate.Le, Predicate.Var z);
                    Predicate.Atom (Predicate.Var z, Predicate.Lt, Predicate.Var y);])) in
    (["mod"; "Pervasives"],
     mk_fun(x, mk_int [],
            mk_fun (y, mk_int [],
                    mk_int [qual])))

(* ming: connectives could use cleanup ... eventually *)

let bool_conj_frame path qname =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.mk_ident qname,
                z,
                Predicate.Or( Predicate.And(mk_int_equals z 1, 
                              Predicate.And(mk_int_equals x 1, mk_int_equals y 1)),
                              Predicate.And(mk_int_equals z 0,
                              Predicate.Or(mk_int_equals x 0, mk_int_equals y 0)) )) in
    rel_fun_frame path (x, y) (mk_bool [], mk_bool []) qual
  
let bool_disj_frame path qname =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.mk_ident qname,
                z,
                Predicate.Or( Predicate.And(mk_int_equals z 1,
                              Predicate.Or(mk_int_equals x 1, mk_int_equals y 1)),
                              Predicate.And(mk_int_equals z 0,
                              Predicate.And(mk_int_equals x 0, mk_int_equals y 0)) )) in
    rel_fun_frame path (x, y) (mk_bool [], mk_bool []) qual

let bool_not_frame =
  let (x, y) = fresh_2_idents () in
  let qual = (Path.mk_ident "NOT",
              y,
              Predicate.And( Predicate.Or(mk_int_equals y 1,
                                         mk_int_equals x 1),
                             Predicate.Or(mk_int_equals y 0,
                                         mk_int_equals x 0))) in
    (["not"; "Pervasives"], mk_fun (x, mk_bool [], mk_bool [qual]))

    
let ignore_frame =
  let x = Path.mk_ident "x" in
    (["ignore"; "Pervasives"], mk_fun (x, mk_tyvar (), mk_unit ())) 

let rel_frame path qname rel =
  let (x, y, z) = fresh_idents () in
    rel_fun_frame path (x, y) (mk_int [], mk_int []) (qbool_rel qname rel (x, y, z))

let poly_rel_frame path qname rel =
  let (x, y, z) = fresh_idents () in
  let tyvar = mk_tyvar () in
    poly_rel_fun_frame path (x, y) tyvar (qbool_rel qname rel (x, y, z))


let array_length_frame = 
	let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in (* make 2 idents fresh *)
	let tyvar = mk_tyvar () in
	(["length"; "Array"], mk_fun (x, (mk_array tyvar []), mk_int [qsize Predicate.Eq x y y; qint Predicate.Gt 0 y]))

let array_set_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = mk_tyvar () in
  (["set"; "Array"], mk_fun(x, (mk_array tyvar []),
                mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y],
                mk_fun(z, tyvar, mk_unit ()))))

let array_get_frame =
	let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
	let tyvar = mk_tyvar () in
	(["get"; "Array"], mk_fun(x, (mk_array tyvar []), 
				  mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y], (tyvar))))

let array_make_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = mk_tyvar () in
  (["make"; "Array"], mk_fun(x, mk_int [],
                 mk_fun(y, tyvar, mk_array tyvar [qsize Predicate.Eq z z x])))

let array_copy_frame =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  (["copy"; "Array"], mk_fun(x, mk_array tyvar [],
                             mk_array tyvar [samesize x]))

let array_init_frame =
  let (x, y, z) = fresh_idents () in
  let i = Path.mk_ident "i" in
  let tyvar = mk_tyvar () in
  let init = mk_fun (y, mk_int [qint Predicate.Ge 0 y; qrel Predicate.Lt y x], tyvar) in
    (["init"; "Array"], mk_fun(x, mk_int [qint Predicate.Gt 0 x],
                               mk_fun(i, init, mk_array tyvar [qsize Predicate.Eq z z x])))

let bigarray_create_frame env =
  let (k, l) = fresh_2_idents () in
  let (a, b, c) = (mk_tyvar (), mk_tyvar (), mk_tyvar ()) in
  let (dim1, dim2, z) = fresh_idents () in
  (["create"; "Array2"; "Bigarray"],
   mk_fun(k, mk_bigarray_kind a b [] env,
          (mk_fun(l, mk_bigarray_layout c [] env,
          (mk_fun(dim1, mk_int [qint Predicate.Gt 0 dim1],
          (mk_fun(dim2, mk_int [qint Predicate.Gt 0 dim2],
                  mk_bigarray_type a b c
                    [qdim Predicate.Eq 1 z z dim1;
                     qdim Predicate.Eq 2 z z dim2]
                    env))))))))

let bigarray_int_frame env =
  (["int"; "Bigarray"],
   mk_named ["kind"; "Bigarray"] [mk_int []; mk_named ["int_elt"; "Bigarray"] [] [] env] [] env)

let bigarray_float64_frame env =
  (["float64"; "Bigarray"],
   mk_named ["kind"; "Bigarray"] [mk_int []; mk_named ["float64_elt"; "Bigarray"] [] [] env] [] env)

let bigarray_c_layout_frame env =
  (["c_layout"; "Bigarray"],
   mk_named ["layout"; "Bigarray"] [mk_named ["c_layout"; "Bigarray"] [] [] env] [] env)

let bigarray_get_frame env =
  let ty = mk_tyvar () in
  let (a, i, j) = fresh_idents () in
    (["get"; "Array2"; "Bigarray"],
     mk_fun(a, mk_bigarray_type ty (mk_tyvar ()) (mk_tyvar ()) [] env,
            mk_fun(i,
                   mk_int [qint Predicate.Ge 0 i;
                           qdim Predicate.Lt 1 a i i],
            mk_fun(j,
                   mk_int [qint Predicate.Ge 0 j;
                           qdim Predicate.Lt 2 a j j],
                   ty))))

let bigarray_set_frame env =
  let ty = mk_tyvar () in
  let (a, v) = fresh_2_idents () in
  let (i, j) = fresh_2_idents () in
    (["set"; "Array2"; "Bigarray"],
     mk_fun(a, mk_bigarray_type ty (mk_tyvar ()) (mk_tyvar ()) [] env,
            mk_fun(i,
                   mk_int [qint Predicate.Ge 0 i;
                           qdim Predicate.Lt 1 a i i],
            mk_fun(j,
                   mk_int [qint Predicate.Ge 0 j;
                           qdim Predicate.Lt 2 a j j],
            mk_fun(v, ty, mk_unit ())))))

let bigarray_dim_frame dim env =
  let ty = mk_tyvar () in
  let (a, s) = fresh_2_idents () in
    (["dim" ^ string_of_int dim; "Array2"; "Bigarray"],
     mk_fun(a, mk_bigarray_type ty (mk_tyvar ()) (mk_tyvar ()) [] env,
            mk_int [qdim Predicate.Eq dim a s s; qint Predicate.Gt 0 s]))

let rand_init_frame =
  let x = Path.mk_ident "x" in
  (["init"; "Random"], mk_fun(x, mk_int [], mk_unit ()))

let void_fun_frame name =
  let x = Path.mk_ident "x" in
  (name, mk_fun(x, mk_unit (), mk_unit ()))

let rand_int_frame =
  let (x, y) = fresh_2_idents () in
  (["int"; "Random"], mk_fun(x, mk_int [qint Predicate.Gt 0 x], 
                                mk_int [qint Predicate.Ge 0 y; qrel Predicate.Lt y x]))

let ref_ref_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  (["ref"; "Pervasives"], mk_fun(x, tyvar, mk_ref tyvar env))

let ref_deref_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  (["!"; "Pervasives"], mk_fun(x, mk_ref tyvar env, tyvar))

let ref_assgn_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  ([":="; "Pervasives"], mk_fun(x, mk_ref tyvar env,
                    mk_fun(y, tyvar, mk_unit ())))

let tuple_fst_snd_frame name fst =
  let x = Path.mk_ident "x" in
  let tyvara = mk_tyvar () in
  let tyvarb = mk_tyvarb () in
    (name, mk_fun(x, Frame.Ftuple [tyvara; tyvarb],
                  if fst then tyvara else tyvarb))

let ref_path env =
  ("ref", find_type_path ["ref"; "Pervasives"] env)

let bigarray_array2_path env =
  ("array2", find_type_path ["t"; "Array2"; "Bigarray"] env)

let max_int_frame = (["max_int"; "Pervasives"], mk_int [])

let _frames = [
  op_frame ["+"; "Pervasives"] "+" Predicate.Plus;
  op_frame ["-"; "Pervasives"] "-" Predicate.Minus;
  op_frame ["/"; "Pervasives"] "/" Predicate.Div;
  op_frame ["*"; "Pervasives"] "*" Predicate.Times;
  uninterp_float_binop ["+."; "Pervasives"];
  uninterp_float_binop ["-."; "Pervasives"];
  uninterp_float_binop ["/."; "Pervasives"];
  uninterp_float_binop ["*."; "Pervasives"];
  uninterp_float_unop ["sin"; "Pervasives"];
  uninterp_float_unop ["cos"; "Pervasives"];
  uninterp_float_unop ["~-."; "Pervasives"];
  float_to_int_unop ["float_of_int"; "Pervasives"];
  uninterp_int_binop ["lor"; "Pervasives"];
  land_frame;
  uninterp_int_binop ["lxor"; "Pervasives"];
  lsr_frame;
  uninterp_int_binop ["lsl"; "Pervasives"];
  (*rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  rel_frame [">"; "Pervasives"] ">" Predicate.Gt;
  rel_frame ["<="; "Pervasives"] ">=" Predicate.Ge;*)
  poly_rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  poly_rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  poly_rel_frame ["<>"; "Pervasives"] "<>" Predicate.Ne;
  poly_rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  poly_rel_frame [">"; "Pervasives"] ">" Predicate.Gt;
  poly_rel_frame [">="; "Pervasives"] ">=" Predicate.Ge;
  poly_rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  tuple_fst_snd_frame ["fst"; "Pervasives"] true;
  tuple_fst_snd_frame ["snd"; "Pervasives"] false;
  bool_conj_frame ["&&"; "Pervasives"] "&&";
  bool_disj_frame ["||"; "Pervasives"] "||";
  bool_not_frame;
  array_length_frame;
  array_copy_frame;
  array_get_frame;
  array_make_frame;
  array_init_frame;
  array_set_frame;
  rand_init_frame;
  rand_int_frame;
  void_fun_frame ["self_init"; "Random"];
  ignore_frame;
  max_int_frame;
  mod_frame;
]

let _lib_frames = [
  ref_ref_frame;
  ref_deref_frame;
  ref_assgn_frame;
  bigarray_create_frame;
  bigarray_get_frame;
  bigarray_set_frame;
  bigarray_dim_frame 1;
  bigarray_dim_frame 2;
  bigarray_int_frame;
  bigarray_float64_frame;
  bigarray_c_layout_frame;
]

let _type_path_constrs = [
  ref_path;
  bigarray_array2_path;
]

let _type_paths = ref None

let ext_find_type_path t =
  (fun (a, b) -> b) (List.find (fun (a, _) -> (a = t)) 
                      (match !_type_paths with None -> assert false
                                               | Some b -> b))

let frames env =
  let _ = _type_paths := Some (List.map (fun x -> x env) _type_path_constrs) in
  let resolve_types x = List.map (fun fr -> fr env) x in
  let resolve_names x = List.map (fun (id, fr) -> (find_path id env, fr)) x in
  List.append (resolve_names  _frames) (resolve_names (resolve_types _lib_frames))

let equality_refinement exp =
  let x = Path.mk_ident "x" in
    ([], Frame.Qconst [(Path.mk_ident "<eq>",
                        x,
                        Predicate.equals (Predicate.Var x, exp))])

let size_lit_refinement i =
	let x = Path.mk_ident "x" in
	  ([], Frame.Qconst [(Path.mk_ident "<size_lit_eq>",
			      x,
			      Predicate.equals(Predicate.FunApp("Array.length", Predicate.Var x), Predicate.PInt(i)))])

let field_eq_qualifier name pexp =
  let x = Path.mk_ident "x" in
    (Path.mk_ident "<field_eq>",
     x,
     Predicate.equals (Predicate.Field (name, Predicate.Var x), pexp))

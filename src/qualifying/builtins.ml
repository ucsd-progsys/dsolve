open Longident
open Predicate

let rec mk_longid = function
  | [] -> assert false
  | [id] -> Lident id
  | id :: idrem -> Ldot (mk_longid idrem, id)

let find_path id env =
  let (path, _) = Env.lookup_value (mk_longid id) env in path

let find_type_path id env =
  let (path, _) = Env.lookup_type (mk_longid id) env in path 

let qfalse = (Path.mk_ident "FALSE", Path.mk_ident "x",
              Predicate.Not Predicate.True)

let qsize rel x y z = (Path.mk_ident ("SIZE_" ^ (Predicate.pprint_rel rel)), y,
						Predicate.Atom(Predicate.Var z, rel, Predicate.FunApp("Array.length", Predicate.Var x)))

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

let mk_bool qs = Frame.Fconstr(Predef.path_bool, [], ([], Frame.Qconst qs))

let mk_array f qs = Frame.Fconstr(Predef.path_array, [f], ([], Frame.Qconst qs))

let mk_ref f qs env = Frame.Fconstr(find_type_path ["ref"; "Pervasives"] env, [f], ([], Frame.Qconst qs))

let mk_unit () = Frame.Fconstr(Predef.path_unit, [], ([], Frame.Qconst []))

let mk_fun (lab, f, f') = Frame.Farrow (Some lab, f, f')

let fun_frame path (x, y) qual =
  (path, mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_int [qual])))

let rel_fun_frame path (x, y) qual =
  (path, mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_bool [qual])))

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

let rel_frame path qname rel =
  let (x, y, z) = fresh_idents () in
    rel_fun_frame path (x, y) (qbool_rel qname rel (x, y, z))

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
  (["make"; "Array"], mk_fun(x, mk_int [qint Predicate.Gt 0 x],
                 mk_fun(y, tyvar, mk_array tyvar [qsize Predicate.Eq z z x])))

let rand_init_frame =
  let x = Path.mk_ident "x" in
  (["init"; "Random"], mk_fun(x, mk_int [], mk_unit ()))

let rand_int_frame =
  let (x, y) = fresh_2_idents () in
  (["int"; "Random"], mk_fun(x, mk_int [qint Predicate.Gt 0 x], 
                                mk_int [qint Predicate.Ge 0 y; qrel Predicate.Lt y x]))

let ref_ref_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  (["ref"; "Pervasives"], mk_fun(x, tyvar, mk_ref tyvar [] env))

let ref_deref_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  (["!"; "Pervasives"], mk_fun(x, mk_ref tyvar [] env, tyvar))

let ref_assgn_frame env =
  let (x, y) = fresh_2_idents () in
  let tyvar = mk_tyvar () in
  ([":="; "Pervasives"], mk_fun(x, mk_ref tyvar [] env,
                    mk_fun(y, tyvar, mk_unit ())))

let tuple_fst_snd_frame name fst =
  let x = Path.mk_ident "x" in
  let tyvara = mk_tyvar () in
  let tyvarb = mk_tyvarb () in
  (name, mk_fun(x, Frame.Ftuple(tyvara, tyvarb),  
                                if fst then tyvara else tyvarb))

let ref_path env =
  ("ref", find_type_path ["ref"; "Pervasives"] env)


let _frames = [
  op_frame ["+"; "Pervasives"] "+" Predicate.Plus;
  op_frame ["-"; "Pervasives"] "-" Predicate.Minus;
  op_frame ["/"; "Pervasives"] "/" Predicate.Div;
  op_frame ["*"; "Pervasives"] "*" Predicate.Times;
  (*rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  rel_frame [">"; "Pervasives"] ">" Predicate.Gt;
  rel_frame ["<="; "Pervasives"] ">=" Predicate.Ge;*)
  poly_rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  poly_rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  poly_rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  poly_rel_frame [">"; "Pervasives"] ">" Predicate.Gt;
  poly_rel_frame [">="; "Pervasives"] ">=" Predicate.Ge;
  poly_rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  tuple_fst_snd_frame ["fst"; "Pervasives"] true;
  tuple_fst_snd_frame ["snd"; "Pervasives"] false;
  array_length_frame;
  array_get_frame;
  array_make_frame;
  array_set_frame;
  rand_init_frame;
  rand_int_frame;
]

let _lib_frames = [
  ref_ref_frame;
  ref_deref_frame;
  ref_assgn_frame;
]

let _type_path_constrs = [
  ref_path;
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

  

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

let qint rel i y = (Path.mk_ident (Printf.sprintf "SIZE_%s%d" 
		(Predicate.pprint_rel rel) i), y, Predicate.Atom(Predicate.Var y, rel, Predicate.PInt i))

let quals = [
  qfalse;
]

let mk_int qs = Frame.Fconstr(Predef.path_int, [], ([], Frame.Qconst qs))

let mk_array f qs = Frame.Fconstr(Predef.path_array, [f], ([], Frame.Qconst qs))

let mk_ref f qs env = Frame.Fconstr(find_type_path ["ref"; "Pervasives"] env, [f], ([], Frame.Qconst qs))

let mk_unit () = Frame.Fconstr(Predef.path_unit, [], ([], Frame.Qconst []))

let mk_fun (lab, f, f') = Frame.Farrow (Some lab, f, f')

let fun_frame path (x, y) qual =
  (path, mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_int [qual])))

let fresh_idents () = (Path.mk_ident "x", Path.mk_ident "y", Path.mk_ident "z")

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
  let truepred = Predicate.Atom (Predicate.Var x, rel, Predicate.Var y) in
  let qual = (Path.mk_ident qname,
              z,
              Predicate.Or (Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 1),
                                           truepred),
                            Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 0),
                                           Predicate.Not truepred))) in
    fun_frame path (x, y) qual


let array_length_frame = 
	let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in (* make 2 idents fresh *)
	let tyvar = Frame.Fvar(Path.mk_ident "'a") in
	(["length"; "Array"], mk_fun (x, (mk_array tyvar []), mk_int [qsize Predicate.Eq x y y; qint Predicate.Gt 0 y]))

let array_set_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = Frame.Fvar(Path.mk_ident "'a") in
  (["set"; "Array"], mk_fun(x, (mk_array tyvar []),
                mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y],
                mk_fun(z, tyvar, mk_unit ()))))

let array_get_frame =
	let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
	let tyvar = Frame.Fvar(Path.mk_ident "'a") in
	(["get"; "Array"], mk_fun(x, (mk_array tyvar []), 
								mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y], (tyvar)))) 

let array_make_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = Frame.Fvar(Path.mk_ident "'a") in
  (["make"; "Array"], mk_fun(x, mk_int [qint Predicate.Gt 0 x],
                 mk_fun(y, tyvar, mk_array tyvar [qsize Predicate.Eq z z x])))

let ref_ref_frame env =
  let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
  let tyvar = Frame.Fvar(Path.mk_ident "'a") in
  (["ref"; "Pervasives"], mk_fun(x, tyvar, mk_ref tyvar [] env))

let ref_deref_frame env =
  let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
  let tyvar = Frame.Fvar(Path.mk_ident "'a") in
  (["!"; "Pervasives"], mk_fun(x, mk_ref tyvar [] env, tyvar))

let ref_assgn_frame env =
  let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
  let tyvar = Frame.Fvar(Path.mk_ident "'a") in
  ([":="; "Pervasives"], mk_fun(x, mk_ref tyvar [] env,
                    mk_fun(y, tyvar, mk_unit ())))

let ref_path env =
  ("ref", find_type_path ["ref"; "Pervasives"] env)

let _frames = [
  op_frame ["+"; "Pervasives"] "+" Predicate.Plus;
  op_frame ["-"; "Pervasives"] "-" Predicate.Minus;
  op_frame ["/"; "Pervasives"] "/" Predicate.Div;
  op_frame ["*"; "Pervasives"] "*" Predicate.Times;
  rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  array_length_frame;
  array_get_frame;
  array_make_frame;
  array_set_frame;
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
  List.append (List.map (fun (id, fr) -> (find_path id env, fr)) _frames)
              (List.map (fun (id, fr) -> (find_path id env, fr)) 
                (List.map (fun (fr) -> (fr env)) _lib_frames))

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

  

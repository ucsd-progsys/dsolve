open Longident
open Predicate

(* element one of the great array hack: 
let arr_length = Path.Pident(Ident.create "Array.length")
let arr_get = Path.Pident(Ident.create "Array.get")*)

let qfalse = (Path.Pident(Ident.create "FALSE"), Ident.create "x",
              Predicate.Not Predicate.True)

(* element two of the great array hack: *)
let qsize rel x y = (Path.Pident(Ident.create ("SIZE_" ^ (Predicate.pprint_rel rel))), y,
							Predicate.Atom(Predicate.Var y, rel, Predicate.FunApp("Array.length", Predicate.Var x)))
let qint rel i y = (Path.Pident(Ident.create (Printf.sprintf "SIZE_%s%d" 
		(Predicate.pprint_rel rel) i)), y, Predicate.Atom(Predicate.Var y, rel, Predicate.PInt i))

let quals = [
  qfalse;
]

let mk_int qs = Frame.Fconstr (Predef.path_int, [], ([], Frame.Qconst qs))

let mk_array f qs = Frame.Fconstr(Predef.path_array, [f], ([], Frame.Qconst qs))

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


(* element 3 of the great array hack: *make sure the generalization works..* *)
let array_length_frame = 
	let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in (* make 2 idents fresh *)
	let tyvar = Path.mk_ident "'a" in
	(["length"; "Array"], mk_fun (x, (mk_array (Frame.Fvar(tyvar)) []), mk_int [qsize Predicate.Eq x y; qint Predicate.Gt 0 y]))

let array_get_frame =
  let (x, y) = (Path.mk_ident "x", Path.mk_ident "y") in
  let tyvar = Path.mk_ident "'a" in
    (["get"; "Array"], mk_fun(x, (mk_array (Frame.Fvar(tyvar)) []),
			      mk_fun (y, mk_int [qsize Predicate.Lt x y; qint Predicate.Ge 0 y], (Frame.Fvar(tyvar))))) 

let _frames = [
  op_frame ["+"; "Pervasives"] "+" Predicate.Plus;
  op_frame ["-"; "Pervasives"] "-" Predicate.Minus;
  rel_frame ["="; "Pervasives"] "=" Predicate.Eq;
  rel_frame ["!="; "Pervasives"] "!=" Predicate.Ne;
  rel_frame ["<"; "Pervasives"] "<" Predicate.Lt;
  rel_frame ["<="; "Pervasives"] "<=" Predicate.Le;
  array_length_frame;
  array_get_frame;
]

let find_path id env =
  let rec mk_lid = function
    | [] -> assert false
    | [id] -> Lident id
    | id :: idrem -> Ldot (mk_lid idrem, id)
  in
  let (path, _) = Env.lookup_value (mk_lid id) env in path

let frames env =
  List.map (fun (id, fr) -> (find_path id env, fr)) _frames

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

let empty_refinement = ([], Frame.Qconst [])

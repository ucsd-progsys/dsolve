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

let fun_frame name (x, y) qual =
  ("Pervasives." ^ name,
   mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_int [qual])))

let fresh_idents () = (Ident.create "x", Ident.create "y", Ident.create "z")

let op_frame name op =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.Pident (Ident.create name),
              z,
              Predicate.equals
                (Predicate.Var z,
                 Predicate.Binop (Predicate.Var x, op, Predicate.Var y))) in
    fun_frame name (x, y) qual

let rel_frame name rel =
  let (x, y, z) = fresh_idents () in
  let truepred = Predicate.Atom (Predicate.Var x, rel, Predicate.Var y) in
  let qual = (Path.Pident (Ident.create name),
              z,
              Predicate.Or (Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 1),
                                           truepred),
                            Predicate.And (Predicate.equals (Predicate.Var z, Predicate.PInt 0),
                                           Predicate.Not truepred))) in
    fun_frame name (x, y) qual


(* element 3 of the great array hack: *make sure the generalization works..* *)
let array_length_frame = 
	let (x, y) = (Ident.create "x", Ident.create "y") in (* make 2 idents fresh *)
	let tyvar = Ident.create "'a" in
	("Array.length", mk_fun (x, (mk_array (Frame.Fvar(tyvar)) []), mk_int [qsize Predicate.Eq x y; qint Predicate.Gt 0 y]))

let array_get_frame =
	let (x, y) = (Ident.create "x", Ident.create "y") in
	let tyvar = Ident.create "'a" in
	("Array.get", mk_fun(x, (mk_array (Frame.Fvar(tyvar)) []), 
								mk_fun (y, mk_int [qsize Predicate.Lt x y; qint Predicate.Ge 0 y], (Frame.Fvar(tyvar))))) 


let frames = [
  op_frame "+" Predicate.Plus;
  op_frame "-" Predicate.Minus;
	op_frame "/" Predicate.Div;
  rel_frame "=" Predicate.Eq;
  rel_frame "!=" Predicate.Ne;
  rel_frame "<" Predicate.Lt;
  rel_frame "<=" Predicate.Le;
	array_length_frame;
	array_get_frame;
]

let equality_refinement exp =
  let x = Ident.create "x" in
    ([], Frame.Qconst [(Path.Pident (Ident.create "<eq>"),
                        x,
                        Predicate.equals (Predicate.Var x, exp))])

let size_lit_refinement i =
	let x = Ident.create "x" in
		([], Frame.Qconst [(Path.Pident(Ident.create "<size_lit_eq>"),
											 x,
											 Predicate.equals(Predicate.FunApp("Array.length", Predicate.Var x), Predicate.PInt(i)))])


let empty_refinement = ([], Frame.Qconst [])

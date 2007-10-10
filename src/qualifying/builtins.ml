open Predicate

let qfalse = (Path.Pident(Ident.create "FALSE"), Ident.create "x",
              Predicate.Not Predicate.True)

let qsize rel x y z = (Path.Pident(Ident.create ("SIZE_" ^ (Predicate.pprint_rel rel))), y,
							Predicate.Atom(Predicate.Var z, rel, Predicate.FunApp("Array.length", Predicate.Var x)))
let qint rel i y = (Path.Pident(Ident.create (Printf.sprintf "SIZE_%s%d" 
		(Predicate.pprint_rel rel) i)), y, Predicate.Atom(Predicate.Var y, rel, Predicate.PInt i))

let quals = [
  qfalse;
]

let mk_int qs = Frame.Fconstr (Predef.path_int, [], ([], Frame.Qconst qs))

let mk_array f qs = Frame.Fconstr(Predef.path_array, [f], ([], Frame.Qconst qs))

(* must find path for refs.. *)
let mk_ref f qs = Frame.Fconstr(Predef.path_int, [f], ([], Frame.Qconst qs))

let mk_unit () = Frame.Fconstr(Predef.path_unit, [], ([], Frame.Qconst []))

let mk_fun (lab, f, f') = Frame.Farrow (Some lab, f, f')

let mk_rel_qual s rel x y z = (Path.Pident (Ident.create s), x, 
          Predicate.Atom(Predicate.Var y, rel, Predicate.Var z))

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


let array_length_frame = 
	let (x, y) = (Ident.create "x", Ident.create "y") in (* make 2 idents fresh *)
	let tyvar = Ident.create "'a" in
	("Array.length", mk_fun (x, (mk_array (Frame.Fvar(tyvar)) []), mk_int [qsize Predicate.Eq x y y; qint Predicate.Gt 0 y]))

let array_set_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = Frame.Fvar(Ident.create "'a") in
  ("Array.set", mk_fun(x, (mk_array tyvar []),
                mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y],
                mk_fun(z, tyvar, mk_unit ()))))

let array_get_frame =
	let (x, y) = (Ident.create "x", Ident.create "y") in
	let tyvar = Frame.Fvar(Ident.create "'a") in
	("Array.get", mk_fun(x, (mk_array tyvar []), 
								mk_fun(y, mk_int [qsize Predicate.Lt x y y; qint Predicate.Ge 0 y], (tyvar)))) 

let array_make_frame =
  let (x, y, z) = fresh_idents () in
  let tyvar = Frame.Fvar(Ident.create "'a") in
  ("Array.make", mk_fun(x, mk_int [qint Predicate.Gt 0 x],
                 mk_fun(y, tyvar, mk_array tyvar [qsize Predicate.Eq z z x])))

let ref_ref_frame =
  let (x, y) = (Ident.create "x", Ident.create "y") in
  let tyvar = Frame.Fvar(Ident.create "'a") in
  ("Pervasives.ref", mk_fun(x, tyvar, mk_ref tyvar []))

let ref_deref_frame =
  let (x, y) = (Ident.create "x", Ident.create "y") in
  let tyvar = Frame.Fvar(Ident.create "'a") in
  ("Pervasives.!", mk_fun(x, mk_ref tyvar [], tyvar))

let ref_assgn_frame =
  let (x, y) = (Ident.create "x", Ident.create "y") in
  let tyvar = Frame.Fvar(Ident.create "'a") in
  ("Pervasives.:=", mk_fun(x, mk_ref tyvar [],
                    mk_fun(y, tyvar, mk_unit ())))

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
  array_make_frame;
  array_set_frame;
  ref_ref_frame;
  ref_deref_frame;
  ref_assgn_frame;
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

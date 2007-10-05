open Frame
open Predicate

let qfalse = (Path.Pident (Ident.create "FALSE"), Ident.create "x", Not True)

let quals = [
  qfalse
]

let mk_int qs = Fconstr (Predef.path_int, [], ([], Qconst qs))

let mk_fun (lab, f, f') = Farrow (Some lab, f, f')

let fun_frame name (x, y) qual =
  ("Pervasives." ^ name,
   mk_fun (x, mk_int [], mk_fun (y, mk_int [], mk_int [qual])))

let fresh_idents () = (Ident.create "x", Ident.create "y", Ident.create "z")

let op_frame name op =
  let (x, y, z) = fresh_idents () in
  let qual = (Path.Pident (Ident.create name),
              z, equals (Var z, Binop (Var x, op, Var y))) in
    fun_frame name (x, y) qual

let rel_frame name rel =
  let (x, y, z) = fresh_idents () in
  let truepred = Atom (Var x, rel, Var y) in
  let qual = (Path.Pident (Ident.create name),
              z,
              Or (And (equals (Var z, PInt 1), truepred),
                  And (equals (Var z, PInt 0), Not truepred))) in
    fun_frame name (x, y) qual

let frames = [
  op_frame "+" Plus;
  op_frame "-" Minus;
  rel_frame "=" Eq;
  rel_frame "!=" Ne;
  rel_frame "<" Lt;
  rel_frame "<=" Le;
]

let equality_refinement exp =
  let x = Ident.create "x" in
    ([], Qconst [(Path.Pident (Ident.create "<eq>"), x, equals (Var x, exp))])

let empty_refinement = ([], Qconst [])

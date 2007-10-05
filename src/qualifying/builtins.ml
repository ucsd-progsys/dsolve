let qfalse = (Path.Pident (Ident.create "FALSE"), Ident.create "x",
              Predicate.Not Predicate.True)

let quals = [
  qfalse
]

let mk_int qs = Frame.Fconstr (Predef.path_int, [], ([], Frame.Qconst qs))

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

let frames = [
  op_frame "+" Predicate.Plus;
  op_frame "-" Predicate.Minus;
  rel_frame "=" Predicate.Eq;
  rel_frame "!=" Predicate.Ne;
  rel_frame "<" Predicate.Lt;
  rel_frame "<=" Predicate.Le;
]

let equality_refinement exp =
  let x = Ident.create "x" in
    ([], Frame.Qconst [(Path.Pident (Ident.create "<eq>"),
                        x,
                        Predicate.equals (Predicate.Var x, exp))])

let empty_refinement = ([], Frame.Qconst [])

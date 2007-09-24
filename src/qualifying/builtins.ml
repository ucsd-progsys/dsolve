open Frame
open Predicate

let xv = Ident.create "_X"
let yv = Ident.create "_Y"
let zv = Ident.create "_Z"
(*

let op_shape name =
  (name, Arrow(xv, Base(Int), Arrow(yv, Base(Int), Base(Int))))
let rel_shape name =
  (name, Arrow(xv, Base(Int), Arrow(yv, Base(Int), Base(Bool))))

let _types = [
  op_shape "+";
  op_shape "-";
  rel_shape "=";
  rel_shape "!=";
  rel_shape "<";
  rel_shape "<=";
]

let types = Env.addn _types Env.empty


let binop_qual name op =
  (name, PredOver(zv, equals(Var zv, Binop(Var xv, op, Var yv))))


let binrel_qual name rel =
  let truepred = Atom(Var xv, rel, Var yv) in
    (name, PredOver(zv, Or(And(equals(Var zv, PInt 1), truepred),
                           And(equals(Var zv, PInt 0), Not(truepred)))))


let qplus = binop_qual "PLUS" Plus
let qminus = binop_qual "MINUS" Minus
let qequal = binrel_qual "EQUAL" Eq
let qnequal = binrel_qual "NEQUAL" Ne
let qless = binrel_qual "LESS" Lt
let qlesseq = binrel_qual "LESSEQ" Le
let qfalse = ("FALSE", PredOver("x", Not(True)))

let _quals = [
  qplus;
  qminus;
  qequal;
  qnequal;
  qless;
  qlesseq;
  qfalse
]

let quals = Env.addn _quals Env.empty


let op_frame op qual =
  (op, FArrow(xv, FBase(Int, ([], RQuals [])),
              FArrow(yv, FBase(Int, ([], RQuals [])), FBase(Int, ([], RQuals [qual])))))
let rel_frame op qual =
  (op, FArrow(xv, FBase(Int, ([], RQuals [])),
              FArrow(yv, FBase(Int, ([], RQuals [])), FBase(Bool, ([], RQuals [qual])))))

let _frames = [
  op_frame "+" qplus;
  op_frame "-" qminus;
  rel_frame "=" qequal;
  rel_frame "!=" qnequal;
  rel_frame "<" qless;
  rel_frame "<=" qlesseq;
]

let frames = Env.addn _frames Env.empty

  *)
let equality_refinement exp =
  ([], Qconst [(Path.Pident(Ident.create "<constant>"), xv, equals(Var xv, exp))])

let true_refinement =
  ([], Qconst [(Path.Pident(Ident.create "true"), xv, True)])

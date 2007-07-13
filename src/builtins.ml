open Type
open Predicate
open Constraint


let xv = "_X"
let yv = "_Y"
let zv = "z"


let op_shape name =
  (name, Arrow(xv, Int [], Arrow(yv, Int[], Int [])))

let _types = [
  op_shape "+";
  op_shape "-";
  op_shape "=";
  op_shape "!=";
  op_shape "<";
  op_shape "<=";
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
  (op, FArrow(xv, FInt([], []), FArrow(yv, FInt([], []), FInt([], [qual]))))


let _frames = [
  op_frame "+" qplus;
  op_frame "-" qminus;
  op_frame "=" qequal;
  op_frame "!=" qnequal;
  op_frame "<" qless;
  op_frame "<=" qlesseq;
]

let frames = Env.addn _frames Env.empty


let equality_qualifier exp =
  ("__EQ" ^ pprint_expression exp, PredOver(xv, equals(Var xv, exp)))

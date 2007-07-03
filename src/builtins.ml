open Type
open Predicate
open Constraint


let op_shape name =
  (name, Arrow("x", Int [], Arrow("y", Int[], Int [])))

let types = [
  op_shape "+";
  op_shape "-";
  op_shape "=";
  op_shape "!=";
  op_shape "<";
  op_shape "<=";
]


let binop_qual name op =
  (name, PredOver("z", equals(Var "z", Binop(Var "x", op, Var "y"))))


let binrel_qual name rel =
  let truepred = Atom(Var "x", rel, Var "y") in
    (name, PredOver("z", Or(And(equals(Var "z", PInt 1), truepred),
                            And(equals(Var "z", PInt 0), Not(truepred)))))


let qplus = binop_qual "PLUS" Plus
let qminus = binop_qual "MINUS" Minus
let qequal = binrel_qual "EQUAL" Eq
let qnequal = binrel_qual "NEQUAL" Ne
let qless = binrel_qual "LESS" Lt
let qlesseq = binrel_qual "LESSEQ" Le

let quals = [
  qplus;
  qminus;
  qequal;
  qnequal;
  qless;
  qlesseq;
]


let op_frame op qual =
  (op, FArrow("x", FInt([], []), FArrow("y", FInt([], []), FInt([], [qual]))))


let frames = [
  op_frame "+" qplus;
  op_frame "-" qminus;
  op_frame "=" qequal;
  op_frame "!=" qnequal;
  op_frame "<" qless;
  op_frame "<=" qlesseq;
]


DSOLVEDIR=../../
FIXDIR=../fixpoint

dconstraint.cmxa:
	ocamlopt -c dconstraint.ml
	ocamlopt -I ${DSOLVEDIR}/liquid -a ${DSOLVEDIR}/liquid/constraint.cmxo dconstraint.cmxo 

fconstraint.cmxa:
	ocamlopt -c fconstraint.ml
	ocamlopt -I ${FIXDIR}/ -a ${FIXDIR}/constraint.cmxo fconstraint.cmxo

all: 

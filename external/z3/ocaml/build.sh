#!/bin/bash

export OCAMLLIB=$1

gcc -I../include -I$OCAMLLIB -c z3_stubs.c
ocamlopt -c z3.mli
ocamlopt -c z3.ml
#ar rcs libz3.a z3_stubs.o z3.o
ar rcs libz3stubs.a z3_stubs.o
#ranlib libz3.a
ranlib libz3stubs.a
ocamlopt -a -o z3.cmxa -cclib -lz3stubs z3.cmx

#ar rcs libz3.a z3.o z3_stubs.o
#ld -o /tmp/fat -L/usr/local/lib/ocaml -lcamlidl z3_stubs.o
#gcc -shared -o libz3.dll z3.o z3_stubs.o

#ocamlopt -a -o z3.cmxa -cclib -lz3 z3.cmx

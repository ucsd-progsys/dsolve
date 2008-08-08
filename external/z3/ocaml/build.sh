#!/bin/bash

#ocamlopt -ccopt "-I../include -I/usr/local/lib/ocaml" -cclib "-L../bin" -c z3_stubs.c z3.ml z3.mli
gcc -I../include -I/usr/local/lib/ocaml -c z3_stubs.c
ocamlopt -c z3.mli
ocamlopt -c z3.ml
ar rs libz3.a z3.o z3_stubs.o
ar rs libz3stubs.a z3_stubs.o
ocamlopt -a -o z3.cmxa -cclib -lz3 z3.cmx

#ar rcs libz3.a z3.o z3_stubs.o
#ld -o /tmp/fat -L/usr/local/lib/ocaml -lcamlidl z3_stubs.o
#gcc -shared -o libz3.dll z3.o z3_stubs.o

#ocamlopt -a -o z3.cmxa -cclib -lz3 z3.cmx

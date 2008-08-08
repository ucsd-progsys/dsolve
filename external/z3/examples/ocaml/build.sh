#!/bin/bash
 
#ocamlopt ../../ocaml/z3.o ../../ocaml/z3_stubs.o -ccopt "-I../../ocaml -L../../bin -L../../ocaml" -o test_mlapi.exe -I ../../ocaml /usr/local/lib/ocaml/libcamlidl.a z3.cmxa test_mlapi.ml
ocamlopt -o test_mlapi.exe -ccopt "-I../../ocaml -L../../bin" -I ../../ocaml/ -I /usr/local/lib/ocaml -cclib -lcamlidl -cclib -lz3stubs -cclib -lz3 /usr/local/lib/ocaml/libcamlidl.a z3.cmxa test_mlapi.ml

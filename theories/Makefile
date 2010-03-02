#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.88 2007/02/09 13:24:20 doligez Exp $

include ../config/Makefile

RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc
CAMLC=$(RUNTIME) $(COMPILER)
COMPFLAGS=-g -warn-error A -nostdlib
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error A -nostdlib -g
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

OBJS=hash2.cmi myhash.cmi mymap2.cmi myset.cmi store2.cmi \
deparray.cmi myaset.cmi mylist.cmi mymap.cmi mystore.cmi store.cmi

.PHONY: all
all: $(OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

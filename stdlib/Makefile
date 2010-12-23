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

RUNTIME=ocamlrun
COMPILER=../ocamlc
CAMLC=$(RUNTIME) $(COMPILER)
COMPFLAGS=-g -warn-error A -nostdlib
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error A -nostdlib -g
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

OBJS=pervasives.cmi $(OTHERS)
OTHERS=array.cmi list.cmi char.cmi string.cmi sys.cmi \
  hashtbl.cmi sort.cmi marshal.cmi obj.cmi \
  int32.cmi int64.cmi nativeint.cmi \
  lexing.cmi parsing.cmi \
  set.cmi map.cmi stack.cmi queue.cmi stream.cmi buffer.cmi \
  printf.cmi format.cmi scanf.cmi \
  arg.cmi printexc.cmi gc.cmi \
  digest.cmi random.cmi callback.cmi \
  camlinternalOO.cmi oo.cmi camlinternalMod.cmi \
  genlex.cmi weak.cmi \
  lazy.cmi filename.cmi complex.cmi \
  arrayLabels.cmi listLabels.cmi stringLabels.cmi moreLabels.cmi stdLabels.cmi

.PHONY: all
all: $(OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

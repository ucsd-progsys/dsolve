include config/Makefile

CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex
CAMLDEP=ocamldep
DEPFLAGS=$(INCLUDES)
COMPFLAGS=$(FLAGS) -g -dtypes -warn-error A $(INCLUDES)
LIBZ3=-cclib -lz3#-gmp
LIBFIX=external/fixpoint/_build #path to fixpoint objects
LIBMISC=external/misc/_build #path to misc objects

LINKFLAGS= -ccopt "-Iexternal/z3/ocaml -Lexternal/z3/lib" $(FLAGS) \
	  -I external/ocamlgraph/ \
	  -cclib -lstdc++ $(LIBZ3) -cclib -lz3stubs \
	  -I external/ocamlgraph/ -I external/z3/ocaml -I external/z3/bin \
					-I $(LIBFIX) -I $(LIBMISC)

INCLUDES=-I external/z3/ocaml/ -I external/ocamlgraph/ \
	 -I utils -I parsing -I typing -I liquid \
				 -I $(LIBFIX) -I $(LIBMISC)

UTILS=utils/miscutil.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/tbl.cmo utils/consistbl.cmo utils/heap.cmo utils/bstats.cmo \
	utils/trie.cmo utils/mystats.cmo

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo \
  parsing/pparse.cmo

TYPING=typing/unused_var.cmo typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/stypes.cmo typing/typecore.cmo \
  typing/common.cmo typing/predicate.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/qualgen.cmo \
  typing/typemod.cmo

LIQUID=liquid/qualmod.cmo liquid/lightenv.cmo \
  liquid/qualifier.cmo liquid/pattern.cmo liquid/frame.cmo \
	liquid/qualdecl.cmo \
  liquid/builtins.cmo liquid/message.cmo \
  liquid/theoremProverZ3.cmo liquid/theoremProver.cmo liquid/wellformed.cmo \
  liquid/constraint.cmo liquid/predglue.cmo \
	liquid/consglue.cmo liquid/fixsolve.cmo \
	liquid/measure.cmo liquid/qualifymod.cmo \
  liquid/qdebug.cmo liquid/normalize.cmo \
  liquid/qdump.cmo liquid/liqerrors.cmo \
	liquid/mlqmod.cmo liquid/liquid.cmo

LIQOBJS=$(UTILS) $(PARSING) $(TYPING) $(LIQUID)

default: liquid.opt

fix.cmxa:
	cd external/fixpoint && make

liquid.byte: $(LIQOBJS)
	$(CAMLC) $(LINKFLAGS) -custom -o liquid.byte str.cma unix.cma nums.cma graph.cma $(LIQOBJS)

liquid.opt: $(LIQOBJS:.cmo=.cmx) fix.cmxa
	$(CAMLOPT) $(LINKFLAGS) -o liquid.opt $(LIBDIR)/libcamlidl.a str.cmxa unix.cmxa nums.cmxa z3.cmxa graph.cmxa fix.cmxa $(LIQOBJS:.cmo=.cmx)

.PHONY: tests
tests:
	./regrtest.py

.PHONY: theories
theories:
	ocamlc theories/*.mli

depend: beforedepend
	(for d in utils parsing typing liquid; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

clean: partialclean
	(for d in utils parsing typing liquid; \
	 do rm -f $$d/*.cm* $$d/*.o; \
	 done);
	rm -f liquid.byte liquid.opt

libclean: z3clean graphclean miscclean fixpointclean

z3clean:
	cd external/z3/ocaml; ./clean.sh

graphclean:
	cd external/ocamlgraph && make clean

miscclean:
	cd external/misc && make clean

fixpointclean:
	cd external/fixpoint && make clean

distclean: clean
	(for d in ./ utils parsing typing tests liquid; \
	 do rm -f $$d/*.annot $$d/*~ $$d/*.quals $$d/*.pyc $$d/*.dot; \
	 done);
	rm -rf .git

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(DSOLVELIBDIR)|' \
	    -e 's|%%BYTERUN%%|$(BINDIR)/ocamlrun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%BYTELINK%%|$(BYTECC) $(BYTECCLINKOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
	    -e 's|%%NATIVELINK%%|$(NATIVECC) $(NATIVECCLINKOPTS)|' \
	    -e 's|%%PARTIALLD%%|$(PARTIALLD) $(NATIVECCLINKOPTS)|' \
	    -e 's|%%PACKLD%%|$(PARTIALLD) $(NATIVECCLINKOPTS) -o |' \
	    -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
	    -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
	    -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
	    -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
	    -e 's|%%ARCH%%|$(ARCH)|' \
	    -e 's|%%MODEL%%|$(MODEL)|' \
	    -e 's|%%SYSTEM%%|$(SYSTEM)|' \
	    -e 's|%%EXT_OBJ%%|.o|' \
	    -e 's|%%EXT_ASM%%|.s|' \
	    -e 's|%%EXT_LIB%%|.a|' \
	    -e 's|%%EXT_DLL%%|.so|' \
	    -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
	    utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

z3lib:
	cd external/z3/ocaml; ./build.sh $(LIBDIR)

graphlib:
	cd external/ocamlgraph; ./configure --libdir $(LIBDIR); $(MAKE) all;

misclib:
	cd external/misc; $(MAKE)

fixpointlib:
	cd external/fixpoint; $(MAKE)

# build the OCaml bootstrap compiler
LIBFILES=*.cmi
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	cd stdlib; $(MAKE) COMPILER=../boot/ocamlc all
	cd stdlib; cp $(LIBFILES) ../boot; cp $(LIBFILES) ../theories

libs: z3lib graphlib misclib fixpointlib coldstart

world: liquid.opt

install:
	cp external/z3/bin/z3.dll . && cp external/yices/lib/cygyices.dll .

include .depend

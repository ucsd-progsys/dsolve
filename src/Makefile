CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLDEP=ocamldep
DEPFLAGS=$(INCLUDES)

COMPFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=-cclib -loyices -cclib -lgmp -cclib -lyices -I ../external/yices/lib/
INCLUDES=-I ../external/yices/lib/ -I ../external/ocamlgraph/ \
         -I utils -I parsing -I typing -I qualifying -I driver -I analyzer

UTILS=utils/misc.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/tbl.cmo utils/consistbl.cmo utils/heap.cmo utils/bstats.cmo

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/unused_var.cmo typing/ident.cmo typing/path.cmo \
  typing/predicate.cmo typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/stypes.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

QUALIFYING=qualifying/lightenv.cmo \
  qualifying/qualifier.cmo qualifying/builtins.cmo qualifying/frame.cmo \
  qualifying/wellformed.cmo qualifying/message.cmo qualifying/pattern.cmo \
  qualifying/theoremProverSimplify.cmo qualifying/theoremProverYices.cmo \
  qualifying/theoremProver.cmo \
  qualifying/constraint.cmo qualifying/qualgen.cmo \
  qualifying/printqual.cmo qualifying/qualifymod.cmo \
  qualifying/qdebug.cmo qualifying/normalize.cmo

LIQUID=driver/pparse.cmo analyzer/liqerrors.cmo analyzer/liquid.cmo

LIQOBJS=$(UTILS) $(PARSING) $(TYPING) $(QUALIFYING) $(LIQUID)

liquid: $(LIQOBJS)
	$(CAMLC) $(LINKFLAGS) -custom -o liquid str.cma unix.cma oyices.cma $(LIQOBJS)

liquid.opt: $(LIQOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o liquid.opt str.cmxa unix.cmxa oyices.cmxa $(LIQOBJS:.cmo=.cmx)

depend:
	(for d in utils parsing typing qualifying driver analyzer; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

include .depend

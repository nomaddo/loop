OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlopt ${LIB}
OCAMLDEP=ocamlfind ocamldep

INCLUDE=-I etc -I parsing -I typing -I ir -I driver
LIB=-package ppx_deriving.show,batteries
LLIB=

INTF    = \
	${addprefix parsing/, parser.cmi}                                    \

ETC     = ${addprefix etc/, etc.cmo flags.cmo}
PARSING = ${addprefix parsing/, pident.cmo ast.cmo parser.cmo lexer.cmo}
TYPING  = ${addprefix typing/, btypes.cmo tident.cmo intf_mod.cmo typed_ast.cmo tyenv.cmo typing.cmo}
IR      = ${addprefix ir/, typ.cmo operand.cmo ir.cmo dump.cmo transl.cmo}
DRIVER  = ${addprefix driver/, options.cmo main.cmo}

OBJS= ${ETC} ${PARSING} ${TYPING} ${IR} ${DRIVER}
XOBJS= ${subst, cmo, cmx, OBJS}
OPT=-g -bin-annot

loop: ${INTF} ${OBJS} libloop.cma
	$(OCAMLC) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

loop.opt: ${INTF} ${XOBJS}
	$(OCAMLOPT) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

libloop.cma: ${INTF} ${OBJS}
	$(OCAMLC) ${LIB} ${OPT} -a -o $@ ${OBJS}

depend:
	$(OCAMLDEP) `find -name "*.ml" -or -name "*.mli"` > .depend

clean:
	rm -rf loop loop.opt `find -name "*.cm?"` parsing/{parser.ml,parser.mli,lexer.ml}

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	${OCAMLC} ${INCLUDE} -c ${LIB} ${OPT} $<
%.cmo: %.ml
	${OCAMLC} ${INCLUDE} -c ${LIB} ${OPT} $<
%.cmx: %.ml
	${OCAMLOPT} ${INCLUDE} -c ${LIB} ${OPT} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	menhir --infer -v --ocamlc 'ocamlc -I parsing' $<
%.mli: %.mly
	menhir --infer -v --ocamlc 'ocamlc -I parsing' $<

.PHONY: clean test

include .depend

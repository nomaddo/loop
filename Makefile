OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlopt ${LIB}
OCAMLDEP=ocamlfind ocamldep

INCLUDE=-I etc -I parsing -I typing -I ir -I asmgen -I driver
LIB=-package ppx_deriving.show,batteries
LLIB=

INTF    = \
	${addprefix parsing/, parser.cmi}

ETC     = ${addprefix etc/, etc.cmo flags.cmo}
PARSING = ${addprefix parsing/, pident.cmo ast.cmo parser.cmo lexer.cmo}
TYPING  = ${addprefix typing/, btypes.cmo tident.cmo intf_mod.cmo typed_ast.cmo tyenv.cmo typing.cmo}
IR      = ${addprefix ir/, typ.cmo operand.cmo ir.cmo ir_util.cmo dump.cmo ila_check.cmo transl.cmo simplify.cmo ir_main.cmo}
ASMGEN  = ${addprefix asmgen/, ilb.cmo ilb_dump.cmo ilb_util.cmo ilb_simplify.cmo toilb.cmo sa.cmo ra.cmo asmgen.cmo}
DRIVER  = ${addprefix driver/, options.cmo main.cmo}

FILES= ${ETC} ${PARSING} ${TYPING} ${IR} ${ASMGEN} ${DRIVER}
OBJS= $(filter %.cmo, ${FILES})
XOBJS= ${subst cmo,cmx,${OBJS}}
OPT=-g -bin-annot

loop: ${INTF} ${FILES} libloop.cma
	$(OCAMLC) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

loop.opt: ${INTF} ${XOBJS}
	$(OCAMLOPT) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${XOBJS}

libloop.cma: ${INTF} ${OBJS}
	$(OCAMLC) ${LIB} ${OPT} -a -o $@ ${OBJS}

depend:
	$(OCAMLDEP) `find -name "*.ml" -or -name "*.mli"` > .depend

clean:
	rm -rf loop loop.opt `find -name "*.cm?"` parsing/{parser.ml,parser.mli,lexer.ml}

parsing/parser.ml: parsing/pident.cmo parsing/ast.cmo
parsing/parser.mli: parsing/pident.cmo parsing/ast.cmo

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

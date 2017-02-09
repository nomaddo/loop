OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
OCAMLDEP=ocamlfind ocamldep

INCLUDE=-I etc -I parsing -I typing -I driver
LIB=-package ppx_deriving.show,batteries,re
LLIB=

INTF=parsing/
ETC    =${addprefix etc/, etc.cmo flags.cmo}
PARSING=${addprefix parsing/, pident.cmo ast.cmo parser.cmo lexer.cmo}
TYPING= ${addprefix typing/, btypes.cmo tident.cmo intf_mod.cmo typed_ast.cmo env.cmo typing.cmo}
DRIVER= ${addprefix driver/, options.cmo main.cmo}
OBJS= ${ETC} ${PARSING} ${TYPING} ${DRIVER}
OPT=-g

loop: ${INTF} ${OBJS}
	$(OCAMLC) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

depend:
	$(OCAMLDEP) `find -name "*.ml" -or -name "*.mli"` > .depend

clean:
	rm -rf loop `find -name "*.cm?"` parsing/{parser.ml,parser.mli,lexer.ml}

parser:
	cd parsing; menhir --infer -v parser.mly

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

parser.mli: parser.mly
	menhir --infer -v $<

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

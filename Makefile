OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=menhir

LIB=-package sexplib,ppx_sexp_conv
LLIB=

OBJS=ast.cmo parser.cmo lexer.cmo main.cmo
OPT=-g

loop: ${OBJS}
	$(OCAMLC) ${LIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

depend:
	$(OCAMLDEP) *.ml? > .depend

clean:
	rm -rf *.o *.cm* parser.ml parser.mli lexer.ml *.output

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

parser.mli: parser.mly
	$(OCAMLYACC) -v $<

%.cmi: %.mli
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmo: %.ml
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmx: %.ml
	${OCAMLOPT} -c ${LIB} ${OPT} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	menhir --infer -v $<

.PHONY: clean test

include .depend

INC=-I common -I syntax/inline
OCAMLBUILDFLAGS=-use-ocamlfind $(INC)
all:
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt \
	syntax/inline/inline.cma 

top:
	rlwrap ocaml -I _build/common -I _build/syntax/inline $(INC) -init ocaml.init

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

view: doc
	xdg-open $$PWD/_build/mlorg.docdir/index.html
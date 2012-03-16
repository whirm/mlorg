INC=-I common
OCAMLBUILDFLAGS=-use-ocamlfind $(INC)
all:
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt \
	common/numbering.cma 

top:
	rwlrap ocaml -I _build/common $(INC) -init ocaml.init

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

view: doc
	xdg-open $$PWD/_build/mlorg.docdir/index.html
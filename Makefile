INC=-I common -I syntax/inline
OCAMLBUILDFLAGS=-use-ocamlfind $(INC)
TARGET=syntax/blocks/parser.cma
_build/$(TARGET):
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt $(TARGET)

top: _build/$(TARGET)
	rlwrap ocaml -I _build/common -I _build/syntax -I _build/syntax/blocks $(INC) -init ocaml.init

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

view: doc
	xdg-open $$PWD/_build/mlorg.docdir/index.html

.PHONY: _build/$(TARGET) top doc view
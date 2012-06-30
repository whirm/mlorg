INC=-I common -I syntax/inline -I syntax/blocks -I document -I exports
OCAMLBUILDFLAGS=-use-ocamlfind $(INC)
TARGET=main.native
_build/$(TARGET):
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt $(TARGET)
_build/main.cma:
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt main.cma
_build/main.cmxa:
	ocamlbuild $(OCAMLBUILDFLAGS) -ocamlc ocp-ocamlc -ocamlopt ocp-ocamlopt main.cmxa



top: _build/main.cma
	rlwrap ocaml -I _build/common -I _build/syntax/inline -I _build/document -I _build/exports -I _build/syntax/blocks -I _build/document/ $(INC) -init ocaml.init

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

view: doc
	xdg-open $$PWD/_build/mlorg.docdir/index.html

README.html: main.native README.org
	./main.native --filename README.org --backend html
.PHONY: _build/$(TARGET) top doc viewrm 
INC=-I common -I syntax/inline -I syntax/blocks -I document -I exports

all:
	ocaml setup.ml -build

top: all
	rlwrap ocaml -I _build/common -I _build/syntax/inline -I _build/document -I _build/exports -I _build/syntax/blocks -I _build/document/ $(INC) -init ocaml.init

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

web: README.html
	mkdir -p $(WEBDDESTIDR)
	cp README.html $(WEBDESTDIR)/index.html

README.html: all README.org
	./main.native --filename README.org --backend html

INC=-I common -I syntax/inline -I syntax/blocks -I document -I exports

all:
	ocaml setup.ml -build

top: all
	rlwrap ocaml -I _build/common -I _build/syntax/inline -I _build/document -I _build/exports -I _build/syntax/blocks -I _build/document/ $(INC) -init ocaml.init

install:
	ocaml setup.ml -reinstall
	cp _build/main.native $(PREFIX)/bin/mlorg
	cp _build/main.byte $(PREFIX)/bin/mlorg.byte

doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html
%.html: all %.org
	./_build/main.native --filename $^ --backend html

web: TUTORIAL.html index.html manual.html doc
	mkdir -p $(WEBDESTDIR)/doc
	cp index.html manual.html $(WEBDESTDIR)
	cp _build/mlorg.docdir/* $(WEBDESTDIR)/doc -Rf
testorg:
	cat $(FILE) | mlorg --backend org > 1.org
	cat 1.org | mlorg --backend org > 2.org
	diff 1.org 2.org

README.html: all README.org
	./_build/mlorg --filename README.org --backend html

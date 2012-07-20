INC=-I common -I syntax/inline -I syntax/blocks -I document -I exports

all:
	ocaml setup.ml -build

top: all
	rlwrap ocaml -I _build/common -I _build/syntax/inline -I _build/document -I _build/exports -I _build/syntax/blocks -I _build/document/ $(INC) -init ocaml.init

install:
	ocaml setup.ml -reinstall
doc:
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html

web: README.html
	mkdir -p $(WEBDESTDIR)
	cp README.html $(WEBDESTDIR)/index.html

testorg:
	cat $(FILE) | mlorg --backend org > 1.org
	cat 1.org | mlorg --backend org > 2.org
	diff 1.org 2.org

README.html: all README.org
	./_build/mlorg --filename README.org --backend html

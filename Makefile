INC=-I common -I syntax/inline -I syntax/blocks -I document -I exports
-include setup.data
all:
	ocaml setup.ml -build

top: all
	rlwrap ocaml -I _build/common -I _build/syntax/inline -I _build/document -I _build/exports -I _build/syntax/blocks -I _build/document/ $(INC) -init ocaml.init

install:
	cp _build/main.native $(prefix)/bin/mlorg
	cp _build/main.byte $(prefix)/bin/mlorg.byte
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall
	rm -f $(prefix)/bin/mlorg*

doc: 
	ocamlbuild $(OCAMLBUILDFLAGS) mlorg.docdir/index.html
%.html: %.org all
	./_build/main.native -o $@ --backend html $< --option=general.math2png.inline=yes
	mv lxtpng/* docs/lxtpng/
%.pdf: %.tex
	pdflatex --output-directory=$$(dirname $<) $< 

%.tex: %.org all
	./_build/main.native -o $@ --backend latex $< 

web: TUTORIAL.html index.html manual.html doc
	mkdir -p $(WEBDESTDIR)/doc
	cp index.html manual.html $(WEBDESTDIR)
	cp _build/mlorg.docdir/* $(WEBDESTDIR)/doc -Rf
testorg:
	cat $(FILE) | mlorg --backend org > 1.org
	cat 1.org | mlorg --backend org > 2.org
	diff 1.org 2.org

.PHONY: install uninstall

all: SYNTAX.html

%.html: %.org
	../_build/main.native -o $@ --backend html $< --option=general.math2png.inline=yes

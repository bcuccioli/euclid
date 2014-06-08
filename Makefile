all: main

main: *.ml
	@ocamllex lexer.mll
	@ocamlyacc parser.mly
	@rm parser.mli
	@ocamlopt -o main Util.ml Polynomial.ml Ideal.ml Interp.ml parser.ml lexer.ml Main.ml

clean:
	@git clean -fdX --quiet

.PHONY: clean

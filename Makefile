all: Main.native

Main.native:
	@ocamlbuild -use-menhir Main.native

clean:
	@git clean -fdX

.PHONY: clean

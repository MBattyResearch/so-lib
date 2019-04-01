all:
	ocamlbuild -use-ocamlfind example.byte

clean:
	ocamlbuild -use-ocamlfind -clean

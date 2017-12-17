test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	rm -f finalsrc.zip

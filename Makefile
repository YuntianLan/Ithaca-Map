build:
	ocamlbuild -use-ocamlfind trie.cmo graph.cmo image.cmo server.cmo client.cmo
zip:
	zip src.zip *.ml*
install-dep:
	opam update
	opam install lablgtk cohttp
simplegui:
	ocamlfind ocamlc -g -package lablgtk2 -linkpkg simple.ml -o simple
	./simple
clean:
	ocamlbuild -clean
	rm -f src.zip
check:
	bash checkenv.sh

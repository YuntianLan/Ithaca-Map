build:
	ocamlbuild -use-ocamlfind trie.cmo graph.cmo image.cmo server.cmo client.cmo
runserver:
	ocamlbuild -use-ocamlfind server.byte
	./server.byte
zip:
	zip src.zip *.ml*
install-dep:
	opam update
	opam install lablgtk cohttp
simplegui:
	ocamlfind ocamlc -g -package lablgtk2 -linkpkg GUI/simple.ml -o simple
	./simple
clean:
	ocamlbuild -clean
	rm -f src.zip
	rm *.cmo
	rm *.cmi
check:
	bash checkenv.sh

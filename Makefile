build:
	ocamlbuild -use-ocamlfind trie.cmo graph.cmo image.cmo server.cmo client.cmo
zip:
	zip src.zip *.ml*
install-dep:
	opam update
	opam install lablgtk cohttp
clean:
	ocamlbuild -clean
	rm -f src.zip

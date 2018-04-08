build:
	ocamlbuild -use-ocamlfind graph.cmo image.cmo server.cmo client.cmo
zip:
	zip src.zip *.ml*
dependencies:
	opam update
	opam install lablgtk cohttp
clean:
	ocamlbuild -clean
	rm -f src.zip

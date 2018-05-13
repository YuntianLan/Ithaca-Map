build:
	ocamlbuild -use-ocamlfind trie.cmo graph.cmo image.cmo


server:
	ocamlbuild -use-ocamlfind httpserver.native
	./httpserver.native

test:
	ocamlbuild -use-ocamlfind test_trie.byte && ./test_trie.byte
	ocamlbuild -use-ocamlfind test_image.byte && ./test_image.byte
	ocamlbuild -use-ocamlfind test_graph.byte && ./test_graph.byte
zip:
	zip src.zip *.ml*
install-dep:
	opam update
	opam install js_of_ocaml js_of_ocaml-ocamlbuild js_of_ocaml-camlp4 js_of_ocaml-lwt
	opam install cohttp cohttp-lwt-unix
	opam install yojson
	opam install camlimages
clean:
	ocamlbuild -clean
	rm -f src.zip
	rm *.cmo
	rm *.cmi
check:
	bash checkenv.sh

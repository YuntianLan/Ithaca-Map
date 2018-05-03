build:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.ppx \
	 -package js_of_ocaml-lwt -package js_of_ocaml.syntax -linkpkg -o gui.byte gui.ml

	js_of_ocaml gui.byte

gui:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui.d.js

gui1:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui1.d.js

cohttpserver:
	ocamlbuild -use-ocamlfind cohttpserv.native
	./cohttpserv.native
clean:
	ocamlbuild -clean
	rm -rf *.js
	rm -rf *.cmi *.cmo *.byte

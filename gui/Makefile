gui:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui.d.js
	ocamlbuild -use-ocamlfind clientgui.cmo

gui1:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui1.d.js
	ocamlbuild -use-ocamlfind clientgui1.cmo
cohttpserver:
	ocamlbuild -use-ocamlfind cohttpserv.native
	./cohttpserv.native
clean:
	ocamlbuild -clean
	rm -rf *.cmi *.cmo *.byte

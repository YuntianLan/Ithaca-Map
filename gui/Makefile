gui:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui.d.js

gui1:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links gui1.d.js
	ocamlbuild -use-ocamlfind clientgui.cmo
cohttpserver:
	ocamlbuild -use-ocamlfind cohttpserv.native
	./cohttpserv.native
clean:
	ocamlbuild -clean
	rm -rf *.js
	rm -rf *.cmi *.cmo *.byte

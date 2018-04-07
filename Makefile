zip:
	zip src.zip *.ml*

clean:
	ocamlbuild -clean
	rm -f src.zip

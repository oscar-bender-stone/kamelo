all:
	ocamllex -ml src/parsing/klexer.mll
	dune build @install
	cp _build/default/main.exe KaMeLo

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean
	rm -f *klexer.ml
	rm -f *kparser.ml *kparser.mli
	rm -f *.lp *.dk *.mykore
	rm -f *~ KaMeLo

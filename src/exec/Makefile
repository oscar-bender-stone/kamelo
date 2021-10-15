all:
	ocamllex -ml run_lexer.mll
	dune build @install

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean
	rm -f *klexer.ml
	rm -f *kparser.ml *kparser.mli
	rm -f *.lp *.dk *.mykore
	rm -f *~

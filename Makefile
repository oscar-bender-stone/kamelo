all:
	ocamllex -ml src/parsing/klexer.mll
	dune build main.exe
	cp _build/default/main.exe KaMeLo

#test:
#	dune runtest

test-lp:
	sh tests/gen_tests.sh lp

#test-dk:
#	sh tests/gen_tests.sh dk

test-clean:
	rm -rf tests/*/*-kompiled

rewrite:
	python3 src/printing/rewrite.py $1

doc:
	dune build @doc

clean:
	dune clean
	rm -f *klexer.ml
	rm -f *kparser.ml *kparser.mli
	rm -f *.lp *.pkg *.dk *.mykore
	rm -f *~ KaMeLo

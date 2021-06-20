%.cmo: %.ml
	ocamlc -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

# Compilation parameters:
CAMLOBJS=type.cmo pos.cmo kparser.cmo klexer.cmo syntax.cmo display_console.cmo \
		 LP_p_term.cmo LP_printer.cmo output.cmo axiom.cmo symbol.cmo main.cmo
CAMLSRC=$(addsuffix .ml,$(basename $(CAMLOBJS)))
FILES=klexer.mll type.ml pos.ml syntax.ml display_console.ml LP_p_term.ml \
      axiom.ml symbol.ml LP_printer.ml kparser.mly output.ml main.ml Makefile

all: kamelo

kamelo: $(CAMLOBJS)
	ocamlc -g -o kamelo unix.cma str.cma $(CAMLOBJS)

clean:
	rm -f *.cmi *.cmo
	rm -f kamelo
	rm -f klexer.ml
	rm -f kparser.ml kparser.mli
	rm -f kparser.output
	rm -f *.lp
	rm -f *~

klexer.ml: pos.ml klexer.mll
	ocamllex klexer.mll
	ocamlc -g -c klexer.ml

kparser.ml: kparser.mly klexer.ml
	menhir --external-tokens Klexer kparser.mly
	ocamlc -g -c kparser.mli

main.ml: klexer.ml kparser.ml kparser.mli type.ml syntax.ml pos.ml LP_p_term.ml
	ocamlc -g -c main.ml

main.cmi: main.ml
main.cmo: main.ml main.cmi

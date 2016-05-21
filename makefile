OCAMLC=ocamlopt
OPAMDIR=~/.opam/system/lib/
OPAMINCLUDES=zarith nocrypto cstruct sexplib ocplib-endian
DEPS= str nocrypto
SRCML= cobalt.ml
SRCMLI=

cobalt: cobalt.ml
	ocamlfind ocamlmklib -o $@ -linkpkg  $(addprefix -package , $(DEPS)) $(SRCML)

test: cobalt.cmi cobalt
	ocamlfind $(OCAMLC) -linkpkg -package nocrypto -package str cobalt.cmx cobalt_test.ml -o test

%.cmi: %.mli
	$(OCAMLC) -c $<

clean:
	rm -f *.o
	rm -f *.cmx
	rm -f *.cmi
	rm -f *.cmo
	rm -f *.a
	rm -f *.cma
	rm -f *.cmxa

distclean: clean
	rm -f test

OCAMLC=ocamlopt
OPAMDIR=~/.opam/system/lib/
OPAMINCLUDES=zarith nocrypto cstruct sexplib ocplib-endian
DEPS= bigarray sexplib bigstring cstruct zarith nocrypto str
SRCML= cobalt.ml
SRCMLI=

foo: cobalt.ml
	$(OCAMLC) -o $@ $(addprefix -I $(OPAMDIR), $(OPAMINCLUDES)) $(addsuffix .cmxa, $(DEPS)) $(SRCML)

foo_q: main.ml
	$(OCAMLC) -o $@ $(addprefix -I $(OPAMDIR), $(OPAMINCLUDES)) $(addsuffix .cmxa, $(DEPS)) main_queue.ml

%.cmi: %.mli
	$(OCAMLC) -c $<

clean:
	rm -rf *.o
	rm -rf *.cmx
	rm -rf *.cmi
	rm -rf *.cmo

OCAMLC = ocamlc.opt -g
OCAMLOPT = ocamlopt.opt

# all:omd_backend.cmo omd_parser.cmo omd_lexer.cmo omd_lexer_fs.cmo

omd:omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.byte:omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo omd_main.cmo
	$(OCAMLC) -o $@ $+

omd.opt:omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.cma:omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo
	$(OCAMLC) -a -o $@ $+

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

%.cmi:%.mli
	$(OCAMLC) -c $<

omd.cmx:omd.cmi

omd_parser.cmo:omd_lexer.cmo omd_backend.cmo
omd_lexer.cmo:
omd_lexer_fs.cmo:omd_lexer.cmo

omd_parser.cmx:omd_lexer.cmx omd_backend.cmx
omd_lexer.cmx:
omd_lexer_fs.cmx:omd_lexer.cmx

omd_backend.cmo:
omd_backend.cmx:

omd_main.cmo:omd.cmo
omd.cmo:omd_backend.cmo omd_lexer.cmo omd_parser.cmo 
omd_main.cmx:omd.cmx 
omd.cmx:omd_backend.cmx omd_lexer.cmx omd_parser.cmx

clean:
	rm -f *.cmx *.cmo *~ *.cmi *.o *.cma md{,.byte,.opt} omd{,.byte,.opt}

test: md.cma
	ocaml md.cma cow_tests/test.ml

htest:
	make ; rlwrap ocaml omd_lexer.cmo omd_parser.cmo 


# compatibility (temporary)
md:omd
	cp $< $@
md.byte:omd.byte
	cp $< $@
md.opt:omd.opt
	cp $< $@


OCAMLC = ocamlc.opt -g
OCAMLOPT = ocamlopt.opt

# all:omd_backend.cmo omd_parser.cmo omd_lexer.cmo omd_lexer_fs.cmo

omd:omd_utils.cmx omd_representation.cmx omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.byte:omd_utils.cmo omd_representation.cmo omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo omd_main.cmo
	$(OCAMLC) -o $@ $+

omd.opt:omd_utils.cmx omd_representation.cmx omd_backend.cmx omd_lexer.cmx omd_parser.cmx omd.cmx omd_main.cmx
	$(OCAMLOPT) -o $@ $+

omd.cma:omd_utils.cmo omd_representation.cmo omd_backend.cmo omd_lexer.cmo omd_parser.cmo omd.cmo
	$(OCAMLC) -a -o $@ $+

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

%.cmi:%.mli
	$(OCAMLC) -c $<

omd_parser.cmo:omd_utils.cmo omd_representation.cmo omd_lexer.cmo omd_backend.cmo
omd_lexer.cmo:omd_representation.cmo
omd_lexer_fs.cmo:omd_lexer.cmo
omd_representation.cmo:omd_utils.cmo

omd_representation.cma:omd_utils.cma

omd_parser.cmx:omd_utils.cmx omd_representation.cmx omd_lexer.cmx omd_backend.cmx
omd_lexer.cmx:omd_representation.cmx
omd_lexer_fs.cmx:omd_lexer.cmx

omd_backend.cmo:omd_representation.cmo
omd_backend.cmx:omd_representation.cmx

omd.cmi:omd_representation.cmi omd_backend.cmi omd_lexer.cmi omd_parser.cmi
omd_backend.cmi:omd_representation.cmi omd_backend.cmo
omd_lexer.cmi:omd_representation.cmi omd_lexer.cmo
omd_parser.cmi:omd_representation.cmi omd_backend.cmi omd_lexer.cmi omd_parser.cmo
omd_representation.cmi:omd_representation.cmo
omd_utils.cmi:omd_utils.cmo

omd_main.cmo:omd.cmo
omd.cmo:omd.cmi omd_representation.cmo omd_backend.cmo omd_lexer.cmo omd_parser.cmo
omd_main.cmx:omd.cmx 
omd.cmx:omd.cmi omd_representation.cmx omd_backend.cmx omd_lexer.cmx omd_parser.cmx

clean:
	rm -f *.cmx *.cmo *~ *.cmi *.o *.cma md{,.byte,.opt} omd{,.byte,.opt}

test: omd.cma
	ocaml omd.cma cow_tests/test.ml


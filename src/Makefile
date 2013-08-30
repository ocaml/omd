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

clean:
	rm -f *.cmx *.cmo *~ *.cmi *.o *.cma md{,.byte,.opt} omd{,.byte,.opt}

test: omd.cma
	ocaml omd.cma cow_tests/test.ml


omd_utils.cmi : omd_utils.ml
	ocamlc -c $<
omd_representation.cmi:omd_representation.ml  omd_utils.cmi
	ocamlc -c $<
omd.cmi:omd.mli omd_utils.cmi
	ocamlc -c $<

omd_utils.cmo :
omd_utils.cmx :
omd_representation.cmo : omd_utils.cmo
omd_representation.cmx : omd_utils.cmx
omd_backend.cmo : omd_utils.cmo omd_representation.cmo
omd_backend.cmx : omd_utils.cmx omd_representation.cmx
omd_lexer.cmo : omd_representation.cmo
omd_lexer.cmx : omd_representation.cmx
omd_parser.cmo : omd_utils.cmo omd_representation.cmo omd_lexer.cmo \
    omd_backend.cmo
omd_parser.cmx : omd_utils.cmx omd_representation.cmx omd_lexer.cmx \
    omd_backend.cmx
omd.cmo : omd_utils.cmo omd_representation.cmo omd_parser.cmo omd_lexer.cmo \
    omd_backend.cmo omd.cmi
omd.cmx : omd_utils.cmo omd_representation.cmx omd_parser.cmx omd_lexer.cmx \
    omd_backend.cmx omd.cmi
omd_main.cmo : omd_utils.cmo omd_parser.cmo omd_backend.cmo omd.cmi
omd_main.cmx : omd_utils.cmx omd_parser.cmx omd_backend.cmx omd.cmx



# include .depend

.depend:omd_utils.ml omd_representation.ml omd_backend.ml omd_lexer.ml omd_parser.ml omd.ml omd_main.ml
	ocamldep $? > .depend

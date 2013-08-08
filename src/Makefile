OCAMLC = ocamlc.opt -g
OCAMLOPT = ocamlopt.opt

# all:md_backend.cmo md_parser.cmo md_lexer.cmo md_lexer_fs.cmo

md:md_backend.cmx md_lexer.cmx md_parser.cmx md_lib.cmx md_main.cmx
	$(OCAMLOPT) -o $@ $+

md.byte:md_backend.cmo md_lexer.cmo md_parser.cmo md_lib.cmo md_main.cmo
	$(OCAMLC) -o $@ $+

md.opt:md_backend.cmx md_lexer.cmx md_parser.cmx md_lib.cmx md_main.cmx
	$(OCAMLOPT) -o $@ $+

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

md_parser.cmo:md_lexer.cmo md_backend.cmo
md_lexer.cmo:
md_lexer_fs.cmo:md_lexer.cmo

md_parser.cmx:md_lexer.cmx md_backend.cmx
md_lexer.cmx:
md_lexer_fs.cmx:md_lexer.cmx

md_backend.cmo:
md_backend.cmx:

md_main.cmo:md_lib.cmo
md_lib.cmo:md_backend.cmo md_lexer.cmo md_parser.cmo 
md_main.cmx:md_lib.cmx 
md_lib.cmx:md_backend.cmx md_lexer.cmx md_parser.cmx

clean:
	rm -f *.cmx *.cmo *~ *.cmi *.o md

htest:
	make ; rlwrap ocaml md_lexer.cmo md_parser.cmo 




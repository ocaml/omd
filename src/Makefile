OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt

all:md_parser.cmo md_lexer.cmo md_lexer_fs.cmo

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

md_parser.cmo:md_lexer.cmo
md_lexer.cmo:
md_lexer_fs.cmo:md_lexer.cmo

md_parser.cmx:md_lexer.cmx
md_lexer.cmx:
md_lexer_fs.cmx:md_lexer.cmx


clean:
	rm -f *.cmx *.cmo *~ *.cmi 

htest:
	make ; rlwrap ocaml md_lexer.cmo md_parser.cmo 




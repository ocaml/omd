OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt

all:md.cmo md_parser.cmo md_lexer.cmo md_lexer_fs.cmo

md:md.cmx md_lexer.cmx md_parser.cmx md_main.cmx
	$(OCAMLOPT) -o $@ $?

%.cmo:%.ml
	$(OCAMLC) -c $<

%.cmx:%.ml
	$(OCAMLOPT) -c $<

md_parser.cmo:md_lexer.cmo md.cmo
md_lexer.cmo:
md_lexer_fs.cmo:md_lexer.cmo

md_parser.cmx:md_lexer.cmx md.cmo
md_lexer.cmx:
md_lexer_fs.cmx:md_lexer.cmx

md.cmo:
md.cmx: 


md_main.cmo:md.cmo md_parser.cmo md_lexer.cmo
md_main.cmx:md.cmx md_parser.cmx md_lexer.cmx

clean:
	rm -f *.cmx *.cmo *~ *.cmi 

htest:
	make ; rlwrap ocaml md_lexer.cmo md_parser.cmo 




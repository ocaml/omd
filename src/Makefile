all:md_parser.cmo md_lexer.cmo md_lexer_fs.cmo


%.cmo:%.ml
	ocamlc -c $<

%.cmx:%.ml
	ocamlopt -c $<

md_parser.cmo:md_lexer.cmo
md_lexer.cmo:
md_lexer_fs.cmo:md_lexer.cmo

md_parser.cmx:
md_lexer.cmx:
md_lexer_fs.cmx:


clean:
	rm -f *.cmx *.cmo *~ *.cmi 





# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

tony$(EXE): tony_ast.cmo tony_lexer.cmo tony_parser.cmo main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

tony_lexer.ml: tony_lexer.mll
	ocamllex -o $@ $<

tony_parser.ml tony_parser.mli: tony_parser.mly
	ocamlyacc -v $<

.PHONY: clean distclean

-include .depend

depend: tony_ast.mli tony_ast.ml tony_lexer.ml tony_lexer.mli tony_parser.ml tony_parser.mli main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) tony_lexer.ml tony_parser.ml tony_parser.mli tony_parser.output *.cmo *.cmi *~

distclean: clean
	$(RM) tony$(EXE) .depend

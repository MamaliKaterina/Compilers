# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep
FILES=Helping_types.cmo tony_lexer.cmo tony_parser.cmo tony_ActRec.cmo tony_ast.cmo main.cmo

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

tony$(EXE): Helping_types.cmo tony_parser.cmi tony_lexer.cmi tony_lexer.cmo tony_parser.cmo tony_ActRec.cmo tony_ast.cmo main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $(FILES)

tony_lexer.ml: tony_lexer.mll
	ocamllex -o $@ $<

tony_parser.ml tony_parser.mli: tony_parser.mly
	ocamlyacc -v $<

.PHONY: clean distclean

-include .depend

depend: Helping_types.ml tony_parser.mli tony_lexer.mli tony_lexer.ml tony_parser.ml tony_ActRec.ml tony_ast.ml  main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) tony_lexer.ml tony_parser.ml tony_parser.mli tony_parser.output *.cmo *.cmi *~

distclean: clean
	$(RM) tony$(EXE) .depend

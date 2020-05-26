.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

EXEFILE=tonyc$(EXE)
MLFILES=Hashcons.ml Identifier.ml Error.ml Helping_types.ml tony_symbol.ml \
  tony_lexer.ml tony_parser.ml tony_sem.ml main.ml
MLIFILES=Hashcons.mli Identifier.mli Error.mli tony_parser.mli tony_lexer.mli
CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))
PARSERFILES=tony_parser.ml tony_parser.mli tony_parser.output tony_lexer.ml
SRCFILES=Makefile extend.ml tony_lexer.mll tony_parser.mly \
  $(filter-out tony_parser.% tony_lexer.%,$(MLFILES)) $(filter-out tony_parser.%,$(MLIFILES))
CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLOPT_FLAGS=
OCAMLC_FLAGS=-g
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OCAMLOPT=ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP=ocamldep
INCLUDES=


default: tonyc$(EXE)

symbtest$(EXE): $(filter-out Lexer.cmo Parser.cmo,$(CMOFILES))
	$(OCAMLC) -o $@ $^

all: $(EXEFILE)

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I +camlp5 -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): tony_parser.mli tony_lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

tony_parser.ml tony_parser.mli: tony_parser.mly
	ocamlyacc -v $<

tony_lexer.ml: tony_lexer.mll
	ocamllex -o $@ $<


-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
	extend.cmi extend.cmo $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) symbtest$(EXE) .depend


pack: clean
	tar cvfz tonyc.tar.gz $(SRCFILES)

count:
	wc -l $(SRCFILES)

.PHONY: default all clean FORCE

LLVMCONFIG=llvm-config
LLVMLDFLAGS=-L`$(LLVMCONFIG) --libdir`
LLVMPACKAGES=llvm,llvm.analysis,llvm.all_backends,llvm.scalar_opts

OCAMLBUILD=ocamlbuild
OCAMLBUILDFLAGS=-pkgs $(LLVMPACKAGES) -lflags -cclib,$(LLVMLDFLAGS) -cflags -w,-A -quiet

default: Main.native

Main.native: FORCE
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@

Main.byte: FORCE
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@

clean:
	$(OCAMLBUILD) -clean
	$(RM) *.ll *.s *.out
	$(RM) *~

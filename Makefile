BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS=-Wall
GHC=ghc

SHELL_SCRIPTS=latc latc_x86_64
TEST_DIRECTORIES=good bad extensions gr5
FILES_TO_PACK=$(SHELL_SCRIPTS) src Makefile README $(TEST_DIRECTORIES)
PACK_NAME=fc359081.tgz

BINARIES=TestLatte Latte
FRONTEND_SOURCES=Context Errors Print Typechecker TypecheckerPure TypecheckerState TypecheckerAssert
BACKEND_SOURCES=Compiler AsmStandard AsmStmt EmitExpr EmitStmt CompilerState Optimize Label
SOURCES=Latte $(addprefix Backend/,$(BACKEND_SOURCES)) $(addprefix Frontend/,$(FRONTEND_SOURCES))
LINKED_SOURCES=$(addsuffix .hs,$(addprefix $(BUILD)/,$(SOURCES))) \
			   $(BUILD)/Frontend/Typechecker.hs-boot $(BUILD)/Frontend/Print.hs-boot
BNFC_MODULES=AbsLatte ErrM LexLatte ParLatte PrintLatte TestLatte
HPC_EXCLUDES=$(addprefix --exclude=,$(BNFC_MODULES))
BNFC_SOURCES_FILES=$(addsuffix .hs,$(BNFC_MODULES))
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean pack test testGood testBad

all: $(BINARIES)

test: testGood testBad

define test_examples
	@for e in $1/*.lat ; do \
		echo -e "\e[93m----------- TESTING\e[96m $$e \e[93m--------------\e[0m"; \
		./latc "$$e" 2>&1 | tee build/output ; \
		[ "x$$(head -n 1 build/output)" = "x$2" ] || exit 1;  \
	done
endef

# if [ "xOK" = "x$2" ]; then \
#  ./$${e%.lat}; \
# fi; \


testGood: Latte
	$(call test_examples,good,OK)
	$(call test_examples,good/basic,OK)
	$(call test_examples,good/arrays,OK)
	$(call test_examples,good/struct,OK)
	
#$(call test_examples,good/objects1,OK)
#$(call test_examples,good/objects2,OK)
#$(call test_examples,good/virtual,OK)
#$(call test_examples,good/hardcore,OK)

testBad: Latte
	$(call test_examples,bad,ERROR)
	$(call test_examples,bad/semantic,ERROR)
	$(call test_examples,bad/infinite_loop,ERROR)
	$(call test_examples,bad/arrays,ERROR)

TestLatte: $(BNFC_SOURCES) $(LINKED_SOURCES)
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) -w --make $@.hs -o ../$@

Latte: $(BNFC_SOURCES) $(LINKED_SOURCES) lib/runtime.o
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) --make $@.hs -o ../$@

$(LINKED_SOURCES): $(BUILD)/%: src/%
	ln -srf $^ -t $(BUILD)

$(BNFC_SOURCES): src/Latte.cf
	mkdir -p $(BUILD) && \
	cd $(BUILD) && \
	bnfc -haskell ../$< && \
	happy -gca ParLatte.y -iHappyOutput && \
	alex -g LexLatte.x

lib/runtime.o: lib/runtime.c
	gcc -c -o $@ $<

pack:
	tar czvf $(PACK_NAME) $(FILES_TO_PACK)

shellcheck:
	shellcheck $(SHELL_SCRIPTS)

coverage: GHCFLAGS=-fhpc

coverage: clean test
	mv Latte.tix $(BUILD) && \
	cd $(BUILD) && \
	hpc report Latte && \
	hpc markup Latte $(HPC_EXCLUDES)

clean:
	rm -rf $(BUILD) $(TMP) $(BINARIES) $(TEST_DIR) *.tgz *.tix lib/runtime.o

BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS= #-Wall
GHC=ghc
TMP=tmp
TEST_DIR=$(BUILD)/test

SHELL_SCRIPTS=latc latc_x86_64
FILES_TO_PACK=$(SHELL_SCRIPTS) src lib Makefile README
PACK_NAME=fc359081.tgz

BINARIES=Latte TestLatte
SOURCES=Compiler Latte Typechecker Errors
LINKED_SOURCES=$(addsuffix .hs,$(addprefix $(BUILD)/,$(SOURCES)))
BNFC_SOURCES_FILES=AbsLatte.hs ErrM.hs LexLatte.hs \
	ParLatte.hs PrintLatte.hs TestLatte.hs
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean pack test testGood testBad run runGood runBad 

all: $(BINARIES)

test: testGood testBad

define test_examples
	@for e in $1/*.lat ; do \
		echo -e "\e[93m----------- TESTING\e[96m $$e \e[93m--------------\e[0m"; \
		./Latte "$$e" 2>&1 | tee build/output ; \
		[ "x$$(head -n 1 build/output)" = "x$2" ] || exit 1;  \
	done
endef

testGood: Latte
	$(call test_examples,good,OK)
	$(call test_examples,good/basic,OK)

testBad: Latte
	$(call test_examples,bad,ERROR)
	$(call test_examples,bad/semantic,ERROR)
	$(call test_examples,bad/infinite_loop,ERROR)

define run_examples
	@for e in $1/*.lat ; do \
		echo -e "\e[93m----------- RUNNING\e[96m $$e \e[93m--------------\e[0m"; \
		./Latte "$$e" ; \
	done
endef

run: runGood runBad

runGood: good Latte
	$(call run_examples,good)
	$(call run_examples,good/basic)

runBad: bad Latte
	-$(call run_examples,$<)

$(BINARIES): %: $(BNFC_SOURCES) $(LINKED_SOURCES)
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) --make $@.hs -o ../$@

$(LINKED_SOURCES): $(BUILD)/%: src/%
	ln -srf $^ $(BUILD)

$(BNFC_SOURCES): src/Latte.cf
	mkdir -p $(BUILD) && \
	cd $(BUILD) && \
	bnfc -haskell ../$< && \
	happy -gca ParLatte.y && \
	alex -g LexLatte.x
	rm -f $(BUILD)/SkelLatte.hs && \
	sed -i "/SkelLatte/d" $(BUILD)/TestLatte.hs

pack:
	tar czvf $(PACK_NAME) $(FILES_TO_PACK)

shellcheck:
	shellcheck $(SHELL_SCRIPTS)

clean:
	rm -rf $(BUILD) $(TMP) $(BINARIES) $(TEST_DIR) *.tgz

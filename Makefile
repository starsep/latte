BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS= #-Wall
GHC=ghc
TMP=tmp
TEST_DIR=$(BUILD)/test

SHELL_SCRIPTS=insc_jvm insc_llvm usage.sh
FILES_TO_PACK=$(SHELL_SCRIPTS) src lib Makefile README
PACK_NAME=fc359081.tgz

BINARIES=Latte TestLatte
SOURCES=Compiler Latte Typechecker Errors
LINKED_SOURCES=$(addsuffix .hs,$(addprefix $(BUILD)/,$(SOURCES)))
BNFC_SOURCES_FILES=AbsLatte.hs ErrM.hs LexLatte.hs \
	ParLatte.hs PrintLatte.hs TestLatte.hs
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean pack

all: $(BINARIES)

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

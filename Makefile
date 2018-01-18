BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS=-Wall
GHC=ghc

SHELL_SCRIPTS=latc latc_x86_64
TESTS=$(shell find good bad -name "*.lat" -o -name "*.input" -o -name "*.output" | xargs)
FILES_TO_PACK=$(SHELL_SCRIPTS) src lib Makefile README $(TESTS)
PACK_NAME=fc359081.tgz

BINARIES=TestLatte Latte
FRONTEND_SOURCES=Assert Check Classes Context Env Errors Functions Print Pure Typecheck
BACKEND_SOURCES=Asm Compiler EmitClass EmitExpr EmitFunction EmitStmt GC Label Locals Optimize State
HS_SOURCES=Main \
	$(addprefix Backend/,$(BACKEND_SOURCES)) \
	$(addprefix Frontend/,$(FRONTEND_SOURCES))
FRONTEND_BOOT=Classes Print Typecheck
BACKEND_BOOT=EmitClass
BOOT_SOURCES= \
	$(addprefix Backend/,$(BACKEND_BOOT)) \
	$(addprefix Frontend/,$(FRONTEND_BOOT))
SOURCES = \
	$(addsuffix .hs,$(HS_SOURCES)) \
	$(addsuffix .hs-boot,$(BOOT_SOURCES))
LINKED_SOURCES=$(addprefix $(BUILD)/,$(SOURCES))
BNFC_MODULES=AbsLatte ErrM LexLatte ParLatte PrintLatte TestLatte
HPC_EXCLUDES=$(addprefix --exclude=,$(BNFC_MODULES))
BNFC_SOURCES_FILES=$(addsuffix .hs,$(BNFC_MODULES))
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean pack test testGood testBad travis

all: $(BINARIES)

test: testGood testBad

define compile_example
	e=$1; \
	echo -e "\e[93m----------- TESTING\e[96m $$e \e[93m--------------\e[0m"; \
	./latc "$$e" 2>&1 | tee "${BUILD}"/output; \
	[ "x$$(head -n 1 "${BUILD}"/output)" = "x$2" ] || exit 1
endef

define test_example
	e=$1; \
	$(call compile_example,$$e,$2); \
	if [ "xOK" = "x$2" ]; then \
		INPUT=/dev/null; \
		if [ -e $${e%.lat}.input ]; then \
		  INPUT=$${e%.lat}.input; \
		fi; \
		./$${e%.lat} < "$$INPUT" > "${BUILD}"/output 2> "${BUILD}"/val; \
		grep "definitely lost\|still reachable" "${BUILD}"/val; \
		cmp $${e%.lat}.output "${BUILD}"/output &> /dev/null || (\
		git --no-pager diff --no-index $${e%.lat}.output "${BUILD}"/output; \
		exit 1;) \
	fi
endef

define test_examples
	@for e in $1/*.lat ; do \
		$(call test_example,$$e,$2) || exit 1; \
	done
endef


testGood: Latte
	$(call test_examples,good,OK)
	$(call test_examples,good/basic,OK)
	$(call test_examples,good/input,OK)
	$(call test_examples,good/arrays,OK)
	$(call test_examples,good/struct,OK)
	$(call test_examples,good/czajka,OK)
	$(call test_examples,good/objects1,OK)
	$(call test_examples,good/objects2,OK)
	$(call test_examples,good/virtual,OK)
	$(call test_examples,good/gr5,OK)

testBad: Latte
	$(call test_examples,bad,ERROR)
	$(call test_examples,bad/semantic,ERROR)
	$(call test_examples,bad/infinite_loop,ERROR)
	$(call test_examples,bad/arrays,ERROR)
	$(call test_examples,bad/classes,ERROR)

TestLatte: $(BNFC_SOURCES) $(LINKED_SOURCES)
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) -w --make $@.hs -o ../$@

Latte: .dependencies $(BNFC_SOURCES) $(LINKED_SOURCES) lib/runtime.o
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) --make Main.hs -o ../$@

.dependencies:
	cabal update && \
	cabal install mtl==2.2.1 extra==1.5 Unique==0.4.7.1 monad-loops-0.4.3 && \
	touch .dependencies

travis: shellcheck test

$(LINKED_SOURCES): $(BUILD)/%: src/%
	ln -srf $^ -t $(BUILD)

$(BNFC_SOURCES): src/Latte.cf
	mkdir -p $(BUILD) && \
	cd $(BUILD) && \
	bnfc -haskell ../$< && \
	happy -gca ParLatte.y -iHappyOutput && \
	alex -g LexLatte.x

lib/runtime.o: lib/runtime.c
	gcc -Wall -g -c -o $@ $<

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
	find good bad -type f ! -name "*.lat" ! -name "*.input" ! -name "*.out*" | xargs rm -f && \
	rm -rf $(BUILD) $(TMP) $(BINARIES) $(TEST_DIR) *.tgz *.tix lib/runtime.o

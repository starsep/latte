BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS=-Wall
GHC=ghc

SHELL_SCRIPTS=latc latc_x86_64
TEST_DIRECTORIES=good bad extensions gr5
FILES_TO_PACK=$(SHELL_SCRIPTS) src Makefile README $(TEST_DIRECTORIES)
PACK_NAME=fc359081.tgz

BINARIES=TestLatte Latte
FRONTEND_SOURCES=Context Errors Print Typecheck Check Functions Pure Env Assert Classes
BACKEND_SOURCES=Compiler Asm EmitExpr EmitStmt State Optimize Label Locals GC
SOURCES=Main $(addprefix Backend/,$(BACKEND_SOURCES)) $(addprefix Frontend/,$(FRONTEND_SOURCES))
LINKED_SOURCES=$(addsuffix .hs,$(addprefix $(BUILD)/,$(SOURCES))) \
			   $(BUILD)/Frontend/Typecheck.hs-boot $(BUILD)/Frontend/Print.hs-boot
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
	./latc "$$e" 2>&1 | tee "${BUILD}"/output
endef

define test_example
	e=$1; \
	$(call compile_example,$$e); \
	[ "x$$(head -n 1 "${BUILD}"/output)" = "x$2" ] || exit 1;  \
	if [ "xOK" = "x$2" ]; then \
		INPUT=/dev/null; \
		if [ -e $${e%.lat}.input ]; then \
		INPUT=$${e%.lat}.input; \
		fi; \
		valgrind ./$${e%.lat} < "$$INPUT" > "${BUILD}"/output 2> "${BUILD}"/val; \
		grep "definitely lost\|still reachable" "${BUILD}"/val; \
		cmp $${e%.lat}.output "${BUILD}"/output &> /dev/null || \
		git --no-pager diff --no-index $${e%.lat}.output "${BUILD}"/output; \
	fi
endef

define test_examples
	@for e in $1/*.lat ; do \
		$(call test_example,$$e,$2); \
	done
endef


testGood: Latte
	$(call test_examples,good,OK)
	$(call test_examples,good/basic,OK)
	$(call test_examples,good/input,OK)
	$(call test_examples,good/arrays,OK)
	$(call test_examples,good/czajka,OK)

#$(call test_examples,good/struct,OK)
#$(call test_examples,good/objects1,OK)
#$(call test_examples,good/objects2,OK)
#$(call test_examples,good/virtual,OK)
#$(call test_examples,good/gr5,OK)
#$(call test_examples,good/hardcore,OK)

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
	cabal install mtl==2.2.1 extra==1.5 Unique==0.4.7.1 && \
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
	gcc -g -c -o $@ $<

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

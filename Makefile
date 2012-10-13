LLVM_SRC_DIR=../llvm-community
LLVM_DIR=$(LLVM_SRC_DIR)/build-ninja/bin
FILECHECK=$(LLVM_DIR)/FileCheck
#OPT=$(LLVM_DIR)/opt
OPT=$V/opt

V=Output

all: $V/opt

$V/opt: OptMain.hs Opt.hs ArgParser.hs
	@mkdir -p $(@D)
	ghc --make -Wall $< -outputdir=$V -o $@

clean:
	rm -rf $V

TESTS=test/Transforms/ConstProp/basictest.ll

test: $(patsubst %,$V/%.passed, $(TESTS))

$V/%.passed: $(LLVM_SRC_DIR)/% $(OPT) Makefile
	$(OPT) -constprop -S < $< | $(FILECHECK) $<
	@mkdir -p $(@D)
	touch $@


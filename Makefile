LLVM_SRC_DIR=../llvm-community
LLVM_DIR=$(LLVM_SRC_DIR)/build-ninja/bin
FILECHECK=$(LLVM_DIR)/FileCheck
#OPT=$(LLVM_DIR)/opt
OPT=$V/hopt

V=Output

all: $V/hopt test

test: $V/ConstPropTest.hs.passed $V/DeadInstructionEliminationTest.hs.passed $V/hopt

$V/hopt: HoptMain.hs Hopt.hs ArgParser.hs LlvmParser.hs LlvmPrinter.hs Block.hs OptPassUtils.hs ConstProp.hs DeadInstructionElimination.hs
	@mkdir -p $(@D)
	ghc --make -Wall $< -outputdir=$V -o $@

$V/%Test.hs.passed: %Test.hs %.hs
	runghc -Wall $<
	@touch $@

clean:
	rm -rf $V

#TESTS=test/Transforms/ConstProp/basictest.ll
#
#test: $(patsubst %,$V/%.passed, $(TESTS))
#
#$V/%.passed: $(LLVM_SRC_DIR)/% $(OPT) Makefile
#	$(OPT) -constprop -S < $< | $(FILECHECK) $<
#	@mkdir -p $(@D)
#	touch $@


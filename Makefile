BUILD_DIR     := .build
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)
LOCAL_LIB     := $(BUILD_LOCAL)/lib

DEPS_DIR         := ext
K_SUBMODULE      := $(abspath $(DEPS_DIR)/k)
PLUGIN_SUBMODULE := $(abspath $(DEPS_DIR)/blockchain-k-plugin)

K_RELEASE ?= $(K_SUBMODULE)/k-distribution/target/release/k
K_BIN     := $(K_RELEASE)/bin
K_LIB     := $(K_RELEASE)/lib
export K_RELEASE

PATH := $(K_BIN):$(PATH)
export PATH

.PHONY: all clean distclean \
        deps k-deps \
        build \
        test \

all: build

# K Dependencies
# --------------

deps	   : repo-deps
repo-deps  : k-deps plugin-deps
k-deps	   : $(K_SUBMODULE)/make.timestamp
plugin-deps: $(PLUGIN_SUBMODULE)/make.timestamp

$(K_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(K_SUBMODULE)
	cd $(K_SUBMODULE) && mvn package -DskipTests -U -Dproject.build.type=FastBuild
	touch $(K_SUBMODULE)/make.timestamp

$(PLUGIN_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(PLUGIN_SUBMODULE)
	touch $(PLUGIN_SUBMODULE)/make.timestamp

# Building
# --------

MAIN_MODULE    := MEDIK
SYNTAX_MODULE  := MEDIK-SYNTAX
export MAIN_DEFN_FILE := medik


COMMON_FILES  :=
EXTRA_K_FILES += $(MAIN_DEFN_FILE).md
ALL_K_FILES   := $(COMMON_FILES) $(EXTRA_K_FILES)

build: build-llvm

COMMON_OPTS := -w all -Wno unused-symbol

# LLVM-Build Pipeline
# ===================

# Concrete Execution
# ------------------

LLVM_EXEC_BUILD_DIR    := $(BUILD_DIR)/llvm-exec
LLVM_EXEC_KOMPILED_DIR := $(LLVM_EXEC_BUILD_DIR)/$(MAIN_DEFN_FILE)-llvm-kompiled
build-execution: $(LLVM_EXEC_KOMPILED_DIR)/make.timestamp

PLUGIN_FILES := $(PLUGIN_SUBMODULE)/plugin-c/json.cpp $(PLUGIN_SUBMODULE)/plugin-c/k.cpp
CPP_FILES    := $(PLUGIN_FILES)

LLVM_OPTS         := --hook-namespaces JSON
LLVM_KOMPILE_OPTS := $(COMMON_OPTS) $(LLVM_OPTS)
LLVM_CC_OPTS      := -L$(LOCAL_LIB) -I$(K_RELEASE)/include/kllvm \
		     -I$(PLUGIN_SUBMODULE)/plugin-c		 \
                     $(abspath $(CPP_FILES))                     \
		     -std=c++14 -Wall -g -Wno-return-type-c-linkage #TODO: Fix disabled warning

$(LLVM_EXEC_KOMPILED_DIR)/make.timestamp: $(ALL_K_FILES) $(CPP_FILES)
	mkdir -p $(LLVM_EXEC_BUILD_DIR)
	kompile -d $(LLVM_EXEC_KOMPILED_DIR)       \
	--md-selector 'k|concrete'                 \
	$(LLVM_KOMPILE_OPTS)                       \
	$(addprefix -ccopt , $(LLVM_CC_OPTS))      \
	--main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) $(MAIN_DEFN_FILE).md
	@touch $@

# Model Checking
# --------------

LLVM_MCHECK_BUILD_DIR    := $(BUILD_DIR)/llvm-mcheck
LLVM_MCHECK_KOMPILED_DIR := $(LLVM_MCHECK_BUILD_DIR)/$(MAIN_DEFN_FILE)-llvm-kompiled
build-model-check: $(LLVM_MCHECK_KOMPILED_DIR)/make.timestamp

LLVM_MCHECK_OPTS := $(COMMON_OPTS) --enable-search \

$(LLVM_MCHECK_KOMPILED_DIR)/make.timestamp: $(ALL_K_FILES)
	mkdir -p $(LLVM_MCHECK_BUILD_DIR)
	kompile -d $(LLVM_MCHECK_KOMPILED_DIR)     \
	--md-selector 'k|mcheck'                   \
	$(LLVM_MCHECK_OPTS)                        \
	--main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) $(MAIN_DEFN_FILE).md
	@touch $@

# Haskell-Build Pipeline
# ----------------------

HASKELL_BUILD_DIR    := $(BUILD_DIR)/haskell
HASKELL_KOMPILED_DIR := $(HASKELL_BUILD_DIR)/$(MAIN_DEFN_FILE)-haskell-kompiled

build-haskell: $(HASKELL_KOMPILED_DIR)/make.timestamp

$(HASKELL_KOMPILED_DIR)/make.timestamp: $(ALL_K_FILES)
	mkdir -p $(HASKELL_BUILD_DIR)
	kompile -d $(HASKELL_KOMPILED_DIR)         \
	--backend haskell			   \
	--md-selector 'k|symbolic'                 \
	$(HASKELL_KOMPILE_OPTS)                    \
	--main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) $(MAIN_DEFN_FILE).md
	@touch $@

# Tests
# -----

COMPARE := git --no-pager diff --no-index --ignore-all-space -R
GREEN   := \033[0;32m
RESET   := \033[0m

# Regular Medik Tests
# -------------------
TEST_EXECUTION_FILES := $(wildcard tests/execution/*.medik)

tests-execution: $(patsubst tests/execution/%.medik, tests/execution/%.medik.run, $(TEST_EXECUTION_FILES))

tests/execution/%.medik.run: tests/execution/%.medik tests/execution/%.medik.expected $(LLVM_EXEC_KOMPILED_DIR)/make.timestamp
	@printf '%-45s %s' "$< " "... "
	@if [ -f tests/execution/$*.medik.in ]; then 		 \
		./medik -in tests/execution/$*.medik.in $< > $@; \
		else ./medik $< > $@; fi
	@$(COMPARE) $@ $(word 2, $^)
	@printf "${GREEN}OK ${RESET}\n"

# Cleaning
# --------

clean:
	rm -rf .build tests/*.tmp


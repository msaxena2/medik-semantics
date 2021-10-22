BUILD_DIR     := .build
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)
LOCAL_LIB     := $(BUILD_LOCAL)/lib

DEPS_DIR         := ext
K_SUBMODULE      := $(abspath $(DEPS_DIR)/k)

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

deps: repo-deps
repo-deps: k-deps
k-deps: $(K_SUBMODULE)/make.timestamp

$(K_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(K_SUBMODULE)
	cd $(K_SUBMODULE) && mvn package -DskipTests -U -Dproject.build.type=FastBuild -Dhaskell.backend.skip
	touch $(K_SUBMODULE)/make.timestamp

# Building
# --------

MAIN_MODULE    := MEDIK
SYNTAX_MODULE  := MEDIK-SYNTAX
export MAIN_DEFN_FILE := medik


COMMON_FILES  :=
EXTRA_K_FILES += $(MAIN_DEFN_FILE).md
ALL_K_FILES   := $(COMMON_FILES) $(EXTRA_K_FILES)

build: build-llvm

# LLVM-Build Pipeline
# -------------------

LLVM_KOMPILED_DIR := $(BUILD_DIR)/$(MAIN_DEFN_FILE)-kompiled
build-llvm: $(LLVM_KOMPILED_DIR)/make.timestamp

KOMPILE_OPTS := --hook-namespaces TIMER -w all -Wno unused-symbol
CPP_FILES := hooks/timer.cpp

LLVM_KOMPILE_OPTS := -L$(LOCAL_LIB) -I$(K_RELEASE)/include/kllvm \
                     $(abspath $(CPP_FILES))                     \
                     -std=c++14 -levent -Wall -g


$(LLVM_KOMPILED_DIR)/make.timestamp: $(ALL_K_FILES) $(CPP_FILES)
	mkdir -p $(BUILD_DIR)
	kompile -d $(LLVM_KOMPILED_DIR)				  \
	$(KOMPILE_OPTS) $(addprefix -ccopt , $(LLVM_KOMPILE_OPTS)) \
	--main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) $(MAIN_DEFN_FILE).md
	@touch $@

# Tests
# -----

TEST_LLVM_FILES := $(wildcard tests/*.medik)
tests-llvm: $(patsubst tests/%.medik, tests/%.medik.run, $(TEST_LLVM_FILES))

COMPARE := git --no-pager diff --no-index --ignore-all-space -R
PROCESS_OUT := "./tests/processOut"

GREEN := \033[0;32m
RESET := \033[0m
tests/%.medik.run: tests/%.medik tests/%.medik.expected $(LLVM_KOMPILED_DIR)/make.timestamp
	@printf '%-35s %s' "$< " "... "
	@krun --output none -d $(LLVM_KOMPILED_DIR) $< > $@
	@$(COMPARE) $@ $(word 2, $^)
	@printf "${GREEN}OK ${RESET}\n"


# Cleaning
# --------

clean:
	rm -rf .build


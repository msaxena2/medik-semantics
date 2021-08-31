CURDIR        := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BUILD_DIR     := $(CURDIR)/.build
TESTS_DIR     := $(CURDIR)/t
DEFN_DIR      := $(BUILD_DIR)/defn
SRC_DIR       := $(CURDIR)
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)

LIBRARY_PATH       := $(BUILD_LOCAL)/lib
C_INCLUDE_PATH     += :$(BUILD_LOCAL)/include
CPLUS_INCLUDE_PATH += :$(BUILD_LOCAL)/include
PKG_CONFIG_PATH    := $(LIBRARY_PATH)/pkgconfig

export LIBRARY_PATH
export C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH
export PKG_CONFIG_PATH

INSTALL_PREFIX := /usr/local
INSTALL_DIR    ?= $(DESTDIR)$(INSTALL_PREFIX)/bin

DEPS_DIR         := ext
K_SUBMODULE      := $(abspath $(DEPS_DIR)/k)
PLUGIN_SUBMODULE := $(abspath $(DEPS_DIR)/blockchain-k-plugin)
export PLUGIN_SUBMODULE

K_RELEASE ?= $(K_SUBMODULE)/k-distribution/target/release/k
K_BIN     := $(K_RELEASE)/bin
K_LIB     := $(K_RELEASE)/lib
export K_RELEASE

PATH := $(K_BIN):$(PATH)
export PATH

.PHONY: all clean distclean \
        deps k-deps plugin-deps \
        build build-llvm build-rpc \
        defn rpc-defn llvm-defn  \
        test-all test-rpc \

all: build

clean:
	rm -rf $(DEFN_BASE_DIR)

distclean:
	rm -rf $(BUILD_DIR)
	git clean -dffx -- tests/

# K Dependencies
# --------------

deps: repo-deps
repo-deps: k-deps plugin-deps
k-deps: $(K_SUBMODULE)/make.timestamp
plugin-deps: $(PLUGIN_SUBMODULE)/make.timestamp

$(K_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(K_SUBMODULE)
	cd $(K_SUBMODULE) && mvn package -DskipTests -U -Dproject.build.type=FastBuild -Dhaskell.backend.skip
	touch $(K_SUBMODULE)/make.timestamp

$(PLUGIN_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(PLUGIN_SUBMODULE)
	touch $(PLUGIN_SUBMODULE)/make.timestamp

# Building
# --------

MAIN_MODULE    := MEDIK
SYNTAX_MODULE  := MEDIK-SYNTAX
export MAIN_DEFN_FILE := medik


COMMON_FILES  := core.md jsonRpc.md kRpc.md commons.md
EXTRA_K_FILES += $(MAIN_DEFN_FILE).md
ALL_K_FILES   := $(COMMON_FILES) $(EXTRA_K_FILES)


# RPC Pipeline
# ------------

rpc_dir      := $(BUILD_DIR)/rpc
rpc_files    := $(patsubst %, $(rpc_dir)/%, $(ALL_K_FILES))
rpc_kore     := $(rpc_dir)/$(MAIN_DEFN_FILE)-kompiled/definition.kore
rpc_kompiled := $(rpc_dir)/server

export rpc_kore
export rpc_dir

$(rpc_dir)/%.md: $(SRC_DIR)/%.md
	@mkdir -p $(rpc_dir)
	@cp $< $@

$(rpc_kore): $(rpc_files)
	kompile --debug --main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) --backend llvm \
	        $(rpc_dir)/$(MAIN_DEFN_FILE).md \
	        --directory $(rpc_dir) -I $(rpc_dir) \
	        --hook-namespaces "JSON K-RPC" --no-llvm-kompile \
		$(KOMPILE_OPTS)

$(rpc_kompiled): $(rpc_kore)
	echo "$(K_RELEASE)"
	@mkdir -p $(rpc_dir)/build
	cd $(rpc_dir)/build && cmake $(CURDIR)/cmake/client -DKOMPILED_DIR=$(rpc_dir)/medik-kompiled -DCMAKE_BUILD_TYPE=DEBUG && $(MAKE)

build-rpc: $(rpc_kompiled)

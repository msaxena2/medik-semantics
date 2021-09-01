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

INSTALL_PREFIX := $(BUILD_LOCAL)

DEPS_DIR         := ext
K_SUBMODULE      := $(abspath $(DEPS_DIR)/k)
PLUGIN_SUBMODULE := $(abspath $(DEPS_DIR)/blockchain-k-plugin)
export PLUGIN_SUBMODULE

# set OS specific defaults
ifeq ($(shell uname -s),Darwin)
# 1. OSX doesn't have /proc/ filesystem
# 2. fix cmake openssl detection for brew
LIBFF_CMAKE_FLAGS += -DWITH_PROCPS=OFF \
                     -DOPENSSL_ROOT_DIR=$(shell brew --prefix openssl)
else
# llvm-backend code doesn't play nice with g++
export CXX := $(if $(findstring default, $(origin CXX)), clang++, $(CXX))
endif

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
plugin-deps: $(PLUGIN_SUBMODULE)/make.timestamp libff

$(K_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(K_SUBMODULE)
	cd $(K_SUBMODULE) && mvn package -DskipTests -U -Dproject.build.type=FastBuild -Dhaskell.backend.skip
	touch $(K_SUBMODULE)/make.timestamp

$(PLUGIN_SUBMODULE)/make.timestamp:
	git submodule update --init --recursive -- $(PLUGIN_SUBMODULE)
	touch $(PLUGIN_SUBMODULE)/make.timestamp


# Plugin Dependencies
# -------------------

LIBFF_BUILD_DIR := $(PLUGIN_SUBMODULE)/deps/libff/build
libff: $(BUILD_DIR)/lib/libff.a

$(BUILD_DIR)/lib/libff.a: $(PLUGIN_SUBMODULE)/make.timestamp
	@mkdir -p $(LIBFF_BUILD_DIR)
	cd $(LIBFF_BUILD_DIR) \ && cmake .. -DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) $(LIBFF_CMAKE_FLAGS)\
	  && $(MAKE)                                           	             \
	  && $(MAKE) install


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

KOMPILE_INCLUDE_DIRS := "-I $(CURDIR) -I $(K_LIB) -I $(BUILD_DIR)/rpc/build"
KOMPILE_FLAGS += --debug --backend llvm --no-llvm-kompile --hook-namespaces "KRYPTO JSON K-RPC" $(KOMPILE_INCLUDE_DIRS)

rpc_dir      := $(BUILD_DIR)/rpc
rpc_files    := $(patsubst %, $(rpc_dir)/%, $(ALL_K_FILES))
rpc_kore     := $(rpc_dir)/$(MAIN_DEFN_FILE)-kompiled/definition.kore
rpc_kompiled := $(rpc_dir)/server

export rpc_dir

$(rpc_dir)/%.md: $(SRC_DIR)/%.md
	@mkdir -p $(rpc_dir)
	@cp $< $@

$(rpc_kore): $(rpc_files)
	kompile --main-module $(MAIN_MODULE) --syntax-module $(SYNTAX_MODULE) \
	        $(rpc_dir)/$(MAIN_DEFN_FILE).md \
	        --directory $(rpc_dir) -I $(rpc_dir) \
		$(KOMPILE_FLAGS)

$(rpc_kompiled): $(rpc_kore)
	echo "$(K_RELEASE)"
	@mkdir -p $(rpc_dir)/build
	cd $(rpc_dir)/build && cmake $(CURDIR)/cmake/client -DKOMPILED_DIR=$(rpc_dir)/medik-kompiled -DCMAKE_BUILD_TYPE=Debug && $(MAKE)

build-rpc: $(rpc_kompiled)

cmake_minimum_required (VERSION 3.4)

list(APPEND CMAKE_MODULE_PATH "$ENV{K_RELEASE}/cmake")
include(LLVMKompilePrelude)
project (KevmClient CXX)

set(KOMPILED_DIR $ENV{rpc_dir}/$ENV{MAIN_DEFN_FILE}-kompiled)
set(KOMPILE_USE_MAIN "library")
set(TARGET_NAME "rpc-server")

add_executable(rpc-server
	$ENV{PLUGIN_SUBMODULE}/client-c/init.cpp
	$ENV{PLUGIN_SUBMODULE}/client-c/json.cpp
        ${CMAKE_SOURCE_DIR}/main.cpp
)

if(UNIX AND NOT APPLE)
	set(LINK_PROCPS procps)
endif()

target_include_directories(rpc-server
	PUBLIC $ENV{PLUGIN_SUBMODULE}/plugin-c
	PUBLIC $ENV{PLUGIN_SUBMODULE}/client-c
	PUBLIC $ENV{PLUGIN_SUBMODULE}/deps/cpp-httplib
	PUBLIC ${CMAKE_SOURCE_DIR}/..)
target_compile_options(rpc-server
	PUBLIC $ENV{LLVM_KOMPILE_OPTS}
	PUBLIC -Wall -Werror -Wno-unknown-warning-option)
target_link_libraries(rpc-server
	ff gmp ${LINK_PROCPS} gflags
)

include(LLVMKompile)

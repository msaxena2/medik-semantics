K Semantics of Medik
====================

Medik is a language for developing medical guidance systems by expressing
them as finite state machines


Building
--------

Use `make deps && make tests-llvm` to run the tests.

Useful targets:

 - `make deps` fetches relevant K submodules.
 - `make defn-llvm` kompiles the medik definition.
 - `make tests-llvm` runs tests.



K Semantics of Medik
====================

Medik is a language for developing medical guidance systems by expressing
them as finite state machines. For more information on the motivation, background work,
and underlying design principles behind MediK, please refer to this
[recorded talk](https://www.youtube.com/watch?v=s9kRwXchORs) on youtube.

Building
--------

Use `make deps && make tests-llvm` to run the tests.

Useful targets:

 - `make deps` fetches relevant K submodules.
 - `make defn-llvm` kompiles the medik definition.
 - `make tests-llvm` runs tests.



Common Infrastructure
=====================

```k
requires "./machines.md"

module MACHINE-COMMONS
    imports MACHINE-UTILS

    syntax State ::= "idle"

    rule stateToString(idle) => "idle"

    syntax ActionName ::= "noop"

    rule actionNameToString(noop) => "noop"

endmodule

```

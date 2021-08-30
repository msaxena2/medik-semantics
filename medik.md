RPC Runner For MediK Programs
=============================

```k
requires "./core.md"
requires "./kRpc.md"

module MEDIK-SYNTAX
  imports MEDIK-CORE-SYNTAX

endmodule

module MEDIK
  imports K-RPC
  imports MEDIK-CORE

  configuration <medik/>
                <k-rpc/>

endmodule

```

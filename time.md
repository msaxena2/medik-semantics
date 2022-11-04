Time
====

We define certain notions dealing with time. We model time
as the obvious commutative monoid.

```k
module TIME-SYNTAX
  imports BOOL-SYNTAX

  syntax Bool ::= Time "==Time" Time [function, strict]

  syntax Time ::= Time "+Time" Time  [function, strict]
endmodule

module TIME
  imports BOOL
  imports TIME-SYNTAX

  rule T ==Time  T  => true
  rule T ==Time  T' => false  [owise]

  rule T =/=Time T' => notBool (T ==Time T')
endmodule
```

The `LINEAR-TIME` module forces a `linear` order on time.

```k
module LINEAR-TIME-SYNTAX
  imports BOOL-SYNTAX
  imports TIME-SYNTAX

  syntax Bool ::= Time  ">Time" Time [strict, function]
                | Time  "<Time" Time [strict, function]
                | Time ">=Time" Time [strict, function]
                | Time ">=Time" Time [strict, function]

endmodule

module LINEAR-TIME
  imports LINEAR-TIME-SYNTAX

endmodule
```

We model discretized time using a model of natural numbers

```k
module INT-TIME-SYNTAX
  imports LINEAR-TIME-SYNTAX
  imports INT-SYNTAX

  syntax Time ::= Int
endmodule

module INT-TIME
  imports INT-TIME-SYNTAX

  rule T1:Int  <Time T2:Int => T1  <Int T2
  rule T1:Int <=Time T2:Int => T1 <=Int T2
  rule T1:Int  >Time T2:Int => T1  >Int T2
  rule T1:Int >=Time T2:Int => T1 >=Int T2

  rule T1:Int +Time T2:Int => T1 +Int T2
endmodule
```

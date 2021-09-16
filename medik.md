Medik Semantics
===============

Syntax
------

```k
module MEDIK-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ","}
  syntax Val ::= Int | Bool | "undef"

  syntax Exp ::= Id
               | Val
               | Exp "+" Exp          [strict]
               | Exp "-" Exp          [strict]
               | Exp "*" Exp          [strict]
               | Exp "/" Exp          [strict]
               | "(" Exp ")"          [bracket]
               > Exp "=" Exp          [strict(2)]
               | "print" "(" Exp ")"  [strict]
               | "var" Id
               | "vars" Ids

  syntax Stmt ::= Exp ";"            [strict]
                | "entry" Block
                | "state" Id Block
                | "machine" Id Block
                > Stmt Stmt          [right]

  syntax Block ::= "{" "}"
                 | "{" Stmt "}"

endmodule
```

Semantics
---------

```k
module MEDIK
  imports MEDIK-SYNTAX
  imports DOMAINS

  syntax Val ::= "mt"
  syntax KResult ::= Val

  syntax KItem ::= "createMainMachine"

  configuration <machines>
                  <machine multiplicity="*" type="Map">
                    <name> . </name>
                    <k> createMainMachine ~> { $PGM:Stmt } </k>
                    <env> .Map </env>
                  </machine>
                </machines>
                <store> .Map </store>
                <nextLoc> 0 </nextLoc>
                <output stream="stdout"> .List </output>
```
### Macros

```k
  rule S:Stmt Ss:Stmt => S ~> Ss                                [structural]
  rule { S:Stmt } => S                                          [structural]
  rule vars I1::Id, I2::Id, Is::Ids; => var I1 ;  vars I2,Is;   [structural]
```

### Machine creation

```k
  syntax Id ::= "Main"
  rule <k> createMainMachine => . ... </k>
       <name> _ => Main </name>


  rule <machine> <k> (machine Name Code => .) ... </k> ... </machine>
       ( .Bag =>  <machine>
                    <name> Name </name>
                    <k> Code </k>
                    ...
                </machine> )
```
### Expression and Statement

#### Variable Assignment and Lookup
```k
  rule var I:Id = V:Val => var I; ~> I = V

  rule <k> var Id => Loc ... </k>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule _:Val; => .

  rule <k> I:Id => V ... </k>
       <env> (I |-> Pointer) ... </env>
       <store> (Pointer |-> V) ... </store>

  rule <k> I:Id = V:Val => V ... </k>
       <env> (I |-> Loc) ... </env>
       <store> Store => Store[Loc <- V] </store>
```

#### Arithmetic Expressions
```k
  rule I1:Int + I2:Int => I1 +Int I2
  rule I1:Int - I2:Int => I1 -Int I2
  rule I1:Int * I2:Int => I1 *Int I2
  rule I1:Int / I2:Int => I1 /Int I2
    requires I2 =/=K 0
  rule _:Int / 0 => undef
```

#### Other Operations
```k
  rule <k> print(V:Val) => V ... </k>
       <output> ... (.List => ListItem(V)) </output>
    requires V =/=K undef

  rule <k> print(undef) => undef ... </k>
       <output> ... (.List => ListItem("undef")) </output>

endmodule
```

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
                > "init" Stmt
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

  syntax KResult ::= Val

  syntax KItem ::= "createMainMachine"

  configuration <machines>
                  <machine multiplicity="*" type="Map">
                    <machineName> . </machineName>
                    <k> createMainMachine ~> { $PGM:Stmt } </k>
                    <activeState> undef </activeState>
                    <machineEnv> .Map </machineEnv>
                    <states>
                      <state multiplicity="*" type="Map">
                        <stateName> . </stateName>
                        <entryBlock> . </entryBlock>
                        <exitBlock> . </exitBlock>
                      </state>
                    </states>
                  </machine>
                </machines>
                <env> .Map </env>
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
       <machineName> _ => Main </machineName>


  rule <machine> <k> (machine Name Code => .) ... </k> ... </machine>
       ( .Bag =>  <machine>
                    <machineName> Name </machineName>
                    <k> Code </k>
                    ...
                </machine> )
```
### Expression and Statement

#### Variable Assignment and Lookup
```k
  rule var I:Id = V:Val => var I; ~> I = V

  rule <k> var Id => Loc ... </k>
       <activeState> undef </activeState>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule <k> var Id => Loc ... </k>
       <activeState> S </activeState>
       <machineEnv> Rho => Rho[Id <- Loc] </machineEnv>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
    requires S =/=K undef

  rule _:Val; => .

  rule <k> I:Id => V ... </k>
       <machineEnv> (I |-> Pointer) ... </machineEnv>
       <store> (Pointer |-> V) ... </store>

  rule <k> I:Id => V ... </k>
       <machineEnv> Rho </machineEnv>
       <env> (I |-> Pointer) ... </env>
       <store> (Pointer |-> V) ... </store>
    requires notBool (I in keys(Rho))


  rule <k> I:Id = V:Val => V ... </k>
       <machineEnv> (I |-> Loc) ... </machineEnv>
       <store> Store => Store[Loc <- V] </store>

  rule <k> I:Id = V:Val => V ... </k>
       <machineEnv> Rho </machineEnv>
       <env> (I |-> Loc) ... </env>
       <store> Store => Store[Loc <- V] </store>
    requires notBool (I in keys(Rho))

  rule <k> I:Id = V:Val => V ... </k>
       <machineEnv> Rho </machineEnv>
       <env> (I |-> Loc) ...  </env>
       <store> Store => Store[Loc <- V] </store>
    requires notBool (I in keys(Rho))
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

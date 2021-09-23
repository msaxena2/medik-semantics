Medik Semantics
===============

Syntax
------

```k
module MEDIK-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ","}
  syntax Exps ::= List{Exp, ","}

  syntax Val ::= Int | Bool | "undef"

  syntax Exp ::= Id
               | Val
               | Exp "+" Exp           [strict]
               | Exp "-" Exp           [strict]
               | Exp "*" Exp           [strict]
               | Exp "/" Exp           [strict]
               | "(" Exp ")"           [bracket]
               | Id "(" Exps ")"       [strict(2)]
               | "new" Id "(" Exps ")" [strict(2)]
               > Exp "=" Exp           [strict(2)]
               | "print" "(" Exp ")"   [strict]
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

  syntax KItem ::= "createMachineTemplates" "(" Stmt ")"

  syntax Id ::= "$Main"

  configuration <instances>
                  <instance mulitplicity="*" type="Map">
                    <id> 0 </id>
                    <k> createMachineTemplates($PGM:Stmt) </k>
                    <env> .Map </env>
                    <class> $Main </class>
                    <activeState> undef </activeState>
                  </instance>
                </instances>
                <machines>
                  <machine multiplicity="*" type="Map">
                    <machineName> $Main </machineName>
                    <code> . </code>
                    <isInitMachine> false </isInitMachine>
                    <states>
                      <state multiplicity="*" type="Map">
                        <stateName> . </stateName>
                        <entryBlock> . </entryBlock>
                        <isInitState> false </isInitState>
                        <exitBlock> . </exitBlock>
                      </state>
                    </states>
                  </machine>
                </machines>
                <store> .Map </store>
                <nextLoc> 0 </nextLoc>
                <output stream="stdout"> .List </output>
```
### Macros

```k
  rule { S:Stmt } => S                                          [structural]
  rule vars I1::Id, I2::Id, Is::Ids; => var I1 ;  vars I2,Is;   [structural]
```

### Machine Template Creation

```k
  rule createMachineTemplates(S Ss) => createMachineTemplates(S) ~> createMachineTemplates(Ss)
  rule <k> createMachineTemplates(machine Name Code) => . ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName>
                      <code> Code </code> ...
                    </machine> ) ...
       </machines>

  rule <k> createMachineTemplates(init machine Name Code) => . ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName>
                      <isInitMachine> true </isInitMachine>
                      <code> Code </code> ...
                    </machine> ) ...
       </machines>

```
### Expression and Statement

#### Variable Assignment and Lookup
```k
  rule var I:Id = V:Val => var I; ~> I = V
  rule S:Stmt Ss:Stmt => S ~> Ss

  rule <k> var Id => Loc ... </k>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule <k> var Id => Loc ... </k>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule _:Val; => .

  rule <k> I:Id => V ... </k>
       <env> (I |-> Loc) ... </env>
       <store> (Loc |-> V) ... </store>


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

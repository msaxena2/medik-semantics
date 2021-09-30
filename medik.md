Medik Semantics
===============

Syntax
------

```k
module MEDIK-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ","}
  syntax Exps ::= List{Exp, ","} | Vals

  syntax Val ::= Int | Bool | "undef"
  syntax Vals ::= List{Val, ","}

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
               | DeclExp

  syntax DeclExp ::= "var" Id
                   | "vars" Ids

  syntax Stmt ::= Exp ";"                      [strict]
                | Block
                | "entry" Block
                | "entry" "(" Ids ")" Block
                | "on" Id "do" Block
                | "on" Id "(" Ids ")" "do" Block
                | StateDecl
                > "machine" Id Block
                | "init" "machine" Id Block
                > Stmt Stmt                    [right]

  syntax StateDecl ::= "state" Id Block
                     | "init" "state" Id Block

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

  syntax KResult ::= Val | Vals

  syntax KItem ::= "createMachineTemplates" "(" Stmt ")"
  syntax Id ::= "$Main"

  configuration <instances>
                  <instance multiplicity="*" type="Map">
                    <id> 0 </id>
                    <k> createMachineTemplates($PGM:Stmt) ~> createInitInstance </k>
                    <env> .Map </env>
                    <class> $Main </class>
                    <activeState> . </activeState>
                  </instance>
                </instances>
                <machines>
                  <machine multiplicity="*" type="Map">
                    <machineName> $Main </machineName>
                    <declarationCode> . </declarationCode>
                    <isInitMachine> false </isInitMachine>
                    <states>
                      <state multiplicity="*" type="Map">
                        <stateName> . </stateName>
                        <stateDeclarations> . </stateDeclarations>
                        <entryBlock> . </entryBlock>
                        <args> . </args>
                        <isInitState> false </isInitState>
                        <exitBlock> . </exitBlock>
                        <eventHandlers>
                          <eventHandler multiplicity="*" type="Set">
                            <eventId> . </eventId>
                            <eventArgs> . </eventArgs>
                            <handlerCode> . </handlerCode>
                          </eventHandler>
                        </eventHandlers>
                      </state>
                    </states>
                  </machine>
                </machines>
                <stack> .List </stack>
                <store> .Map </store>
                <nextLoc> 1 </nextLoc>
                <output stream="stdout"> .List </output>
```
### Macros

```k
  rule { S:Stmt } => S                                          [structural]
  rule vars I1::Id, I2::Id, Is::Ids; => var I1 ;  vars I2, Is;  [macro]
  rule vars I::Id , .Ids ; => var I;                            [macro]
  rule entry B:Block => entry (.Ids) B                          [macro]
  rule on E:Id do B:Block => on E (.Ids) do B                   [macro]
```

### Machine Template Creation

```k

  syntax KItem ::= "createTransitionSystem"  "(" machineName: Id "," code: Stmt ")"
                 | "createDeclarationCode"   "(" machineName: Id "," code: Stmt ")"
                 | "createEventHandlers"     "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createStateDeclarations" "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createEntryBlock"        "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createInstance"          "(" machineName: Id ")"
                 | "createInitInstance"

  rule createMachineTemplates(S Ss) => createMachineTemplates(S) ~> createMachineTemplates(Ss)
  rule <k> createMachineTemplates(machine Name ({ Code } #as CodeBlock:Block))
        => createDeclarationCode(Name, Code) ~> createTransitionSystem(Name, CodeBlock) ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName> ...
                    </machine> ) ...
       </machines>

  rule <k> createMachineTemplates(init machine Name ({ Code } #as CodeBlock))
        => createDeclarationCode(Name, Code) ~> createTransitionSystem(Name, CodeBlock) ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName>
                      <isInitMachine> true </isInitMachine> ...
                    </machine> ) ...
       </machines>

  rule createDeclarationCode(Name, S Ss)
    => createDeclarationCode(Name, S) ~> createDeclarationCode(Name, Ss)

  rule <k> createDeclarationCode(Name, E::DeclExp ;) => . ... </k>
       <machine>
        <machineName> Name </machineName>
        <declarationCode> . => { E ; }:>Stmt </declarationCode> ...
       </machine>

  rule createDeclarationCode(_, _) => . [owise]


  rule <k> createDeclarationCode(Name, E::DeclExp ;) => . ... </k>
       <machine>
        <machineName> Name </machineName>
        <declarationCode> S:Stmt => { S E; }:>Stmt </declarationCode> ...
       </machine>

  rule createTransitionSystem(MName, S Ss)
    => createTransitionSystem(MName, S) ~> createTransitionSystem(MName, Ss)

  rule createTransitionSystem(MName, { B }) => createTransitionSystem(MName, B)

  rule <k> createTransitionSystem(MName, state SName:Id ({ Code } #as CodeBlock:Block))
        =>   createStateDeclarations(MName, SName, Code)
          ~> createEntryBlock(MName, SName, CodeBlock)
          ~> createEventHandlers(MName, SName, CodeBlock) ...
       </k>
       <machine>
        <machineName> MName </machineName>
        <states> ( .Bag => <state>
                            <stateName> SName </stateName> ...
                           </state> ) ...
        </states> ...
       </machine>

  rule <k> createTransitionSystem(MName, init state SName:Id ({ Code } #as CodeBlock:Block))
        =>   createStateDeclarations(MName, SName, Code)
          ~> createEntryBlock(MName, SName, CodeBlock)
          ~> createEventHandlers(MName, SName, CodeBlock) ...
       </k>
       <machine>
        <machineName> MName </machineName>
        <states> ( .Bag => <state>
                            <stateName> SName </stateName>
                            <isInitState> true </isInitState> ...
                           </state> ) ...
        </states> ...
       </machine>

  rule createTransitionSystem(_, _) => . [owise]

  rule createStateDeclarations(MName, SName, S Ss)
    => createStateDeclarations(MName, SName, S) ~> createStateDeclarations(MName, SName, Ss)

  rule <k> createStateDeclarations(MName, SName, E::DeclExp ;) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S:Stmt => S E; </stateDeclarations> ...
        </state> ...
      </machine>

  rule <k> createStateDeclarations(MName, SName, E::DeclExp ;) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> . => E; </stateDeclarations> ...
        </state> ...
      </machine>

  rule createStateDeclarations(_,_,_) => . [owise]

  rule <k> createEntryBlock(MName, SName, entry ( Args:Ids ) Code:Block) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <entryBlock> _ => Code </entryBlock>
          <args> _ => Args </args> ...
        </state> ...
       </machine>

  rule createEntryBlock(_, _, { B } => B)
  rule createEntryBlock(MName, SName, S Ss)
    => createEntryBlock(MName, SName, S) ~> createEntryBlock(MName, SName, Ss)
  rule createEntryBlock(_, _, _) => .     [owise]


  rule <k> createEventHandlers(MName, SName, on EName ( Args:Ids ) do Code:Block) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <eventHandlers>
            (.Bag => <eventHandler>
                      <eventId> EName </eventId>
                      <eventArgs> Args </eventArgs>
                      <handlerCode> Code </handlerCode> ...
                     </eventHandler> ) ...
          </eventHandlers> ...
       </state> ...
      </machine>

  rule createEventHandlers(_, _, { B } => B)
  rule createEventHandlers(MName, SName, S Ss)
    => createEventHandlers(MName, SName, S) ~> createEventHandlers(MName, SName, Ss)

  rule createEventHandlers(_, _, _) => . [owise]

  rule <k> createInitInstance => new InitMName ( .Vals ); </k>
       <machine>
        <machineName> InitMName </machineName>
        <isInitMachine> true </isInitMachine> ...
       </machine>

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

#### Instance creation via new

```k
  syntax KItem ::= "enterState" "(" stateName: Id "," entryArgs: Vals ")"
                 | "instance" "(" instanceId: Int ")"
                 | "recordEnv"
                 | "restoreEnv"
                 | "dequeueEvent"
                 | "execEntryBlock" "(" Vals ")"

  rule  <instance>
          <k> new MName ( Args ) => Loc ... </k>
          ...
        </instance>
        ( .Bag => <instance>
                    <id> Loc </id>
                    <k> MachineDecls ~> enterState(InitState, Args) </k>
                    <class> MName </class> ...
                   </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> MachineDecls </declarationCode>
        <state>
          <stateName> InitState </stateName>
          <isInitState> true </isInitState> ...
        </state> ...
       </machine>

  rule <k> enterState(SName, Vals)
        =>   recordEnv
          ~> StateDecls
          ~> execEntryBlock(Vals)
          ~> dequeueEvent
          ~> restoreEnv ...
      </k>
      <env> _ </env>
      <class> Class </class>
      <activeState> _ => SName </activeState>
      <machine>
        <machineName> Class </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> StateDecls </stateDeclarations> ...
        </state>
        ...
      </machine>

  rule <k> recordEnv => . ... </k>
       <env> Rho </env>
       <stack> .List => ListItem(Rho) ... </stack>

  syntax KItem ::= "assign" "(" args: Ids "|" values: Vals ")"

  rule <k> execEntryBlock(Vals) => assign(Args | Vals) ~> EntryCode ... </k>
       <class> MName </class>
       <activeState> ActiveState </activeState>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> ActiveState </stateName>
          <entryBlock> EntryCode </entryBlock>
          <args> Args </args> ...
        </state> ...
      </machine>

  rule <k> assign(I:Id , Is | V:Val, Vs) => assign(Is | Vs) ... </k>
       <env> .Map => (I |-> V) ... </env>

  rule assign( .Ids | .Vals ) => .

  rule <k> restoreEnv => . ... </k>
       <env> _ => Rho </env>
       <stack> (ListItem(Rho) => .List ) ... </stack>
```
#### Event Handling

```k
  rule <k> dequeueEvent => . ... </k>
       <class> MName </class>
       <activeState> State </activeState>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> State </stateName>
          <eventHandlers> .Bag </eventHandlers> ...
        </state> ...
      </machine>

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

Medik Semantics
===============

Syntax
------

```k
requires "json.md"

module MEDIK-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ","}
  syntax Exps ::= List{Exp, ","}  [strict, klabel(exps)]
  syntax UndefExp ::= "undef"

  syntax Val
  syntax Vals ::= List{Val, ","}  [klabel(exps)]

  syntax ThisExp ::= "this"

  syntax Exp ::= Id
               | Int
               | Bool
               | String
               | UndefExp
               | ThisExp
               | Exp "+" Exp                        [strict]
               | Exp "-" Exp                        [strict]
               | Exp "*" Exp                        [strict]
               | Exp "/" Exp                        [strict]
               | Exp "." Exp                        [strict(1), left]
               | "(" Exp ")"                        [bracket]
               | Id "(" Exps ")"                    [strict(2)]
               | "new" Id "(" Exps ")"              [strict(2)]
               | "send" Exp "," Id                  [strict(1)]
               | "send" Exp "," Id "," "(" Exps ")" [strict(1)]
               > Exp "=" Exp                        [strict(2)]
               | "print" "(" Exp ")"                [strict]
               | DeclExp
               | "extern" Id "(" Exps ")"

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
  imports JSON
  imports K-REFLECTION

  syntax Val ::= Int | Bool | String | UndefExp
  syntax Exp ::= Val
  syntax Exps ::= Vals

  syntax KResult ::= Val | Vals

  syntax KItem ::= "createMachineTemplates" "(" Stmt ")"

  syntax Id ::= "$Main"

  configuration <instances>
                  <instance multiplicity="*" type="Map">
                    <id> 0 </id>
                    <k> createMachineTemplates($PGM:Stmt) ~> createInitInstance </k>
                    <env> .Map </env>
                    <genv> .Map </genv>
                    <class> $Main </class>
                    <stack> .List </stack>
                    <inBuffer> .List </inBuffer>
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
  rule send Id , Event => send Id, Event, ( .Vals )             [macro]
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

  syntax Exp ::= "var" Exp "." Id [strict(1)]

  rule <k> this => instance(Id) ... </k>
       <id> Id </id>

  rule <k> var instance(Id) . Field => Loc ... </k>
       <id> Id </id>
       <genv> GRho => GRho[Field <- Loc] </genv>
       <store> (.Map => (Loc |-> undef)) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule <k> var Id => Loc ... </k>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule _:Val; => .

  rule <k> I:Id => V ... </k>
       <env> (I |-> Loc) ... </env>
       <store> (Loc |-> V) ... </store>

  rule <k> I:Id => this . I ... </k>
       <env> Rho </env>
       <genv> GRho </genv>
    requires (notBool(I in keys(Rho))) andBool (I in keys(GRho))

  rule <k> I:Id = V:Val => V ... </k>
       <env> (I |-> Loc) ... </env>
       <store> Store => Store[Loc <- V] </store>

  context HOLE . _:Id = _:Val

  rule <k> ( I:Id => this . I ) = _:Val ... </k>
       <env> Rho </env>
       <genv> GRho </genv>
    requires (notBool(I in keys(Rho))) andBool (I in keys(GRho))

  rule <k> ( instance(Id) . Field:Id = V:Val ) => V ... </k>
       <id> Id </id>
       <genv> (Field |-> Pointer) ... </genv>
       <store> (Pointer |-> (_ => V)) ... </store>

  rule <k> instance(Id) . Field => Value ... </k>
       <id> Id </id>
       <genv> (Field |-> Pointer) ... </genv>
       <store> (Pointer |-> Value) ... </store>

  rule <instance>
        <k> instance(Id:Int) . Field => Value ... </k> ...
       </instance>
       <instance>
        <id> Id </id>
        <genv> (Field |-> Pointer) ... </genv> ...
       </instance>
       <store> (Pointer |-> Value) ... </store>
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
  syntax KItem ::= "execEntryCode"  "(" stateName: Id "," entryArgs: Vals ")"
                 | "returnControl"  "(" machineId: Int ")"
                 | "asGlobalDecls"  "(" decls: Stmt ")"
                 | "execEntryBlock" "(" Vals ")"
                 | "recordEnv"
                 | "restoreEnv"
                 | "execEventHandlers"
                 | "wait"
                 | "dequeueEvent"

  syntax InstExp ::= "instance" "(" instanceId: Int ")"
  syntax Exp ::= InstExp
  syntax Val ::= InstExp

  rule  <id> SourceId </id>
        <k> new MName ( Args ) => wait ... </k>
        ( .Bag => <instance>
                    <id> Loc </id>
                    <k> asGlobalDecls(MachineDecls)
                     ~> execEntryCode(InitState, Args)
                     ~> returnControl(SourceId)
                     ~> execEventHandlers </k>
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

  rule  <id> SourceId </id>
        <k> new MName ( Args ) => wait ... </k>
        ( .Bag => <instance>
                    <id> Loc </id>
                    <k> execEntryCode(InitState, Args)
                     ~> returnControl(SourceId)
                     ~> execEventHandlers </k>
                    <class> MName </class> ...
                   </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> . </declarationCode>
        <state>
          <stateName> InitState </stateName>
          <isInitState> true </isInitState> ...
        </state> ...
       </machine>

  rule asGlobalDecls(S Ss) => asGlobalDecls(S) ~> asGlobalDecls(Ss)
  rule asGlobalDecls(var Id;) => var this . Id;


  rule <instance>
        <id> SourceId </id>
        <k> wait => instance(TargetId) ... </k> ...
       </instance>
       <instance>
        <id> TargetId </id>
        <k> returnControl(SourceId) => . ... </k>  ...
       </instance>

  rule <k> execEntryCode(SName, Vals)
        =>   recordEnv
          ~> StateDecls
          ~> execEntryBlock(Vals) ...
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

  rule execEventHandlers => dequeueEvent ~> restoreEnv

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

  rule <k> execEntryBlock(.Vals) => . ... </k>
       <class> MName </class>
       <activeState> ActiveState </activeState>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> ActiveState </stateName>
          <entryBlock> . </entryBlock>
          <args> . </args> ...
        </state> ...
      </machine>

  rule assign(I:Id , Is | V:Val, Vs)
    => var I = V; ~> assign(Is | Vs)

  rule assign( .Ids | .Vals ) => .

  rule <k> restoreEnv => . ... </k>
       <env> _ => Rho </env>
       <stack> (ListItem(Rho) => .List ) ... </stack>
```
#### Event Handling

##### Sending Events
```k
  syntax Val ::= "done"
  syntax KItem ::= "eventArgsPair" "(" eventId: Id "|" args: Vals ")"

  rule <instance>
        <k> send instance(Id) , EventName:Id , ( Args ) =>  done ... </k>
        ...
       </instance>
       <instance>
        <id> Id </id>
        <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer> ...
       </instance>

  rule <k> send instance(Id) , EventName:Id , ( Args ) =>  done ... </k>
       <id> Id </id>
       <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer>
```

##### Dequeueing Events
```k
  rule <k> dequeueEvent =>
              recordEnv
           ~> assign(EventArgs | Vals )
           ~> HandlerCode
           ~> restoreEnv ... </k>
       <class> MName </class>
       <activeState> State </activeState>
       <inBuffer> (ListItem(eventArgsPair(EventId | Vals)) => .List) ... </inBuffer>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> State </stateName>
          <eventHandler>
            <eventId> EventId </eventId>
            <eventArgs> EventArgs </eventArgs>
            <handlerCode> HandlerCode </handlerCode> ...
          </eventHandler> ...
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
```

#### IPC via extern

```k
  syntax KItem ::= "doWriteAndCall" "(" String ")"
                 | "processCallResult"

  syntax JSON  ::= "Exp2JSON"   "(" Exp ")"  [function]
  syntax JSONs ::= "Exps2JSONs" "(" Vals ")" [function]
                 | "Obj2JSONs"  "(" Map ")"  [function]

  syntax ExpOrJSON ::= Val | JSON

  syntax Exp ::= "JSON2Obj"     "(" ExpOrJSON ")"
               | "constructObj" "(" ExpOrJSON ")"
               | "result2Obj"   "(" JSON ")"
               | "JSONs2Obj"    "(" JSONs ")"

  rule Exp2JSON(Name:Id) => Id2String(Name)
  rule Exp2JSON(Name:Id (Args:Vals))
    => { "name": Id2String(Name)
       , "args": [ Exps2JSONs(Args) ] }

  rule Exp2JSON(S:String) => S
  rule Exp2JSON(I:Int)    => I
  rule Exp2JSON(undef)    => "undef"

  rule [[ Exp2JSON(instance(Id)) => { Obj2JSONs(GEnv) } ]]
       <instance>
        <id> Id </id>
        <genv> GEnv </genv> ...
       </instance>

  rule [[ Obj2JSONs((Id |-> Pointer) GEnv)
    =>    Id2String(Id) : Exp2JSON(Value) , Obj2JSONs(GEnv) ]]
       <store> (Pointer |-> Value) ... </store>

  rule Obj2JSONs( .Map ) => .JSONs

  rule Exps2JSONs(E, Es) => Exp2JSON(E) , Exps2JSONs(Es)
  rule Exps2JSONs(.Vals) => .JSONs


  rule result2Obj({ "result" : S:String })                => S
  rule result2Obj({ "result" : I:Int })                   => I
  rule result2Obj({ "result" : (({ _:JSONs }) #as Obj )}) => constructObj(Obj)

  rule JSON2Obj(Field : I:Int)
    => var this.String2Id(Field); this.String2Id(Field) = I;

  rule JSON2Obj(Field : S:String)
    => var this.String2Id(Field); this.String2Id(Field) = S;

  rule JSON2Obj(Field : B:Bool)
    => var this.String2Id(Field); this.String2Id(Field) = B;

  rule JSON2Obj(Field : (({ _:JSONs }) #as Obj ))
    => var this.String2Id(Field); this.String2Id(Field) = constructObj(Obj);

  rule JSONs2Obj(J:JSON, Js:JSONs) => JSON2Obj(J) ~> JSONs2Obj(Js)
  rule JSONs2Obj(.JSONs)           => .

  syntax Id ::= "$Dynamic"

  rule <id> SourceId  </id>
       <k> constructObj( { Pairs:JSONs } ) => wait ... </k>
       ( .Bag =>  <instance>
                    <k> JSONs2Obj(Pairs) ~> returnControl(SourceId) </k>
                    <id> Loc </id>
                    <class> $Dynamic </class> ...
                  </instance> )
       <nextLoc> Loc:Int => Loc +Int 1 </nextLoc>
       <store> (.Map => (Loc |-> instance(Loc))) ... </store>

  context extern _:Id ( HOLE:Exps )
  rule extern Name:Id ( Args:Vals )
    =>   #mkstemp("externXXX")
      ~> doWriteAndCall(JSON2String(Exp2JSON(Name(Args))))

  rule #tempFile(FName, FD) ~> doWriteAndCall(S)
    =>   #write(FD, S)
      ~> #close(FD)
      ~> #system("./middleware " +String FName)
      ~> processCallResult

  rule #systemResult(0, StdOut, _) ~> processCallResult
    => result2Obj(#parseKORE(StdOut))

endmodule
```

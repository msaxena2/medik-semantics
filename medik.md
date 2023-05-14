Medik Semantics
===============

Syntax
------

```k
requires "json.md"
requires "rat.md"

module MEDIK-SYNTAX
  imports DOMAINS-SYNTAX
  imports RAT-SYNTAX

  syntax Ids ::= List{Id, ","}    [klabel(exps)]

  syntax DetVal ::= Int | Bool | String
  syntax NonDetVal

  syntax Val  ::= DetVal | NonDetVal
  syntax Vals ::= List{Val, ","}  [klabel(exps)]

  syntax FloatLiteral ::= r"([\\+-]?[0-9]+(\\.[0-9]*)?|\\.[0-9]+)" [token, prec(1)]

  syntax StandaloneExp ::= "new" Id "(" Exps ")"                       [strict(2)]
                         | "createFromInterface" "(" Id "," String ")" [strict(2)]
                         | Id "(" Exps ")"                             [strict(2)]
                         | "exit"
                         | "stop"

  syntax UndefExp ::= "undef"

  syntax Exp ::= Id
               | Val
               | Rat
               | FloatLiteral
               | "this"
               | UndefExp
               | "obtainFrom" "(" Exp "," Exp ")"            [seqstrict]
               | "(" Exp ")"                                 [bracket]
               > Exp "." Exp                                 [strict(1), left]
               > Exp "+" Exp                                 [seqstrict, left]
               | Exp "-" Exp                                 [seqstrict, left]
               | Exp "*" Exp                                 [seqstrict, left]
               | Exp "/" Exp                                 [seqstrict, left]
               | Exp ">" Exp                                 [seqstrict, left]
               | Exp "<" Exp                                 [seqstrict, left]
               | Exp ">=" Exp                                [seqstrict, left]
               | Exp "<=" Exp                                [seqstrict, left]
               | "!" Exp                                     [seqstrict, left]
               | Exp "&&" Exp                                [strict(1), left]
               | Exp "||" Exp                                [strict(1), left]
               > Exp "==" Exp                                [seqstrict, left]
               | "interval" "(" Exp "," Exp ")"
               > Exp "in" Exp
               | "parseInt" "(" Exp ")"                      [strict]
               | StandaloneExp

  syntax Exps ::= List{Exp, ","}  [seqstrict, klabel(exps), avoid]
                | Vals

  syntax Stmt ::= StandaloneExp ";"                               [strict]
                | "sleep" "(" Exp ")" ";"                         [strict(1)]
                | "send" Exp "," ExtId ";"                        [macro]
                | "send" Exp "," ExtId "," "(" Exps ")" ";"       [seqstrict(1, 3)]
                | "broadcast" Id ";"                              [macro]
                | "broadcast" Id "," "(" Exps ")" ";"             [strict(2)]
                | "goto" Id ";"                                   [macro]
                | "goto" Id "(" Exps ")" ";"                      [strict(2)]
                | "print" "(" Exp ")" ";"                         [strict]
                > "return" ";"                                    [macro]
                | "return" Exp ";"                                [strict(1)]
                | "var" Id "=" Exp ";"                            [macro]
                > Exp "=" Exp ";"                                 [strict(2)]
                > "var" Id ";"
                | "vars" Ids ";"                                  [macro-rec]
                | Block
                > "if" "(" Exp ")" Block                          [strict(1)]
                | "if" "(" Exp ")" Block "else" Block             [strict(1)]
                | "while" "(" Exp ")" Block
                | "entry" Block                                   [macro]
                | "entry" "(" Ids ")" Block
                | "on" ExtId "do" Block                           [macro]
                | "on" ExtId "(" Ids ")" "do" Block
                | "fun" Id "(" Ids ")" Block
                | NonDetStmt
                | Exp "in" "{" CaseDecl "}"
                | StateDecl
                > "machine" Id Block                              [macro]
                | "machine" Id "receives" Ids Block
                | "interface" Id Block                            [macro]
                | "interface" Id "receives" Ids Block
                | "init" "machine" Id Block                       [macro]
                | "init" "machine" Id "receives" Ids Block
                | "yield" ";"
                > Stmt Stmt                                       [right]

  syntax StateDecl ::= "state" Id Block
                     | "init" "state" Id Block

  syntax CaseDecl ::= Exp ":" Stmt
                    | CaseDecl "default" ":" Stmt
                    > CaseDecl CaseDecl                    [right]

  syntax Block ::= "{" "}"                                 [macro]
                 | "{" Stmt "}"

```

### Non-deterministic choice

```{.mcheck .symbolic}
  syntax NonDetStmt ::= "either" Block "or" Block

  syntax NonDetVal ::= "#nondet"
```

### ObtainFrom requests/responses

We allow certain special Ids in the semantics to track
obtainFrom requests and responses. For concrete execution,
these Ids are not available in programs.

```k
  // Declarations for symbolic execution/model-checking
  syntax NonDetStmt

  // Extended Ids for obtain requests/response
  syntax ExtId ::= Id
  syntax Exp ::= ExtId
```

For model checking  and symbolic execution however, since
these requests are simulated by ghost machines, we allow using them
in ghost machine syntax.

```{.mcheck .symbolic}
  syntax ExtId ::= Id | "$ObtainRequest" | "$ObtainResponse"
```

```k
endmodule
```
### Macros

Rewrite rules for `macro` constructs

```k
module MEDIK-SYNTAX-EXT
  imports MEDIK-SYNTAX

  rule vars I1:Id, Is:Ids; => var I1; vars Is;
  rule vars I, .Ids; => var I;
  rule var I:Id = E:Exp ; => var I; I = E;
  rule entry B:Block => entry (.Ids) B
  rule on E:ExtId do B:Block => on E (.Ids) do B
  rule send Id , Event; => send Id, Event, ( .Vals );
  rule goto State:Id; => goto State ( .Vals );
  rule machine Name:Id Code
    => machine Name receives .Ids Code
  rule init machine Name:Id Code
    => init machine Name receives .Ids Code
  rule broadcast Event; => broadcast Event, ( .Vals );
  rule interface Name:Id Code
    => interface Name receives .Ids Code
  rule return ; => return undef;

  syntax Stmt ::= "nothing" ";"

  rule { } => { nothing; }

endmodule
```

Semantics
---------

```k
module MEDIK
  imports MEDIK-SYNTAX-EXT
  imports DOMAINS
  imports JSON
  imports K-REFLECTION
  imports RAT
  imports RAT-COMMON
  imports COLLECTIONS

  syntax DetVal  ::= "null" | UndefExp | Rat

  syntax KResult ::= Val
                   | Vals
```
```symbolic
  // For some reason, haskell backend doesn't cool ".Vals"
  rule isKResult(.Vals) => true
```
```k
  configuration <instances>
                  <instance multiplicity="*" type="Map">
                    <id> 0 </id>
                    <k> populateCells($PGM:Stmt) ~> createInitInstances </k>
                    <env> .Map </env>
                    <genv> .Map </genv>
                    <class> . </class>
                    <fstack> .List </fstack>
                    <stack> .List </stack>
                    <inBuffer> .List </inBuffer>
                    <activeState> . </activeState>
                    <callerId> . </callerId>
                    <foreignId> ""    </foreignId>
                  </instance>
                </instances>
                <machines>
                  <machine multiplicity="*" type="Map">
                    <machineName> . </machineName>
                    <declarationCode> nothing; </declarationCode>
                    <isInitMachine> false </isInitMachine>
                    <receiveEvents> .Set </receiveEvents>
                    <states>
                      <state multiplicity="*" type="Map">
                        <stateName> . </stateName>
                        <stateDeclarations> nothing; </stateDeclarations>
                        <entryBlock>  nothing; </entryBlock>
                        <args> .Ids </args>
                        <isInitState> false </isInitState>
                        <exitBlock> . </exitBlock>
                        <handledEventIds> .Set </handledEventIds>
                        <eventHandlers>
                          <eventHandler multiplicity="*" type="Set">
                            <eventId> . </eventId>
                            <eventArgs> .Ids </eventArgs>
                            <handlerCode> nothing; </handlerCode>
                          </eventHandler>
                        </eventHandlers>
                      </state>
                    </states>
                    <functions>
                      <function multiplicity="*" type="Map">
                        <functionName> . </functionName>
                        <functionArgs> . </functionArgs>
                        <functionCode> . </functionCode>
                      </function>
                    </functions>
                  </machine>
                </machines>
                <interfaces>
                  <interface multiplicity="*" type="Map">
                    <interfaceName> . </interfaceName>
                    <interfaceDeclarations> nothing; </interfaceDeclarations>
                    <interfaceReceiveEvents> .Set </interfaceReceiveEvents>
                  </interface>
                </interfaces>
                <activeInstances> SetItem(0) </activeInstances>
                <store> .Map </store>
                <nextLoc> 1 </nextLoc>
                <foreignInstances> false </foreignInstances>
                <tidCount> 1 </tidCount>
                <externInstanceId> . </externInstanceId> // Hack until k is fixed
                <executorAvailable> true </executorAvailable>
                <stuck> false </stuck>
                <epoch> 0 </epoch>
                <shouldAdvanceEpoch> false </shouldAdvanceEpoch>
```
```{.mcheck .symbolic}
                <output> .List </output>
                <sleeping> .List </sleeping>
                <slept> .List </slept>
```

### Configuration Population

Here we define semantics for constructing
the configuration by traversing the program

```k

  syntax Set ::= "asSet" "(" Ids ")" [function]

  rule asSet(I:Id, Is:Ids) => SetItem(I) asSet(Is)
  rule asSet(.Ids)         => .Set

  syntax KItem ::= "populateCells" "(" program: Stmt ")"

  rule populateCells(M Ms) => populateCells(M) ~> populateCells(Ms)

  syntax KItem ::= "populateMachineCell" "(" machine: Id "," code: Stmt ")"

  rule <k> populateCells(machine MName receives EventList { Code } )
        => populateMachineCell(MName, Code) ...
       </k>
       <machines>
        ( .Bag => <machine>
                    <machineName> MName </machineName>
                    <receiveEvents> asSet(EventList) </receiveEvents> ...
                  </machine> ) ...
       </machines>

  rule <k> populateCells(init machine MName receives EventList { Code } )
        => populateMachineCell(MName, Code) ...
       </k>
       <machines>
        ( .Bag => <machine>
                    <machineName> MName </machineName>
                    <receiveEvents> asSet(EventList) </receiveEvents>
                    <isInitMachine> true </isInitMachine> ...
                  </machine> ) ...
       </machines>

  rule <k> populateCells(interface IName receives EventList { Code } )
        => populateInterfaceCell(IName, Code) ...
       </k>
       <interfaces>
        ( .Bag => <interface>
                    <interfaceName> IName </interfaceName>
                    <interfaceReceiveEvents> asSet(EventList) </interfaceReceiveEvents> ...
                  </interface> ) ...
       </interfaces>

  rule populateMachineCell(M, S Ss)
    => populateMachineCell(M, S) ~> populateMachineCell(M, Ss)

  rule <k> populateMachineCell(MName, (var GVar;)) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> S:Stmt => S var this.GVar; </declarationCode> ...
       </machine>

  rule <k> populateMachineCell(MName, (GVar = Exp;)) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> S:Stmt => S this.GVar = Exp; </declarationCode> ...
       </machine>

  rule <k> populateMachineCell(MName, fun FunName (Args) Block) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <functions>
          (.Bag => <function>
                    <functionName> FunName </functionName>
                    <functionArgs> Args </functionArgs>
                    <functionCode> Block </functionCode> ...
                   </function> ) ...
        </functions> ...
      </machine>

  syntax KItem ::= "populateInterfaceCell" "(" name:Id "," code: Stmt ")"

  rule populateInterfaceCell(IName, S Ss)
    => populateInterfaceCell(IName, S) ~> populateInterfaceCell(IName, Ss)

  rule <k> populateInterfaceCell(IName, var GVar;) => . ... </k>
       <interface>
        <interfaceName> IName </interfaceName>
        <interfaceDeclarations> S => S var this.GVar; </interfaceDeclarations> ...
       </interface>

  rule populateInterfaceCell(_, nothing;) => .

  syntax KItem ::= "populateStateCell"   "(" machine: Id "," state: Id "," code: Stmt ")"

  rule <k> populateMachineCell(MName, state SName { Code } )
        => populateStateCell(MName, SName, Code) ... </k>
       <machine>
        <machineName> MName </machineName>
        <states>
          (.Bag => <state>
                    <stateName> SName </stateName> ...
                   </state> ) ...
        </states> ...
       </machine>

  rule <k> populateMachineCell(MName, init state SName { Code } )
        => populateStateCell(MName, SName, Code) ... </k>
       <machine>
        <machineName> MName </machineName>
        <states>
          (.Bag => <state>
                    <stateName> SName </stateName>
                    <isInitState> true </isInitState> ...
                   </state> ) ...
        </states> ...
       </machine>

  rule populateStateCell(MName, SName, S Ss)
    => populateStateCell(MName, SName, S) ~> populateStateCell(MName, SName, Ss)

  rule <k> populateStateCell(MName, SName, (var _;) #as StateDecl) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S:Stmt => S StateDecl </stateDeclarations> ...
        </state> ...
       </machine>

  rule <k> populateStateCell(MName, SName, (_ = _;) #as StateDecl) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S:Stmt => S StateDecl </stateDeclarations> ...
        </state> ...
       </machine>

  rule <k> populateStateCell(MName, SName, entry(Args) Code:Block) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <entryBlock> _ => Code </entryBlock>
          <args> _ => Args </args> ...
        </state> ...
       </machine>

  rule <k> populateStateCell(MName, SName, on Event (EventArgs) do HandlerCode:Block) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <handledEventIds> (.Set => SetItem(Event)) ... </handledEventIds>
          <eventHandlers>
            (.Bag => <eventHandler>
                      <eventId> Event </eventId>
                      <eventArgs> EventArgs </eventArgs>
                      <handlerCode> HandlerCode </handlerCode> ...
                     </eventHandler> ) ...
          </eventHandlers> ...
        </state> ...
      </machine>

  rule populateStateCell(_, _, nothing;) => .
```

Here we define operations for starting execution in the
initial state of the initial instance, and the instance
for handling external communications.

```k
  syntax KItem ::= "createMainInstance"
                 | "createInitInstances"
                 | "createExternHandlerInstance"

```

```concrete
  rule createInitInstances => createExternHandlerInstance ~> createMainInstance

  rule <k> createExternHandlerInstance => . ... </k>
       (.Bag =>  <instance>
                  <id> Loc </id>
                  <k> readExternInput </k> ...
                 </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> (.Map => (Loc |-> instance(Loc))) ... </store>
       <externInstanceId> _ => Loc </externInstanceId>
````

```{.symbolic .mcheck}
  rule createInitInstances => createMainInstance
```


```k
  rule <k>   createMainInstance
        =>   MachineDecls
          ~> enterState(InitState | .Vals | Epoch ) ...
       </k>
       <class> _ => InitMName </class>
       <machine>
        <machineName> InitMName </machineName>
        <declarationCode> MachineDecls </declarationCode>
        <isInitMachine> true </isInitMachine>
        <state>
          <stateName> InitState </stateName>
          <isInitState> true </isInitState> ...
        </state> ...
       </machine>
       <epoch> Epoch </epoch>
```

### Expression and Statement

#### Variable Assignment and Lookup
```k
  rule nothing ;   => .
  rule _:Val ;     => .

  rule S:Stmt Ss:Stmt => S ~> Ss

  syntax Stmt ::= "var" Exp "." Id ";" [strict(1)]

  rule <k> this => instance(Id) ... </k>
       <id> Id </id>

  rule <k> var instance(Id) . Field ; => . ... </k>
       <id> Id </id>
       <genv> GRho => GRho[Field <- Loc] </genv>
       <store> (.Map => (Loc |-> undef)) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule <k> var Id ; => . ... </k>
       <env> Rho => Rho[Id <- Loc] </env>
       <store> .Map => (Loc |-> undef) ... </store>
       <nextLoc> Loc => Loc +Int 1 </nextLoc>

  rule <k> I:Id => V ... </k>
       <env> (I |-> Loc) ... </env>
       <store> (Loc |-> V) ... </store>

  rule <k> I:Id => this . I ... </k>
       <env> Rho </env>
       <genv> GRho </genv>
    requires (notBool(I in keys(Rho))) andBool (I in keys(GRho))

  rule <k> I:Id = V:Val ; => . ... </k>
       <env> (I |-> Loc) ... </env>
       <store> Store => Store[Loc <- V] </store>

  context HOLE . _:Id = _:Val ;

  rule <k> ( I:Id => this . I ) = _:Val ; ... </k>
       <env> Rho </env>
       <genv> GRho </genv>
    requires (notBool(I in keys(Rho))) andBool (I in keys(GRho))

  rule <k> ( instance(Id) . Field:Id = V:Val ; ) => . ... </k>
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

#### Rational Numbers
We convert every floating point literal into a rational.

We first convert the float-literal token to a string. Then,
we split the string on the radix. The characteristic+mantissa string forms the
numerator of our rational fraction. The denominator is of the form 10^n, where n
is the number of mantissa digits.

```k
  syntax Rat    ::= "floatString2Rat"     "(" floatString: String ")"        [function]
  syntax String ::= "floatLiteral2String" "(" floatToken:  FloatLiteral ")"  [function, functional, hook(STRING.token2string)]

  rule F:FloatLiteral => floatString2Rat(floatLiteral2String(F))

  rule floatString2Rat(FL) =>    String2Int( substrString( FL
                                                         , 0
                                                         , findChar(FL, ".", 0))
                                            +String
                                            substrString( FL
                                                        , findChar(FL, ".", 0) +Int 1
                                                        , lengthString(FL)))
                              /Rat
                                 (10 ^Int (lengthString(FL) -Int (findChar(FL, ".", 0) +Int 1)))

```

#### Arithmetic Expressions
```k
  rule I1 + I2 => I1 +Rat I2
  rule I1 - I2 => I1 -Rat I2
  rule I1 * I2 => I1 *Rat I2
  rule I1 / I2 => I1 /Rat I2
    requires notBool (I2 ==Rat 0)
  rule _ / 0 => undef

  rule S1       + S2          => S1 +String S2
  rule S:String + I:Int       => S +String Int2String(I)
  rule I:Int    + S:String    => Int2String(I) +String S
  rule S:String + true        => S +String "true"
  rule S:String + false       => S +String "false"
  rule true     + S:String    => "true" +String S
  rule false    + S:String    => "false" +String S
  rule <I1, I2>Rat + S:String
    =>        "<" +String Int2String(I1)
      +String "," +String Int2String(I2)
      +String ">Rat"
      +String S
  rule S:String + <I1, I2>Rat
    =>         S
      +String "<" +String Int2String(I1)
      +String "," +String Int2String(I2)
      +String ">Rat"
```

#### Blocks
```k

  rule { S:Stmt } => recordEnv ~> S ~> restoreEnv
```

#### Boolean Expressions

```k
  rule I1 < I2 => I1 <Rat I2
  rule I1 > I2 => I1 >Rat I2

  rule I1 <= I2 => I1 <=Rat I2
  rule I1 >= I2 => I1 >=Rat I2

  rule !true  => false
  rule !false => true

  rule true  && B => B
  rule false && _ => false
  rule true  || _ => true
  rule false || B => B

  rule X:DetVal == X        => true
  rule _:DetVal == _:DetVal => false [owise]
```

```mcheck
  rule _:NonDetVal == _:Val           => #nondet
  rule _:Val       == _:NonDetVal     => #nondet
```

#### Instance creation via new

When a new instance is created, the instance cell is added and
global environment initialized. The new instance's `init` state
is scheduled. The *caller* does not give up control.

```k
  syntax KItem ::= "wait"

  rule <id> SourceId </id>
       <k> new MName (Args) => wait ... </k>
       ( .Bag => <instance>
                  <id> Loc </id>
                  <k> MachineDecls
                   ~> enterState(InitState | Args )
                  </k>
                  <callerId> SourceId </callerId>
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
       <activeInstances> ... (.Set => SetItem(Loc)) </activeInstances>

```

The `instance` construct simply wraps the `id` of an
instance.

```k
  syntax Val           ::= "instance" "(" instanceId: Int ")"
  syntax StandaloneExp ::= Val

```

#### Executors

An executor is responsible for *running* a block of code.

```k
  syntax KItem ::= "releaseExecutor"

  rule <instance>
        <id> CalleeId </id>
        <k> releaseExecutor => . ... </k>
        <callerId> CallerId => . </callerId> ...
       </instance>
       <instance>
        <id> CallerId </id>
        <k> wait => instance(CalleeId) ... </k> ...
       </instance>

  rule <k> releaseExecutor => . ... </k>
       <callerId> . </callerId>
       <executorAvailable> _ => true </executorAvailable>
```

#### Entering States

```k
  syntax KItem ::= "enterState" "(" stateName: Id "|" entryArgs: Vals ")"
                 | "enterState" "(" stateName: Id "|" entryArgs: Vals "|" epoch: Int ")"

  rule <k>   enterState(SName | Args )
        =>   StateDecls
          ~> assign(BlockVars | Args)
          ~> EntryBlock
          ~> releaseExecutor
          ~> handleEvents ...
       </k>
       <activeState> _ => SName </activeState>
       <env> _ => .Map </env>
       <class> MName </class>
       <machine>
        <machineName> MName </machineName>
         <state>
          <stateName> SName </stateName>
          <stateDeclarations> StateDecls </stateDeclarations>
          <entryBlock> EntryBlock </entryBlock>
          <args> BlockVars </args> ...
         </state> ...
        </machine>

  rule <k>   enterState(SName | Args | Scheduled )
        =>   StateDecls
          ~> assign(BlockVars | Args)
          ~> EntryBlock
          ~> releaseExecutor
          ~> handleEvents ...
       </k>
       <activeState> _ => SName </activeState>
       <env> _ => .Map </env>
       <class> MName </class>
       <machine>
        <machineName> MName </machineName>
         <state>
          <stateName> SName </stateName>
          <stateDeclarations> StateDecls </stateDeclarations>
          <entryBlock> EntryBlock </entryBlock>
          <args> BlockVars </args> ...
         </state> ...
        </machine>
       <executorAvailable> true => false </executorAvailable>
       <epoch> Current </epoch>
        requires Scheduled <=Int Current
```

#### Event Handling

A machine is *enabled* if there is an event in the queue that
can be handled in the active state. An executor must be available
to run the event handler.

```k
  syntax KItem ::= "handleEvents"

  rule <k>   handleEvents
        =>   assign(Vars | Args)
          ~> HandlerCode
          ~> releaseExecutor ...
       </k>
       <class> MachineName </class>
       <activeState> Active </activeState>
       <inBuffer> (ListItem(event(EventId | Args | Scheduled )) => .List) ... </inBuffer>
       <machine>
        <machineName> MachineName </machineName>
        <state>
         <stateName> Active </stateName>
         <eventHandler>
           <eventId> EventId </eventId>
           <eventArgs> Vars </eventArgs>
           <handlerCode> HandlerCode </handlerCode> ...
         </eventHandler> ...
        </state> ...
      </machine>
      <epoch> Current </epoch>
      <executorAvailable> true => false </executorAvailable>
        requires Scheduled <=Int Current

  rule <epoch> Epoch => Epoch +Int 1 </epoch>
       <shouldAdvanceEpoch> true => false </shouldAdvanceEpoch>
    [priority(250)]
```

#### Stuck Execution

We assume a machine is `stuck` if the `entry` block
of a state has been executed, i.e., the machine is waiting for
an incoming event, and the event at the head of the queue is
not handled in the machine's active state

```k
  syntax KItem ::= "stuck"

  rule <k> handleEvents ~> _ => stuck </k>
       <activeState> ActiveState </activeState>
       <class> MachineName </class>
       <inBuffer> ListItem(event(InputEvent | _ | _ )) ... </inBuffer>
       <machine>
        <machineName> MachineName </machineName>
        <state>
         <stateName> ActiveState </stateName>
         <handledEventIds> HandledEvents </handledEventIds> ...
        </state> ...
       </machine>
       <executorAvailable> true </executorAvailable>
       <stuck> _ => true </stuck>
    requires notBool(InputEvent in HandledEvents)                       [priority(175)]

  rule <k> . => stuck </k>
       <inBuffer> ListItem(_) ... </inBuffer>
       <stuck> _ => true </stuck>                                       [priority(175)]

```

#### Memory Management

We use a simple memory management scheme:
 - Once a local variable goes out of scope, the corresponding
   value is removed from the global store.
 - Once a machine can no longer execute, it is garbage collected.

```k

  syntax KItem ::= "recordEnv"
                 | "restoreEnv"
                 | "restoreEnv" "(" List ")"

  rule <k> recordEnv => . ... </k>
       <env> Rho </env>
       <stack> .List => ListItem(Rho) ... </stack>

  rule <k> restoreEnv => restoreEnv(keys_list(Rho)) ... </k>
       <env> Rho </env>

  rule <k> restoreEnv((ListItem(Id) => .List) _) ... </k>
       <env> ((Id |-> Ptr) => .Map) ... </env>
       <stack> (ListItem((Id |-> Ptr2) _)) ... </stack>
       <store> ((Ptr |-> _) => .Map) ... </store>
    requires Ptr =/=K Ptr2

  rule <k> restoreEnv((ListItem(Id) => .List) _) ... </k>
       <env> ((Id |-> Ptr) => .Map) ... </env>
       <stack> (ListItem((Id |-> Ptr) _)) ... </stack>

  rule <k> restoreEnv((ListItem(Id) => .List) _) ... </k>
       <env> ((Id |-> Ptr) => .Map) ... </env>
       <stack> (ListItem(Rho)) ... </stack>
       <store> ((Ptr |-> _) => .Map) ... </store>
         requires notBool (Id in keys(Rho))

  rule <k> restoreEnv(.List) => . ... </k>
       <env> .Map => Rho </env>
       <stack> (ListItem(Rho) => .List) ... </stack>

  rule <k> restoreEnv(.List) => . ... </k>
       <env> .Map </env>
       <stack> .List </stack>

  syntax KItem ::= "deleteInstance" "(" envList: List ")"

  rule <instance>
        <k> . => deleteInstance(keys_list(Rho)) </k>
        <genv> Rho </genv>
        <class> MachineName </class>
        <activeState> _ </activeState> ...
       </instance>
       <machine>
        <machineName> MachineName </machineName> ...
       </machine>
       <executorAvailable> true => false </executorAvailable>

  rule <k> deleteInstance((ListItem(Id) => .List) _) </k>
       <genv> ((Id |-> Ptr) => .Map) ... </genv>
       <store> ((Ptr |-> _) => .Map) ... </store>

  rule <instances>
        ( <instance>
            <id> Id </id>
            <k> deleteInstance(.List) </k>
            <genv> .Map </genv> ...
          </instance>
          => .Bag
        ) ...
       </instances>
       <activeInstances> (SetItem(Id) => .Set) ... </activeInstances>
       <executorAvailable> false => true </executorAvailable>

  syntax KItem ::= "assign" "(" args: Ids "|" values: Vals ")"

  rule assign(I:Id , Is | V:Val, Vs)
    => var I = V; ~> assign(Is | Vs)

  rule assign( .Ids | .Vals ) => .
```

##### Semantics of goto

```k

  rule <k> goto Target:Id ( Args:Vals ) ; ~> _
       =>  releaseExecutor
        ~> enterState(Target | Args | Epoch +Int 1 )
       </k>
       <env> _ => .Map </env>
       <stack> _ => .List </stack>
       <epoch> Epoch </epoch>
       <shouldAdvanceEpoch> _ => true </shouldAdvanceEpoch>
```
#### Event Handling

Ids beginning with a `$` cannot be used in a medik program for concrete
execution.

```concrete
  syntax ExtId ::= "$ObtainResponse"
                 | "$SleepDone"
```

##### Sending Events
```k
  syntax KItem ::= "event"            "(" eventId: ExtId "|" args: Vals "|" epoch: Int ")"
                 | "performBroadcast" "(" eventId: Id    "|" args: Vals "|" List ")"

  syntax List ::= "getRecievers"    "(" eventId: Id ")"          [function]
  syntax List ::= "getRecieversAux" "(" eventId: Id "|" List ")" [function]

  rule [[ getRecievers(Event) => getRecieversAux(Event | Set2List(ActiveInstances)) ]]
       <activeInstances> ActiveInstances </activeInstances>

  rule [[ getRecieversAux(Event | ListItem(Id) Rest) => ListItem(Id) getRecieversAux(Event | Rest) ]]
       <instance>
        <id> Id </id>
        <class> Name </class> ...
       </instance>
       <machine>
        <machineName> Name </machineName>
        <receiveEvents> SetItem(Event) ... </receiveEvents> ...
       </machine>


  rule [[ getRecieversAux(Event | ListItem(Id) Rest) => ListItem(Id) getRecieversAux(Event | Rest) ]]
       <instance>
        <id> Id </id>
        <class> Name </class> ...
       </instance>
       <interface>
        <interfaceName> Name </interfaceName>
        <interfaceReceiveEvents> SetItem(Event) ... </interfaceReceiveEvents> ...
       </interface>

  rule getRecieversAux(_     | .List)            => .List
  rule getRecieversAux(Event | ListItem(_) Rest) => getRecieversAux(Event | Rest) [owise]

  rule <instance>
        <k> send instance(Id) , EventName:ExtId , ( Args ) ; =>  . ... </k>
        ...
       </instance>
       <instance>
        <id> Id </id>
        <class> CName </class>
        <inBuffer> ... (.List => ListItem( event(EventName | Args | Epoch +Int 1))) </inBuffer> ...
       </instance>
       <machineName> CName </machineName>
       <epoch> Epoch </epoch>
       <shouldAdvanceEpoch> _ => true </shouldAdvanceEpoch>

  rule <k> send instance(Id) , EventName:ExtId , ( Args ) ; =>  . ... </k>
       <id> Id </id>
       <inBuffer> ... (.List => ListItem( event(EventName | Args | Epoch +Int 1))) </inBuffer>
       <epoch> Epoch </epoch>
       <shouldAdvanceEpoch> _ => true </shouldAdvanceEpoch>
```

#### Broadcasts

To perform a broadcast, all instances belonging to machine definitions
that *receive* the event are gathered, followed by a *send* to each of
these instance.

```k
  rule broadcast EventName:Id , ( Args ) ;
    => performBroadcast ( EventName | Args | getRecievers(EventName))

  rule performBroadcast ( EventName | Args | ListItem(Id) Recievers)
    =>   send instance(Id), EventName, ( Args ) ;
      ~> performBroadcast ( EventName | Args | Recievers)

  rule performBroadcast(_ | _ | .List) => .
```

#### Other Operations

```k
  syntax JSON ::= "Val2JSON" "(" Val ")" [function]

  rule Val2JSON(<I1, I2>Rat)
    => "<" +String Int2String(I1) +String "," +String Int2String(I2) +String ">Rat"
  rule Val2JSON(I:Int)      => I
  rule Val2JSON(S:String)   => S
  rule Val2JSON(undef)      => "undef"
  rule Val2JSON(B:Bool)     => B
```

##### Print

We treat printing as sending an event to an implicit "tty"
external machine

```concrete
  rule <k> print(V:Val) ;
        => jsonWrite( { "action" : "print"
                      , "args"   : [Val2JSON(V)] }
                    , #stdout)  ...
       </k>
       <tidCount> TId => TId +Int 1 </tidCount>

```

```{.mcheck .symbolic}
  rule <k> print(S:String); => . ... </k>
       <output> ... .List => ListItem(S) </output>
  rule <k> print(I:Int); => . ... </k>
       <output> ... .List => ListItem(Int2String(I)) </output>
  rule <k> print(B:Bool); => . ... </k>
       <output> ... .List => ListItem(Bool2String(B)) </output>
```

#### If/While/In

```k
  rule if (true) Block => Block
  rule if (false) _ => .

  rule if (true) Block else _  => Block
  rule if (false) _ else Block => Block
```

```k
  rule while (Cond) Block
   => if (Cond) { Block while (Cond) Block }
  rule E in interval(L, R) => (E >= L) && (E < R)
  rule E in { interval(L, U): S:Stmt Cs:CaseDecl }
    => if ((E >= L) && (E < U)) { S } else { E in { Cs } }
  rule E in { interval(L, U): S:Stmt }
    => if ((E >= L) && (E < U)) { S }
  rule E in { interval(L, U): S1:Stmt default: S2:Stmt }
    => if  ((E >= L) && (E < U)) { S1 } else { S2 }
```

#### Methods

```k
  syntax KItem ::= "fstackItem" "(" Map "|" K ")"

  rule <k> FunName:Id ( Vals ) ~> Rest
        => assign(Args | Vals) ~> FunCode ~> return undef;
       </k>
       <class> CName </class>
       <machine>
        <machineName> CName </machineName>
        <function>
          <functionName> FunName </functionName>
          <functionArgs> Args </functionArgs>
          <functionCode> FunCode </functionCode> ...
        </function> ...
       </machine>
       <fstack> (.List => ListItem(fstackItem(Rho | Rest))) ... </fstack>
       <env> Rho </env>

  rule <k> return V:Val ; ~> _ => V ~> Rest </k>
       <env> _ => Rho </env>
       <fstack> (ListItem(fstackItem(Rho | Rest)) => .List) ... </fstack>

```

#### Semantics of Obtain

The `obtainFrom` keyword provides a mechanism in medik to fetch values from an external
source at runtime.

```concrete
  syntax KItem ::= "waitForObtainResponse" "(" Exp ")"

  rule <k> obtainFrom(instance(Id:Int), Field:String)
        =>   jsonWrite( { "id"        : FId
                        , "tid"       : TId
                        , "interface" : Exp2JSON(IName)
                        , "name"      : "Obtain"
                        , "args"      : [ Field , .JSONs ] }, #stdout)
          ~> releaseExecutor
          ~> waitForObtainResponse(TId) ...
       </k>
       <instance>
        <id> Id </id>
        <class> IName </class>
        <foreignId> FId </foreignId> ...
       </instance>
       <interfaceName> IName </interfaceName>
       <tidCount> TId => TId +Int 1 </tidCount>

  rule <k> waitForObtainResponse(_) => V ... </k>
       <inBuffer> (ListItem(event($ObtainResponse | V:Val  | Epoch )) => .List)
                   ...
       </inBuffer>
       <executorAvailable> true => false </executorAvailable>
       <epoch> Epoch </epoch>

```

When model checking, a ghost machine is expected to receive and respond to the
request. The event sent to the ghost has the following format:
 - name: $ObtainRequest, args: (sourceId:Int, transactionId:Int, fieldToObtain:String)

The expected response by the ghost must have the name `$ObtainReponse`, and
contain the transaction id from the request, and the obtained value, as
the first and second arguments respectively.

```mcheck
  syntax KItem ::= "waitForObtainResponse" "(" Exp ")"

  rule <id> SrcId </id>
       <k> obtainFrom(instance(Id:Int), Field:String)
        =>   releaseExecutor
          ~> waitForObtainResponse(TId) ...
       </k>
       <instance>
        <id> Id </id>
        <inBuffer> ...
            (.List => ListItem(event($ObtainRequest | instance(SrcId) , TId , Field | Epoch +Int 1)))
        </inBuffer> ...
       </instance>
       <tidCount> TId => TId +Int 1 </tidCount>
       <epoch> Epoch </epoch>
       <shouldAdvanceEpoch> _ => true </shouldAdvanceEpoch>

  rule <k> waitForObtainResponse(TId) => V ... </k>
       <inBuffer>
        (ListItem(event($ObtainResponse | TId:Int, V:Val | Epoch )) => .List) ...
       </inBuffer>
       <executorAvailable> true => false </executorAvailable>
       <epoch> Epoch </epoch>
```
#### IPC via extern

```concrete
  syntax JSON ::= "json-undef" [klabel(JSON-RPCundef), symbol]
  syntax IOJSON ::= IOError | JSON

  syntax IOJSON ::= "jsonRead" "(" Int ")"            [function, hook(JSON.read)]
                  | "jsonWriteError" "(" JSON ")"     [klabel(JSON-RPC_putResponseError), symbol]

  syntax K ::= "jsonWrite" "(" JSON "," Int ")"       [function, hook(JSON.write)]
```

```symbolic
  syntax IOJSON ::= IOError
                  | JSON
                  | "jsonRead" "(" Int ")"            [function]
                  | "jsonWriteError" "(" JSON ")"     [klabel(JSON-RPC_putResponseError), symbol]
                  | "jsonReadError"  "(" String ")"   [klabel(JSON-RPC_readError), symbol]

  syntax K ::= "jsonWrite" "(" JSON "," Int ")"       [function]


  rule jsonWrite(J, _) => jsonWriteError(J)
  rule jsonRead(_)     => jsonReadError("unimplemented JSON Read/Write hooks")
```

```concrete
  syntax KItem ::= "doWriteAndCall" "(" String ")"
                 | "processCallResult"

  syntax JSON  ::= "Exp2JSON"   "(" Exp ")"  [function]
  syntax JSONs ::= "Exps2JSONs" "(" Vals ")" [function]
                 | "Obj2JSONs"  "(" Map ")"  [function]

  syntax ValOrJSON ::= Val | JSON

  syntax Exp ::= ValOrJSON
               | "JSON2Obj"     "(" ValOrJSON ")"
               | "constructObj" "(" ValOrJSON ")"
               | "result2Obj"   "(" JSON ")"
               | "JSONs2Obj"    "(" JSONs ")"

  rule Exp2JSON(Name:Id) => Id2String(Name)
  rule Exp2JSON(Name:Id (Args:Vals))
    => { "name": Id2String(Name)
       , "args": [ Exps2JSONs(Args) ] }


  rule Exp2JSON(V:Val) => Val2JSON(V)

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
  rule result2Obj({ "result" : null })                    => undef
  rule result2Obj({ "result" : (({ _:JSONs }) #as Obj )}) => constructObj(Obj)

  rule JSON2Obj(Field : I:Int)
    => var this.String2Id(Field); this.String2Id(Field) = I;

  rule JSON2Obj(Field : S:String)
    => var this.String2Id(Field); this.String2Id(Field) = S;

  rule JSON2Obj(Field : null)
    => var this.String2Id(Field); this.String2Id(Field) = undef;

  rule JSON2Obj(Field : B:Bool)
    => var this.String2Id(Field); this.String2Id(Field) = B;

  rule JSON2Obj(Field : (({ _:JSONs }) #as Obj ))
    => var this.String2Id(Field); this.String2Id(Field) = constructObj(Obj);

  rule JSONs2Obj(J:JSON, Js:JSONs) => JSON2Obj(J) ~> JSONs2Obj(Js)
  rule JSONs2Obj(.JSONs)           => .

  rule <id> SourceId  </id>
       <k> constructObj( { Pairs:JSONs } ) => wait ... </k>
       ( .Bag =>  <instance>
                    <k> JSONs2Obj(Pairs) ~> releaseExecutor </k>
                    <id> Loc </id>
                    <callerId> SourceId </callerId> ...
                  </instance> )
       <nextLoc> Loc:Int => Loc +Int 1 </nextLoc>
       <store> (.Map => (Loc |-> instance(Loc))) ... </store>
```

#### Infrastructure for Foreign Machines

MediK programs often need to interact with external agents, like
GUIs and sensors. These agents are represented as *interfaces* or *foreign
machines*, i.e. machines with transition systems *external* to the MediK program.

```concrete
  rule <id> SourceId </id>
       <k> createFromInterface( IName, FId ) => wait ... </k>
       ( .Bag =>  <instance>
                    <id> Loc </id>
                    <k> InterfaceDecls ~> releaseExecutor </k>
                    <class> IName </class>
                    <callerId> SourceId </callerId>
                    <foreignId> FId </foreignId> ...
                  </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <activeInstances> ... (.Set => SetItem(Loc)) </activeInstances>
       <interface>
        <interfaceName> IName </interfaceName>
        <interfaceDeclarations> InterfaceDecls </interfaceDeclarations> ...
       </interface>
       <foreignInstances> _ => true </foreignInstances>


  rule <k> send instance(Id) , EventName:ExtId , ( Args ) ;
        => jsonWrite( { "id"        : FId
                      , "tid"       : TId
                      , "interface" : Exp2JSON(IName)
                      , "name"      : Exp2JSON(EventName)
                      , "args"      : [Exps2JSONs(Args)] }, #stdout) ...
       </k>
       <instance>
        <id> Id </id>
        <class> IName </class>
        <foreignId> FId </foreignId> ...
       </instance>
       <interfaceName> IName </interfaceName>
       <tidCount> TId => TId +Int 1 </tidCount>

  syntax Exp  ::= "JSON2Exp"   "(" JSON ")"    [function]
  syntax Exps ::= "JSONs2Exps" "(" JSONs ")"   [function]

  rule JSONs2Exps(J:JSON , Js:JSONs)
    => JSON2Exp(J) , JSONs2Exps(Js)

  rule JSONs2Exps(.JSONs) => .Exps

  rule JSON2Exp(I:Int)       => I
  rule JSON2Exp(B:Bool)      => B
  rule JSON2Exp({ _ } #as J) => constructObj(J)

  rule JSON2Exp(S:String) => S
    requires  (findChar(S, "<", 0) ==Int -1)
      andBool (rfindString(S, ">Rat", 0) ==Int -1)
  rule JSON2Exp(S:String)
    =>     String2Int(substrString(S, 1                         , findChar(S, ",", 0)))
      /Rat String2Int(substrString(S, findChar(S, ",", 0) +Int 1, lengthString(S) -Int lengthString(">Rat")))
    [owise]

  rule JSON2Exp(null)     => undef
```

### External Message Handling

The *ExternHandler* instance is scheduled as any other machine.
It reads the read end of the input buffer, and places the message
in the appropriate input queue.

```concrete

  syntax KItem ::= "readExternInput"
                 | "processExternInput" "(" IOJSON ")"

  rule <instance>
        <id> Id </id>
        <k>    readExternInput
         =>    processExternInput(jsonRead(#stdin))
            ~> releaseExecutor
            ~> readExternInput ... </k> ...
       </instance>
       <externInstanceId> Id </externInstanceId>
       <foreignInstances> true </foreignInstances>
       <executorAvailable> true => false </executorAvailable> [priority(300)]

  rule processExternInput([J:JSON , Js:JSONs])
    => processExternInput(J) ~> processExternInput([ Js ])

  rule processExternInput([ .JSONs ]) => .

  rule <k> processExternInput({ "action" : "exit" , _:JSONs }) ~> _  => releaseExecutor </k>
  rule <k> processExternInput(#EOF) ~> _ => releaseExecutor </k>

  rule  <instance>
          <k> processExternInput({ "id"       : IId
                                 , "action"    : "broadcast"
                                 , "eventName" : EName:String
                                 , "eventArgs" : [ Args:JSONs ]
                                 , _:JSONs })
          => broadcast String2Id(EName), (JSONs2Exps(Args));  ...
          </k> ...
       </instance>
       <instance>
        <foreignId> IId </foreignId> ...
       </instance>

  syntax Id ::= "createUpdateStateEvent" "(" Id "," String ")" [function]

  rule createUpdateStateEvent(IName, FNameStr)
    => String2Id(Id2String(IName) +String "_" +String FNameStr +String "_update")


  rule  <instance>
          <k> processExternInput({ "id"       : IId
                                 , "action"    : "updateField"
                                 , "fieldName" : FNameStr:String
                                 , "fieldVal"  : NewVal:JSON
                                 , _:JSONs } )
          =>  broadcast createUpdateStateEvent(IName, FNameStr) ; ...
          </k> ...
       </instance>
       <instance>
        <class> IName </class>
        <foreignId> IId </foreignId>
        <genv> (String2Id(FNameStr) |-> Loc) ... </genv> ...
       </instance>
       <store> (Loc |-> (_ => JSON2Exp(NewVal))) ... </store>

  rule  <instance>
          <k> processExternInput(({ "tid"       : TId:Int
                                  , "id"        : _:String
                                  , "result"    : "obtainResponse"
                                  , "args"      : Val:JSON , _:JSONs  }) ) => . ...
          </k> ...
       </instance>
       <instance>
          <k> waitForObtainResponse(TId) ... </k>
          <inBuffer>
            (.List => ListItem(event($ObtainResponse | { JSON2Exp(Val) }:>Val | Epoch ))) ...
          </inBuffer> ...
       </instance>
       <epoch> Epoch </epoch>

  rule  <instance>
          <k> processExternInput(({ "action" : "sleepResponse"
                                  , "tid"    : TId:Int , _:JSONs }) ) => . ...
          </k> ...
       </instance>
       <instance>
          <k> waitForSleepResponse(TId) ... </k>
          <inBuffer>
            (.List => ListItem(event($SleepDone | .Vals | Epoch ))) ...
          </inBuffer> ...
       </instance>
       <epoch> Epoch </epoch>

```
#### Sleeps

A sleep is accomplished by sending a message to an external process
that responds when the sleep is done.

```concrete

  rule <k> sleep(Duration:Int) ;
        =>    jsonWrite( { "action"   : "sleep"
                         , "duration" : Duration
                         , "tid"      : TId }
                       , #stdout )
           ~> releaseExecutor
           ~> waitForSleepResponse(TId) ...
       </k>
       <foreignInstances> _ => true </foreignInstances>
       <tidCount> TId => TId +Int 1 </tidCount>
```

```k
  syntax KItem ::= "waitForSleepResponse" "(" tid: Int ")"

  rule <k> waitForSleepResponse(_) => . ... </k>
       <inBuffer>
        (ListItem(event($SleepDone | _ | Scheduled )) => .List)  ...
       </inBuffer>
       <executorAvailable> true => false </executorAvailable>
       <epoch> Current </epoch>
         requires Scheduled <=Int Current

  rule <instances>
       (<instance>
          <k> exit ... </k>
          ...
        </instance> _ ) => .Bag
       </instances>
       <store> _ => .Map </store> [priority(170)]

```

#### Symbolic Sleep

We assume a discrete model of time, i.e., a time-epoch is a
Nat.

Upon encountering a sleep, the machine's Tid is added to
a list of sleeping machines, and the control is ceeded.

```{.mcheck .symbolic}

  syntax KItem ::= "doSleep" "(" duration: Int ")"

  rule <id> Id </id>
       <k> sleep(N); => releaseExecutor ~> doSleep(N) ... </k>
       <sleeping> ... (.List => ListItem(Id)) </sleeping>

```

If no "instantaneous" rule can apply, the sleeps are
processed, until at least one machine's sleep is completed.

```{.mcheck .symbolic}

  rule <sleeping> Sleeping </sleeping>
       <slept> .List </slept>
       <executorAvailable> true => false </executorAvailable>
        requires Sleeping =/=K .List                          [priority(300)]

  rule <k> doSleep(N => N -Int 1) ... </k>
       <id> Id </id>
       <sleeping> ListItem(Id) => .List ... </sleeping>
       <slept> ... (.List => ListItem(Id)) </slept>
       <executorAvailable> false </executorAvailable>
        requires N >Int 0                                     [priority(300)]

  rule <k> doSleep(0) => waitForSleepResponse(-1) ... </k>
       <id> Id </id>
       <inBuffer> (.List => ListItem(event($SleepDone | .Vals | Epoch +Int 1))) ... </inBuffer>
       <sleeping> ListItem(Id) => .List ... </sleeping>
       <executorAvailable> false </executorAvailable>
       <epoch> Epoch </epoch>
       <shouldAdvanceEpoch> _ => true </shouldAdvanceEpoch>

  rule <sleeping> .List => Slept </sleeping>
       <slept> Slept => .List </slept>
       <executorAvailable> false => true </executorAvailable>
        requires Slept =/=K .List

  rule <sleeping> .List </sleeping>
       <slept> .List </slept>
       <executorAvailable> false => true </executorAvailable> [priority(300)]

```

#### Non-deterministic choice

```{.mcheck .symbolic}
  rule either B or _ => B
  rule either _ or B => B
```

#### Skolemized Variable

```{.mcheck}
  rule #nondet + _:Val     => #nondet
  rule _:Val     + #nondet => #nondet

  rule #nondet - _:Val     => #nondet
  rule _:Val     - #nondet => #nondet

  rule #nondet * _:Val     => #nondet
  rule _:Val     * #nondet => #nondet

  rule #nondet / _:Val     => #nondet
  rule _:Val     / #nondet => #nondet

  rule #nondet > _:Val     => #nondet
  rule _:Val     > #nondet => #nondet

  rule #nondet < _:Val     => #nondet
  rule _:Val     < #nondet => #nondet

  rule #nondet >= _:Val     => #nondet
  rule _:Val     >= #nondet => #nondet

  rule #nondet <= _:Val     => #nondet
  rule _:Val     <= #nondet => #nondet

  rule ! #nondet => #nondet

  rule #nondet && _         => #nondet
  rule #nondet || _         => #nondet


  rule if (#nondet) Block => Block
  rule if (#nondet) _     => .

  rule if (#nondet) Block else _      => Block
  rule if (#nondet) _     else Block  => Block
```

#### Simple Functions For Conversions

```k
  rule parseInt(S:String) => String2Int(S)

endmodule
```

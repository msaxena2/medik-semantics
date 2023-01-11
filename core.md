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

  syntax Ids ::= List{Id, ","}    [klabel(ids)]

  syntax Val  ::= Int | Bool | String
  syntax Vals ::= List{Val, ","}  [klabel(exps)]

  syntax FloatLiteral ::= r"([\\+-]?[0-9]+(\\.[0-9]*)?|\\.[0-9]+)" [token, prec(1)]

  syntax StandaloneExp ::= "new" Id "(" Exps ")"                       [strict(2)]
                         | "createFromInterface" "(" Id "," String ")" [strict(2)]
                         | Id "(" Exps ")"                             [strict(2)]
                         | "exit"
  syntax Exp ::= Id
               | Val
               | Rat
               | FloatLiteral
               | "this"
               | "obtainFrom" "(" Exp "," Exp ")"            [strict]
               | "(" Exp ")"                                 [bracket]
               > Exp "." Exp                                 [strict(1), left]
               > Exp "+" Exp                                 [strict, left]
               | Exp "-" Exp                                 [strict, left]
               | Exp "*" Exp                                 [strict, left]
               | Exp "/" Exp                                 [strict, left]
               | Exp ">" Exp                                 [strict, left]
               | Exp "<" Exp                                 [strict, left]
               | Exp ">=" Exp                                [strict, left]
               | Exp "<=" Exp                                [strict, left]
               | "!" Exp                                     [strict, left]
               | Exp "&&" Exp                                [strict(1), left]
               | Exp "||" Exp                                [strict, left]
               > Exp "==" Exp                                [strict, left]
               | "interval" "(" Exp "," Exp ")"
               > Exp "in" Exp
               | "parseInt" "(" Exp ")"                      [strict]
               | StandaloneExp

  syntax Exps ::= List{Exp, ","}  [strict, klabel(exps), avoid]
                | Vals

  syntax Stmt ::= StandaloneExp ";"                               [strict]
                | "sleep" "(" Exp ")" ";"                         [strict(1)]
                | "send" Exp "," Id ";"                           [macro]
                | "send" Exp "," Id "," "(" Exps ")" ";"          [strict(1, 3)]
                | "broadcast" Id ";"                              [macro]
                | "broadcast" Id "," "(" Exps ")" ";"             [strict(2)]
                | "goto" Id ";"                                   [macro]
                | "goto" Id "(" Exps ")" ";"                      [strict(2)]
                | "print" "(" Exp ")" ";"                         [strict]
                > "return" ";"
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
                | "on" Id "do" Block                              [macro]
                | "on" Id "(" Ids ")" "do" Block
                | "fun" Id "(" Ids ")" Block
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
endmodule
```
### Macros

Rewrite rules for `macro` constructs

```k
module MEDIK-SYNTAX-EXT
  imports MEDIK-SYNTAX

  rule vars I1:Id, I2:Id, Is:Ids; => var I1; vars I2, Is;
  rule vars I, .Ids; => var I;
  rule var I:Id = E:Exp ; => var I; I = E;
  rule entry B:Block => entry (.Ids) B
  rule on E:Id do B:Block => on E (.Ids) do B
  rule send Id , Event; => send Id, Event, ( .Vals );
  rule goto State:Id; => goto State ( .Vals );
  rule machine Name:Id Code
    => machine Name receives .Ids Code
  rule init machine Name:Id Code
    => init machine Name receives .Ids Code
  rule broadcast Event; => broadcast Event, ( .Vals );
  rule interface Name:Id Code
    => interface Name receives .Ids Code

  syntax Stmt ::= "nothing" ";"

  rule { } => { nothing; }

endmodule
```

Core Semantics
--------------

Here we define semantics of core medik constructs.

```k
module MEDIK-CORE
  imports MEDIK-SYNTAX-EXT
  imports DOMAINS
  imports JSON
  imports K-REFLECTION
  imports RAT
  imports RAT-COMMON

  syntax Val  ::= "null" | "undef" | Rat | Bool | String

  syntax KResult ::= Val
                   | Vals
```
```symbolic
  // For some reason, haskell backend doesn't cool ".Vals"
  rule isKResult(.Vals) => true
```
```k
  configuration <core>
                  <activeInstances> ListItem(0) </activeInstances>
                  <store> .Map </store>
                  <nextLoc> 1 </nextLoc>
                  <tidCount> 1 </tidCount>
                  <executorAvailable> true </executorAvailable>
                  <instances>
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
                </core>
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

  rule populateMachineCell(M, S Ss)
    => populateMachineCell(M, S) ~> populateMachineCell(M, Ss)

  rule <k> populateMachineCell(MName, (var _ ;) #as GlobalDecl) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> S => {S GlobalDecl}:>Stmt </declarationCode> ...
       </machine>

  rule <k> populateMachineCell(MName, (_ = _;) #as GlobalDecl) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> S => {S GlobalDecl}:>Stmt </declarationCode> ...
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
          <stateDeclarations> S:Stmt => {S StateDecl}:>Stmt </stateDeclarations> ...
        </state> ...
       </machine>

  rule <k> populateStateCell(MName, SName, (_ = _;) #as StateDecl) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S:Stmt => {S StateDecl}:>Stmt </stateDeclarations> ...
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
          <eventHandlers>
            (.Bag => <eventHandler>
                      <eventId> Event </eventId>
                      <eventArgs> EventArgs </eventArgs>
                      <handlerCode> HandlerCode </handlerCode> ...
                     </eventHandler> ) ...
          </eventHandlers> ...
        </state> ...
      </machine>

```

```k
  syntax KItem ::= "createInitInstances"
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

  rule I1 == I2 => I1 ==Rat I2
  rule S1 == S2 => S1 ==String S2
  rule B1 == B2 => B1 ==Bool B2

  rule undef == undef => true

  rule X == undef => false
    requires X =/=K undef

  rule undef == X => false
    requires X =/=K undef
```

#### Instance creation via new

When a new instance is created, the instance cell is added and
global environment initialized. The new instance's `init` state
is scheduled. The *caller* does not give up control.

```k
  syntax KItem ::= "unblockInstance" "(" instanceId: Int ")"
                 | "wait"

  rule <instance>
        <id> SourceId </id>
        <k> wait => instance(TargetId) ... </k>  ...
       </instance>
       <instance>
        <id> TargetId </id>
        <k> unblockInstance(SourceId) => . ... </k> ...
       </instance>

  rule <id> SourceId </id>
       <k> new MName (Args) => wait ... </k>
       ( .Bag => <instance>
                  <id> Loc </id>
                  <k> asGlobalDecls(MachineDecls)
                   ~> unblockInstance(SourceId)
                   ~> enterState(InitState, Args)
                  </k>
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
       <activeInstances> ... (.List => ListItem(Loc)) </activeInstances>

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

  rule <k> releaseExecutor => . ... </k>
       <executorAvailable> _ => true </executorAvailable>
```

#### Entering States

```k
  syntax KItem ::= "enterState" "(" stateName: Id "," entryArgs: Vals ")"

  rule <k>   enterState(SName, Args)
        =>   StateDecls
          ~> recordEnv
          ~> assign(BlockVars | Args)
          ~> EntryBlock
          ~> restoreEnv
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
       <inBuffer> (ListItem(eventArgsPair(EventId | Args)) => .List) ... </inBuffer>
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
      <executorAvailable> true => false </executorAvailable>
```

#### Stuck Execution

We assume a machine is `stuck` if the `entry` block
of a state has been executed, i.e., the machine is waiting for
an incoming event, and the event at the head of the queue is
not handled in the machine's active state

```k
  syntax KItem ::= "stuck"

  rule <k> handleEvents ~> _ => stuck </k>
       <activeState> ActiveEvent </activeState>
       <class> MachineName </class>
       <inBuffer> ListItem(eventArgsPair(InputEvent | _)) ... </inBuffer>
    requires notBool (eventIsHandled(MachineName, ActiveEvent, InputEvent))

  syntax Bool ::= "eventIsHandled" "(" machineName: Id "," activeState: Id "," inputEvent: Id ")"  [function]

  rule [[ eventIsHandled( MName, ActiveState, InputEvent) => true ]]
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> ActiveState </stateName>
          <eventHandler>
            <eventId> InputEvent </eventId> ...
          </eventHandler> ...
        </state> ...
       </machine>

  rule eventIsHandled( _, _, _) => false
    [owise]
```

```k

  syntax KItem ::= "asGlobalDecls"   "(" decls: Stmt ")"

  rule asGlobalDecls(S:Stmt Ss:Stmt)
    => asGlobalDecls(S) ~> asGlobalDecls(Ss)
  rule asGlobalDecls(var Id;) => var this . Id;
  rule asGlobalDecls(S:Stmt) => S               [owise]

  syntax KItem ::= "unblockCaller"

  rule <instance>
        <id> SourceId </id>
        <k> wait => instance(TargetId) ... </k> ...
       </instance>
       <instance>
        <id> TargetId </id>
        <k> unblockCaller => . ... </k>
        <callerId> SourceId => . </callerId> ...
       </instance>

  rule <k> unblockCaller => . ... </k>
       <callerId> . </callerId>

  rule yield; => unblockCaller

  syntax KItem ::= "recordEnv"
                 | "restoreEnv"

  rule <k> recordEnv => . ... </k>
       <env> Rho </env>
       <stack> .List => ListItem(Rho) ... </stack>

  rule <k> restoreEnv => . ... </k>
       <env> _ => Rho </env>
       <stack> (ListItem(Rho) => .List ) ... </stack>

  syntax KItem ::= "assign" "(" args: Ids "|" values: Vals ")"

  rule assign(I:Id , Is | V:Val, Vs)
    => var I = V; ~> assign(Is | Vs)

  rule assign( .Ids | .Vals ) => .
```

##### Semantics of goto

Goto results in a context switch. If a caller is waiting on execution,
it is unblocked before the switch occurs.

```k

  rule <k> goto Target:Id ( Args:Vals ) ; ~> _
       => releaseExecutor ~> enterState(Target, Args) </k>
       <env> _ => .Map </env>
       <stack> _ => .List </stack>
```

Ids beginning with a `$` cannot be used in a medik program.
prevents any clashes with user-defined events, we create a new
`ExtendedExps` sort containing some constructors for Ids
prefixed with `$`.

```k
  syntax ExtId ::= Id

  syntax Exp ::= ExtId

```

##### Sending Events
```k


  syntax KItem ::= "eventArgsPair"    "(" eventId: ExtId "|" args: Vals ")"
                 | "performBroadcast" "(" eventId: Id    "|" args: Vals "|" List ")"

  syntax List ::= "getRecievers"    "(" eventId: Id ")"          [function]
  syntax List ::= "getRecieversAux" "(" eventId: Id "|" List ")" [function]

  rule [[ getRecievers(Event) => getRecieversAux(Event | ActiveInstances) ]]
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


  rule getRecieversAux(_     | .List)            => .List
  rule getRecieversAux(Event | ListItem(_) Rest) => getRecieversAux(Event | Rest) [owise]

  rule <instance>
        <k> send instance(Id) , EventName:Id , ( Args ) ; =>  . ... </k>
        ...
       </instance>
       <instance>
        <id> Id </id>
        <class> CName </class>
        <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer> ...
       </instance>
       <machineName> CName </machineName>

  rule <k> send instance(Id) , EventName:Id , ( Args ) ; =>  . ... </k>
       <id> Id </id>
       <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer>
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
        => assign(Args | Vals) ~> FunCode ~> return;
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

  rule <k> return ; ~> _ => Rest </k>
       <env> _ => Rho </env>
       <fstack> (ListItem(fstackItem(Rho | Rest)) => .List) ... </fstack>

  rule <k> return V:Val ; ~> _ => V ~> Rest </k>
       <env> _ => Rho </env>
       <fstack> (ListItem(fstackItem(Rho | Rest)) => .List) ... </fstack>

```

```k
endmodule
```

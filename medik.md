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

  syntax Ids ::= List{Id, ","}    [ids]

  syntax Exps ::= List{Exp, ","}  [strict, klabel(exps), avoid]
  syntax UndefExp ::= "undef"

  syntax Val
  syntax Vals ::= List{Val, ","}  [klabel(exps)]

  syntax ThisExp ::= "this"

  syntax FloatLiteral ::= r"([\\+-]?[0-9]+(\\.[0-9]*)?|\\.[0-9]+)" [token, prec(1)]

  syntax Exp ::= Id
               | Rat
               | FloatLiteral
               | Bool
               | String
               | UndefExp
               | ThisExp
               | "obtainFrom" "(" Exp "," Exp ")"            [strict]
               | "yield"
               | "exit"
               | "(" Exp ")"                                 [bracket]
               | Id "(" Exps ")"                             [strict(2)]
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
               | "sleep" "(" Exp ")"                         [strict(1)]
               | "new" Id "(" Exps ")"                       [strict(2)]
               | "send" Exp "," Id                           [strict(1)]
               | "send" Exp "," Id "," "(" Exps ")"          [strict(1, 3)]
               | Exp "in" Exp
               | "interval" "(" Exp "," Exp ")"              [strict]
               | "broadcast" Id                              [broadcast]
               | "broadcast" Id "," "(" Exps ")"             [strict(2)]
               | "goto" Id
               | "goto" Id "(" Exps ")"                      [strict(2)]
               | "print" "(" Exp ")"                         [strict]
               | "createFromInterface" "(" Id "," String ")" [strict(2)]
               | "parseInt" "(" Exp ")"                      [strict]
               > "return"
               | "return" Exp                                [strict(1)]
               > Exp "=" Exp                                 [strict(2)]
               > "var" Id
               | "vars" Ids
               | "var" Exp "=" Exp

  syntax Stmt ::= Exp ";"                                  [strict]
                | Block
                > "if" "(" Exp ")" Block                   [strict(1)]
                | "if" "(" Exp ")" Block "else" Block      [strict(1)]
                | "while" "(" Exp ")" Block
                | "entry" Block
                | "entry" "(" Ids ")" Block
                | "on" Id "do" Block
                | "on" Id "(" Ids ")" "do" Block
                | "fun" Id "(" Ids ")" Block
                | Exp "in" "{" CaseDecl "}"
                | StateDecl
                > "machine" Id Block
                | "machine" Id "receives" Ids Block
                | "interface" Id Block
                | "interface" Id "receives" Ids Block
                | "init" "machine" Id Block
                | "init" "machine" Id "receives" Ids Block
                > Stmt Stmt                                [right]

  syntax StateDecl ::= "state" Id Block
                     | "init" "state" Id Block

  syntax CaseDecl ::= Exp ":" Stmt
                    | CaseDecl "default" ":" Stmt
                    > CaseDecl CaseDecl                    [right]

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
  imports RAT
  imports RAT-COMMON

  syntax Val ::= "null" | "nothing" | Rat | Bool | String | UndefExp
  syntax Exp ::= Val
  syntax Exps ::= Vals

  syntax KResult ::= Val | Vals

  syntax KItem ::= "createMachineDefs" "(" Stmt ")"
                 | "closeForeignFds"

  syntax Id ::= "$Main"

  configuration <instances>
                  <instance multiplicity="*" type="Map">
                    <id> 0 </id>
                    <k> createMachineDefs($PGM:Stmt) ~> createInitInstances </k>
                    <env> .Map </env>
                    <genv> .Map </genv>
                    <class> $Main </class>
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
                    <machineName> $Main </machineName>
                    <declarationCode> nothing; </declarationCode>
                    <isInitMachine> false </isInitMachine>
                    <receiveEvents> .Set </receiveEvents>
                    <states>
                      <state multiplicity="*" type="Map">
                        <stateName> . </stateName>
                        <stateDeclarations> nothing; </stateDeclarations>
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
                <activeInstances> ListItem(0) </activeInstances>
                <store> .Map </store>
                <nextLoc> 1 </nextLoc>
                <foreignInstances> false </foreignInstances>
                <tidCount> 1 </tidCount>
                <externInstanceId> . </externInstanceId> // Hack until k is fixed
```
### Macros

```k
  rule vars I1:Id, I2:Id, Is:Ids; => var I1; vars I2, Is;       [macro-rec]
  rule vars I, .Ids; => var I;                                  [macro]
  rule var I:Id = E:Exp ; => var I; I = E;                      [macro]
  rule entry B:Block => entry (.Ids) B                          [macro]
  rule on E:Id do B:Block => on E (.Ids) do B                   [macro]
  rule send Id , Event => send Id, Event, ( .Vals )             [macro]
  rule goto State:Id => goto State ( .Vals )                    [macro]
  rule machine Name:Id Code
    => machine Name receives .Ids Code                          [macro]
  rule init machine Name:Id Code
    => init machine Name receives .Ids Code                     [macro]
  rule broadcast Event => broadcast Event, ( .Vals )            [macro]
  rule while (Cond) Block
   => if (Cond) { Block while (Cond) Block }                    [structural]
  rule E in interval(L, R) => (E >= L) && (E < R)               [macro]
  rule E in { interval(L, U): S:Stmt Cs:CaseDecl }
    => if (E in interval(L, U)) { S } else { E in { Cs } }      [macro-rec]
  rule E in { interval(L, U): S:Stmt }
    => if (E in interval(L, U)) { S }                           [macro]
  rule E in { interval(L, U): S1:Stmt default: S2:Stmt }
    => if (E in interval(L, U)) { S1 } else { S2 }              [macro]
  rule interface Name:Id Code
    => interface Name receives .Ids Code                        [macro]
  rule interface _ receives _ ( { } => { nothing; } )           [macro]
```

### Machine Template Creation

```k

  syntax KItem ::= "createTransitionSystem"  "(" machineName: Id "," code: Stmt ")"
                 | "createDeclarationCode"   "(" machineName: Id "," code: Stmt ")"
                 | "createEventHandlers"     "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createStateDeclarations" "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createEntryBlock"        "(" machineName: Id "," stateName: Id "," code: Stmt ")"
                 | "createInstance"          "(" machineName: Id ")"
                 | "createInitInstances"

  syntax Set ::= "asSet" "(" Ids ")" [function]

  rule asSet(I:Id, Is:Ids) => SetItem(I) asSet(Is)
  rule asSet(.Ids)         => .Set

  rule createMachineDefs(S Ss) => createMachineDefs(S) ~> createMachineDefs(Ss)
  rule <k> createMachineDefs(machine Name receives InEvents ({ Code } #as CodeBlock:Block))
        => createDeclarationCode(Name, Code) ~> createTransitionSystem(Name, CodeBlock) ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName>
                      <receiveEvents> asSet(InEvents) </receiveEvents> ...
                    </machine> ) ...
       </machines>

  rule <k> createMachineDefs(init machine Name receives InEvents ({ Code } #as CodeBlock))
        => createDeclarationCode(Name, Code) ~> createTransitionSystem(Name, CodeBlock) ... </k>
       <machines>
         ( .Bag =>  <machine>
                      <machineName> Name </machineName>
                      <receiveEvents> asSet(InEvents) </receiveEvents>
                      <isInitMachine> true </isInitMachine> ...
                    </machine> ) ...
       </machines>

  rule <k> createMachineDefs(interface Name receives InEvents { Code })
        => createDeclarationCode(Name, Code) ... </k>
       <interfaces>
         ( .Bag =>  <interface>
                      <interfaceName> Name </interfaceName>
                      <interfaceReceiveEvents> asSet(InEvents) </interfaceReceiveEvents> ...
                    </interface> ) ...
       </interfaces>

  rule createDeclarationCode(Name, S Ss)
    => createDeclarationCode(Name, S) ~> createDeclarationCode(Name, Ss)

  rule <k> createDeclarationCode(Name, fun FunName:Id ( Args ) Block) => . ...  </k>
       <machine>
        <machineName> Name </machineName>
        <functions> ( .Bag => <function>
                                <functionName> FunName </functionName>
                                <functionArgs> Args </functionArgs>
                                <functionCode> Block </functionCode> ...
                              </function> ) ...
        </functions> ...
       </machine>

  rule <k> createDeclarationCode(Name, (var _ ;) #as S2 ) => . ... </k>
       <interface>
        <interfaceName> Name </interfaceName>
        <interfaceDeclarations> S1:Stmt => { S1 S2 }:>Stmt </interfaceDeclarations> ...
       </interface>

  rule createDeclarationCode(_, _) => . [owise]

  rule <k> createDeclarationCode(Name, (var _ ;) #as S2) => . ... </k>
       <machine>
        <machineName> Name </machineName>
        <declarationCode> S1:Stmt => { S1 S2 }:>Stmt </declarationCode> ...
       </machine>

  rule <k> createDeclarationCode(Name, (_ = _ ;) #as S2) => . ... </k>
       <machine>
        <machineName> Name </machineName>
        <declarationCode> S1:Stmt => { S1 S2 }:>Stmt </declarationCode> ...
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

  rule <k> createStateDeclarations(MName, SName, (var _:Id ;) #as S2:Stmt) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S1:Stmt => {S1 S2}:>Stmt </stateDeclarations> ...
        </state> ...
      </machine>


  rule <k> createStateDeclarations(MName, SName, (_ = _ ;) #as S2:Stmt) => . ... </k>
       <machine>
        <machineName> MName </machineName>
        <state>
          <stateName> SName </stateName>
          <stateDeclarations> S1:Stmt => {S1 S2}:>Stmt </stateDeclarations> ...
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

  syntax KItem ::= "createMainInstance"
                 | "createExternHandlerInstance"
                 | "readExternInput"
                 | "processExternInput" "(" IOJSON ")"

  syntax Id    ::= "$ExternHandler"

  rule createInitInstances => createExternHandlerInstance ~> createMainInstance

  rule <k> createMainInstance => new InitMName ( .Vals ); ... </k>
       <machine>
        <machineName> InitMName </machineName>
        <isInitMachine> true </isInitMachine> ...
       </machine>

  rule <k> createExternHandlerInstance => . ... </k>
       (.Bag =>  <instance>
                  <id> Loc </id>
                  <k> readExternInput </k>
                  <class> $ExternHandler </class> ...
                 </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> (.Map => (Loc |-> instance(Loc))) ... </store>
       <externInstanceId> _ => Loc </externInstanceId>

```

### Expression and Statement

#### Variable Assignment and Lookup
```k
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
                                               , lengthString(FL) )
                                   )
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

```k
  syntax KItem ::= "execEntryCode"  "(" stateName: Id "," entryArgs: Vals ")"
                 | "asGlobalDecls"  "(" decls: Stmt ")"
                 | "execEntryBlock" "(" Vals ")"
                 | "unblockCaller"
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
                     ~> unblockCaller
                     ~> execEventHandlers </k>
                    <class> MName </class>
                    <callerId> SourceId </callerId> ...
                   </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <activeInstances> ... (.List => ListItem(Loc)) </activeInstances>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> MachineDecls </declarationCode>
        <state>
          <stateName> InitState </stateName>
          <isInitState> true </isInitState> ...
        </state> ...
       </machine>

  // Default constructor/state if no init state present
  syntax Id ::= "$Default"

  rule  <id> SourceId </id>
        <k> new MName ( .Vals ) => wait ... </k>
        ( .Bag => <instance>
                    <id> Loc </id>
                    <k> asGlobalDecls(MachineDecls)
                     ~> unblockCaller
                     ~> execEventHandlers </k>
                    <class> MName </class>
                    <activeState> $Default </activeState>
                    <callerId> SourceId </callerId> ...
                   </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <activeInstances> ... (.List => ListItem(Loc)) </activeInstances>
       <machine>
        <machineName> MName </machineName>
        <declarationCode> MachineDecls </declarationCode> ...
       </machine>                                                       [owise]

  rule asGlobalDecls(S:Stmt Ss:Stmt)
    => asGlobalDecls(S) ~> asGlobalDecls(Ss)
  rule asGlobalDecls(var Id;) => var this . Id;
  rule asGlobalDecls(S:Stmt) => S               [owise]

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

  rule yield => unblockCaller ~> nothing

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
##### Semantics of goto

Goto results in a context switch. If a caller is waiting on execution,
it is unblocked before the switch occurs.

```k
  syntax KItem ::= "clearEnv"

  rule <k> goto Target:Id ( Args:Vals ) ~> _
       =>    unblockCaller
          ~> recordEnv
          ~> execEntryCode( Target , Args )
          ~> execEventHandlers </k>
       <env> _ => .Map </env>
       <stack> _ => .List </stack>
```
#### Event Handling

##### Sending Events
```k
  syntax Val ::= "done"
  syntax KItem ::= "eventArgsPair"    "(" eventId: Id "|" args: Vals ")"
                 | "performBroadcast" "(" eventId: Id "|" args: Vals "|" List ")"

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
        <k> send instance(Id) , EventName:Id , ( Args ) =>  done ... </k>
        ...
       </instance>
       <instance>
        <id> Id </id>
        <class> CName </class>
        <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer> ...
       </instance>
       <machineName> CName </machineName>

  rule <k> send instance(Id) , EventName:Id , ( Args ) =>  done ... </k>
       <id> Id </id>
       <inBuffer> ... (.List => ListItem( eventArgsPair(EventName | Args ))) </inBuffer>

  rule broadcast EventName:Id , ( Args )
    => performBroadcast ( EventName | Args | getRecievers(EventName))

  rule performBroadcast ( EventName | Args | ListItem(Id) Recievers)
    =>   send instance(Id), EventName, ( Args ) ;
      ~> performBroadcast ( EventName | Args | Recievers)

  rule performBroadcast(_ | _ | .List) => done
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

##### Print

We treat printing as sending an event to an implicit "tty"
external machine
```k

  syntax JSON ::= "Val2JSON" "(" Val ")" [function]

  rule Val2JSON(<I1, I2>Rat)
    => "<" +String Int2String(I1) +String "," +String Int2String(I2) +String ">Rat"
  rule Val2JSON(I:Int)      => I
  rule Val2JSON(S:String)   => S
  rule Val2JSON(undef)      => "undef"
  rule Val2JSON(B:Bool)     => Bool2String(B)

  rule <k> print(V:Val)
        => jsonWrite( { "action" : "print"
                      , "args"   : [Val2JSON(V)] }
                    , #stdout) ~> V ...
       </k>
       <tidCount> TId => TId +Int 1 </tidCount>

```

#### If/While

```k
  rule if (true) Block => Block
  rule if (false) _ => .

  rule if (true) Block else _  => Block
  rule if (false) _ else Block => Block
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

  rule <k> return ~> _ => done ~> Rest </k>
       <env> _ => Rho </env>
       <fstack> (ListItem(fstackItem(Rho | Rest)) => .List) ... </fstack>

  rule <k> return V:Val ~> _ => V ~> Rest </k>
       <env> _ => Rho </env>
       <fstack> (ListItem(fstackItem(Rho | Rest)) => .List) ... </fstack>

```

#### Semantics of Obtain

The `obtainFrom` keyword provides a mechanism in medik to fetch values from an external
source at runtime.

```k
  syntax KItem ::= "waitForObtainResponse" "(" Exp ")"

  rule <k> obtainFrom(instance(Id:Int), Field:String)
        =>   jsonWrite( { "id"        : FId
                        , "tid"       : TId
                        , "interface" : Exp2JSON(IName)
                        , "name"      : "Obtain"
                        , "args"      : [ Field , .JSONs ] }, #stdout)
          ~> waitForObtainResponse(TId) ...
       </k>
       <instance>
        <id> Id </id>
        <class> IName </class>
        <foreignId> FId </foreignId> ...
       </instance>
       <interfaceName> IName </interfaceName>
       <tidCount> TId => TId +Int 1 </tidCount>
```

#### IPC via extern

```k
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

  rule Exp2JSON(S:String) => S
  rule Exp2JSON(I:Int)    => I
  rule Exp2JSON(B:Bool)   => B
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

  syntax Id ::= "$Dynamic"

  rule <id> SourceId  </id>
       <k> constructObj( { Pairs:JSONs } ) => wait ... </k>
       ( .Bag =>  <instance>
                    <k> JSONs2Obj(Pairs) ~> unblockCaller </k>
                    <id> Loc </id>
                    <class> $Dynamic </class>
                    <callerId> SourceId </callerId> ...
                  </instance> )
       <nextLoc> Loc:Int => Loc +Int 1 </nextLoc>
       <store> (.Map => (Loc |-> instance(Loc))) ... </store>
```

#### Infrastructure for Foreign Machines

MediK programs often need to interact with external agents, like
GUIs and sensors. These agents are represented as *interfaces* or *foreign
machines*, i.e. machines with transition systems *external* to the MediK program.

```k
  rule <id> SourceId </id>
       <k> createFromInterface( IName, FId ) => wait ... </k>
       ( .Bag =>  <instance>
                    <id> Loc </id>
                    <k> asGlobalDecls(InterfaceDecls) ~> unblockCaller </k>
                    <class> IName </class>
                    <activeState> $Default </activeState>
                    <callerId> SourceId </callerId>
                    <foreignId> FId </foreignId> ...
                  </instance> )
       <nextLoc> Loc => Loc +Int 1 </nextLoc>
       <store> ( .Map => (Loc |-> instance(Loc))) ... </store>
       <activeInstances> ... (.List => ListItem(Loc)) </activeInstances>
       <interface>
        <interfaceName> IName </interfaceName>
        <interfaceDeclarations> InterfaceDecls </interfaceDeclarations> ...
       </interface>
       <foreignInstances> _ => true </foreignInstances>


  rule <k> send instance(Id) , EventName:Id , ( Args )
        => jsonWrite( { "id"        : FId
                      , "tid"       : TId
                      , "interface" : Exp2JSON(IName)
                      , "name"      : Exp2JSON(EventName)
                      , "args"      : [Exps2JSONs(Args)] }, #stdout) ~> done ...
       </k>
       <instance>
        <id> Id </id>
        <class> IName </class>
        <foreignId> FId </foreignId> ...
       </instance>
       <interfaceName> IName </interfaceName>
       <tidCount> TId => TId +Int 1 </tidCount>

  syntax JSON ::= "json-undef" [klabel(JSON-RPCundef), symbol]
  syntax IOJSON ::= IOError | JSON

  syntax IOJSON ::= "jsonRead" "(" Int ")"            [function, hook(JSON.read)]
                  | "jsonWriteError" "(" JSON ")"     [klabel(JSON-RPC_putResponseError), symbol]

  syntax K ::= "jsonWrite" "(" JSON "," Int ")"       [function, hook(JSON.write)]

  syntax Exp  ::= "JSON2Exp"   "(" JSON ")"    [function]
  syntax Exps ::= "JSONs2Exps" "(" JSONs ")"   [function]

  rule JSONs2Exps(J:JSON , Js:JSONs)
    => JSON2Exp(J) , JSONs2Exps(Js)

  rule JSONs2Exps(.JSONs) => .Exps

  rule JSON2Exp(I:Int)       => I
  rule JSON2Exp(S:String)    => S
  rule JSON2Exp({ _ } #as J) => constructObj(J)

  rule  readExternInput => doRead

  rule <instance>
        <id> Id </id>
        <k> doRead => processExternInput(jsonRead(#stdin)) ~> readExternInput ... </k> ...
       </instance>
       <externInstanceId> Id </externInstanceId>
       <foreignInstances> true </foreignInstances>  [priority(300)]

  rule processExternInput([J:JSON , Js:JSONs])
    => processExternInput(J) ~> processExternInput([ Js ])

  rule processExternInput([ .JSONs ]) => .

  rule <k> processExternInput({ "action" : "exit" , _:JSONs }) ~> _  => done </k>


  syntax KItem ::= "doRead"

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
          =>  broadcast createUpdateStateEvent(IName, FNameStr) ...
          </k> ...
       </instance>
       <instance>
        <class> IName </class>
        <foreignId> IId </foreignId>
        <genv> (String2Id(FNameStr) |-> Loc) ... </genv> ...
       </instance>
       <store> (Loc |-> (_ => JSON2Exp(NewVal))) ... </store>

  syntax KItem ::= JSON2Val(JSON) [function]

  rule JSON2Val(I:Int)    => I
  rule JSON2Val(B:Bool)   => B
  rule JSON2Val(S:String) => S
    requires  (findChar(S, "<", 0) ==Int -1)
      andBool (rfindString(S, ">Rat", 0) ==Int -1)
  rule JSON2Val(S:String)
    =>     String2Int(substrString(S, 1                         , findChar(S, ",", 0)))
      /Rat String2Int(substrString(S, findChar(S, ",", 0) +Int 1, lengthString(S) -Int lengthString(">Rat")))
    [owise]

  rule JSON2Val(null)     => undef

  rule  <instance>
          <k> processExternInput({ "tid"       : TId:Int
                                 , "id"        : _:String
                                 , "result"    : "obtainResponse"
                                 , "args"      : Val:JSON , _:JSONs  }) => . ...
          </k> ...
       </instance>
       <instance>
        <k> waitForObtainResponse(TId) => JSON2Val(Val) ... </k> ...
       </instance>
```
#### Timer Hooks

A simple hook to make the process wait

```k
  syntax KItem ::= "waitForSleepResponse" "(" tid: Int ")"

  rule <k> sleep(Duration:Int)
        => jsonWrite( { "action"   : "sleep"
                      , "duration" : Duration
                      , "tid"      : TId }
                    , #stdout ) ~> waitForSleepResponse(TId) ...
       </k>
       <foreignInstances> _ => true </foreignInstances>
       <tidCount> TId => TId +Int 1 </tidCount>

  rule <instance>
        <k> waitForSleepResponse(TId) => done ... </k> ...
       </instance>
       <instance>
        <k> processExternInput({ "action" : "sleepResponse"
                               , "tid"    : TId
                               , _:JSONs })
         => . ...
        </k> ...
       </instance>

  rule <instances>
       (<instance>
          <k> exit ... </k>
          ...
        </instance> _ ) => .Bag
       </instances>

```
#### Simple Functions For Conversions

```k
  rule parseInt(S:String) => String2Int(S)

endmodule
```

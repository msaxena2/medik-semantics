```k
requires "./jsonRpc.md"

module MEDIK-CORE-SYNTAX
  imports JSON-EXT
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ""}


  syntax FlowChart ::= "flowchart" ":" Decls "end" [format(%1%2%n%i%3%d%n%4)]
                     | "setInputBuffer" "(" JSONs ")"
  syntax FlowCharts ::= List{FlowChart, ""}

  syntax Decls ::= List{Decl, ""} [format(%1%n%3)]

  syntax Decl ::= "name"           ":" Id
                | "state"          ":" Ids
                | "variable"       ":" Ids
                | InitializationDecl
                | TransitionDecl

  syntax ActionDecl ::= "action" ":" Actions

  syntax InitializationDecl ::= "initialization" ":" Id
                              | "initialization" ":" Id ActionDecl

  syntax TransitionDecl ::= "transition" ":" Transition
                            "event"      ":" Event
                          | "transition" ":" Transition
                            "event"      ":" Event
                            "action"     ":" Actions
                          | "transition" ":" Transition
                            "event"      ":" Event
                            "condition"  ":" Exps
                          | "transition" ":" Transition
                            "event"      ":" Event
                            "condition"  ":" Exps
                            "action"     ":" Actions
  syntax TransitionDecls ::= List{TransitionDecl, ""}
  syntax State ::= Id
  syntax Exp ::= "true" | "false" | String | Int | Id
               | Id "." Id
               > Exp "+" Exp [left, strict]
               | Exp "-" Exp [left, strict]
               | Exp "<" Exp [strict]
               | Exp ">" Exp [strict]
               | Exp "<=" Exp [strict]
               | Exp ">=" Exp [strict]
               | Exp "==" Exp [strict]
               | Exp "!=" Exp [strict]
               | "(" Exp ")" [bracket]
  syntax Exps ::= List{Exp, ""}
  syntax ExpCList ::= List{Exp, ","}
  syntax Action ::= Id
                  | Id "(" ExpCList ")"
                  | Id "=" Exp
                  | "noop"
  syntax Actions ::= List{Action, ""}
  syntax Transition ::= State "->" State
  syntax Event ::= Id
                 | Id "(" ExpCList ")"

endmodule

module MEDIK-CORE
  imports DOMAINS
  imports MEDIK-SYNTAX

  syntax KResult ::= Id | Int | String

  configuration <medik>
                <inputBuffer> .JSONs </inputBuffer>
                <outputBuffer> .JSONs </outputBuffer>
                <machines>
                  <machine multiplicity="*" type="Set">
                    <pgm> .Actions </pgm>
                    <name> .K </name>
                    <states> .Set </states>
                    <store> .Map </store>
                    <currentState> .K </currentState>
                    <initState> .K </initState>
                    <initActions> .Actions </initActions>
                    <edges>
                      <edge multiplicity="*" type="Set">
                        <startState> .K  </startState>
                        <endState> .K </endState>
                        <eventId> .K </eventId>
                        <edgeCondition> .K </edgeCondition>
                        <actions> .Actions </actions>
                      </edge>
                    </edges>
                  </machine>
                </machines>
                </medik>

```
### Initialization

During initialization, we populate the configuration
using information from the parsed flowchart program.

### Macros

```k

  rule transition :
         StartState -> EndState
         event : EVENT
    => transition :
         StartState -> EndState
         event : EVENT
         condition : true
         action : .Actions
    [macro]

  rule transition :
         StartState -> EndState
         event : EVENT
         condition : CONDITION
    => transition :
         StartState -> EndState
         event : EVENT
         condition : CONDITION
         action : .Actions
    [macro]

  rule transition :
         StartState -> EndState
         event : EVENT
         action : ACTIONS
    => transition :
         StartState -> EndState
         event : EVENT
         condition : true
         action : ACTIONS
    [macro]


  rule transition :
         StartState -> EndState
         event : EVENT
         action : X = EXP ACTIONS
    => transition :
         StartState -> EndState
         event : EVENT
         condition : true
         action : ACTIONS
    [macro]

```
### Action Helpers

We define some helpers related to Action lists.
Mainly we define `processActions`, which takes
a list of actions and does the following:

 * If said list contains an external action, leaves the list unmodified
 * Else, adds the "noop" external action to the end of the list

This is done to ensure that a "noop" response is said to the
frontend even when the user doesn't specify an external action
in the program's transition


```k
  syntax Actions ::= Actions "+Actions" Actions [function]

  rule (A1 A1s) +Actions A2s => A1 (A1s +Actions A2s)
  rule .Actions +Actions A2s => A2s

  //
  syntax Actions ::= "processActions" "(" Actions "|" Actions ")" [function]
                   | "processActions" "(" Actions ")"             [function]

  rule processActions( ACTIONS ) => processActions( ACTIONS | .Actions )

  rule processActions( X = E  ACTIONS                   | PREV)
    => processActions( ACTIONS                          | PREV +Actions X = E .Actions )

  rule processActions( (_:Id Actions) #as CURR::Actions | PREV )
    => PREV +Actions CURR

  rule processActions( .Actions | PREV )
    => PREV +Actions ( noop .Actions )


  syntax JSON ::= asJSON(Action) [function]

  rule asJSON(ID:Id) => Id2String(ID)
  rule asJSON(noop) => "noop"

```
### Initialization

```k

  rule <inputBuffer> { "event" : "init" , "data" : [ MACHINE_NAME:String , .JSONs ] }, IN => IN </inputBuffer>
       <machine>
        <pgm> .Actions => processActions(ACTIONS) </pgm>
        <name> MACHINE_ID </name>
        <currentState> _ => INIT_STATE </currentState>
        <initState> INIT_STATE </initState>
        <initActions> ACTIONS </initActions>
        ...
       </machine>
  requires Id2String(MACHINE_ID) ==String MACHINE_NAME

```
### Main Transition Rule

For an event with corresponding data,
if there a machine in the start state,
then perform transition followed
by specified actions.

```k
  rule <inputBuffer> { "event" : EVENT , "data"  :  DATA } , IN => IN </inputBuffer>
       <machine>
         <pgm> .Actions => processActions(ACTIONS) </pgm>
         <name> MACHINE_NAME </name>
         <currentState> START_STATE => END_STATE </currentState>
         <edge>
            <startState> START_STATE </startState>
            <endState> END_STATE </endState>
            <eventId> EVENT </eventId>
            <actions> ACTIONS </actions>
            ...
         </edge>
         ...
       </machine>
```
### Semantics For Actions

```k

  // TODO: Combination of the following rules without `owise` leads to a stuck configuration.

  rule <machine>
         <pgm> ACTION:Action ACTIONS:Actions => ACTIONS </pgm>
         ...
       </machine>
       <outputBuffer> OUTPUT => OUTPUT +JSONs asJSON(ACTION) </outputBuffer> [owise]

  rule <machine>
         <pgm> ID = EXP:Exp ACTIONS:Actions => ACTIONS </pgm>
         <store> ID |-> ( _ => EXP ) ... </store>
         ...
       </machine>

endmodule
```

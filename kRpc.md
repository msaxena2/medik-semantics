JSON RPC Server
===============

Here we describe insfrastructure
required to enable K to support
JSON rpc.

```.k
requires "jsonRpc.md"

module K-RPC
  imports DOMAINS
  imports JSON-RPC
  imports K-IO


  configuration
    <k-rpc>
        <k> $PGM </k>
        <json-rpc/>
    </k-rpc>

    syntax IOJSON ::= JSON | IOError

    syntax Commands ::= JSON

    syntax JSON ::= #getJSON ( JSONKey , JSON ) [function]
 // ------------------------------------------------------
    rule #getJSON( KEY, { KEY : J, _ } )     => J
    rule #getJSON( _, { .JSONs } )           => undef
    rule #getJSON( KEY, { KEY2 : _, REST } ) => #getJSON( KEY, { REST } )
      requires KEY =/=K KEY2

    syntax Int ::= #getInt ( JSONKey , JSON ) [function]
 // ----------------------------------------------------
    rule #getInt( KEY, J ) => {#getJSON( KEY, J )}:>Int

    syntax String ::= #getString ( JSONKey , JSON ) [function]
 // ----------------------------------------------------------
    rule #getString( KEY, J ) => {#getJSON( KEY, J )}:>String

    syntax Bool ::= isJSONUndef ( JSON ) [function]
 // -----------------------------------------------
    rule isJSONUndef(J) => J ==K undef

    syntax IOJSON ::= JSON | IOError
 // --------------------------------

    syntax KItem ::= getRequest()
 // -----------------------------
    rule <k> getRequest() => #loadRPCCall(#getRequest(FD)) ... </k>
         <rpc-input> FD </rpc-input>
         <batch> _ => undef </batch>


    syntax KItem ::= "#loadFromBatch"
 // ---------------------------------
    rule <k> #loadRPCCall([ _, _ ] #as J) => #loadFromBatch ... </k>
         <batch> _ => J </batch>
         <response> _ => .List </response>

    rule <k> #loadFromBatch ~> _ => #loadRPCCall(J) </k>
         <batch> [ J , JS => JS ] </batch>

    rule <k> #loadFromBatch ~> _ => #putResponse(List2JSON(RESPONSE), FD) ~> getRequest() </k>
         <rpc-output> FD </rpc-output>
         <batch> [ .JSONs ] </batch>
         <response> RESPONSE </response>
      requires size(RESPONSE) >Int 0

    rule <k> #loadFromBatch ~> _ => getRequest() </k>
         <batch> [ .JSONs ] </batch>
         <response> .List </response>

    syntax IOJSON ::= #getRequest(Int) [function, hook(JSON.read)]
 // --------------------------------------------------------------

    syntax K ::= #putResponse(JSON, Int) [function, hook(JSON.write)]
 // -----------------------------------------------------------------

    syntax IOJSON ::= #putResponseError ( JSON ) [klabel(JSON-RPC_putResponseError), symbol]
 // ----------------------------------------------------------------------------------------

    syntax KItem ::= #loadRPCCall(IOJSON)
 // -------------------------------------
    rule <k> #loadRPCCall({ _ } #as J) => #checkRPCCall ~> #runRPCCall ... </k>
         <jsonrpc> _ => #getJSON("jsonrpc", J) </jsonrpc>
         <callid>  _ => #getJSON("id"     , J) </callid>
         <method>  _ => #getJSON("method" , J) </method>
         <params>  _ => #getJSON("params" , J) </params>

    syntax Commands ::= accept() [symbol]

    rule accept() => getRequest()

    rule <k> #loadRPCCall(#EOF) => accept() ... </k>

    rule <k> #loadRPCCall(_:String #Or null #Or _:Int #Or [ .JSONs ]) => #rpcResponseError(-32600,  "Invalid Request") ... </k>
         <callid> _ => null </callid>

    rule <k> #loadRPCCall(undef) => #rpcResponseError(-32700,  "Parse error") ... </k>
         <callid> _ => null </callid>

    syntax JSON ::= List2JSON(List)        [function]
                  | List2JSON(List, JSONs) [function, klabel(List2JSONAux)]
 // -----------------------------------------------------------------------
    rule List2JSON(L) => List2JSON(L, .JSONs)

    rule List2JSON(L ListItem(J), JS) => List2JSON(L, (J, JS))
    rule List2JSON(.List        , JS) => [ JS ]

    syntax KItem ::= #sendResponse ( JSONs )
 // ----------------------------------------
    rule <k> #sendResponse(J) ~> _ => #putResponse({ "jsonrpc": "2.0", "id": CALLID, J }, FD) ~> getRequest() </k>
         <callid>     CALLID </callid>
         <rpc-output> FD     </rpc-output>
         <batch> undef </batch>
      requires CALLID =/=K undef

    rule <k> #sendResponse(J) ~> _ => #putResponse({ "jsonrpc": "2.0", J }, FD) ~> getRequest() </k>
         <callid>            undef </callid>
         <rpc-output>        FD    </rpc-output>
         <batch> undef </batch>

    rule <k> #sendResponse(J) ~> _ => #loadFromBatch </k>
         <callid>           CALLID                                                   </callid>
         <batch>            [ _ ]                                                    </batch>
         <response> ... .List => ListItem({ "jsonrpc": "2.0", "id": CALLID, J }) </response>
      requires CALLID =/=K undef

    rule <k> #sendResponse(_) ~> _ => getRequest() </k>
         <callid>            undef </callid>
         <batch>             undef </batch>

    rule <k> #sendResponse(_) ~> _ => #loadFromBatch </k>
         <callid>            undef </callid>
         <batch>             [ _ ] </batch>

    syntax KItem ::= #rpcResponseSuccess          ( JSON                )
                   | #rpcResponseSuccessException ( JSON , JSON         )
                   | #rpcResponseError            ( JSON                )
                   | #rpcResponseError            ( Int , String        )
                   | #rpcResponseError            ( Int , String , JSON )
                   | "#rpcResponseUnimplemented"
 // --------------------------------------------
    rule <k> #rpcResponseSuccess(J)                 => #sendResponse( "result" : J )                                                ... </k> requires isProperJson(J)
    rule <k> #rpcResponseSuccessException(RES, ERR) => #sendResponse( ( "result" : RES, "error": ERR ) )                            ... </k> requires isProperJson(RES) andBool isProperJson(ERR)
    rule <k> #rpcResponseError(ERR)                 => #sendResponse( "error" : ERR )                                               ... </k>
    rule <k> #rpcResponseError(CODE, MSG)           => #sendResponse( "error" : { "code": CODE , "message": MSG } )                 ... </k>
    rule <k> #rpcResponseError(CODE, MSG, DATA)     => #sendResponse( "error" : { "code": CODE , "message": MSG , "data" : DATA } ) ... </k> requires isProperJson(DATA)
    rule <k> #rpcResponseUnimplemented              => #sendResponse( "unimplemented" : RPCCALL )                                   ... </k> <method> RPCCALL </method>

    syntax KItem ::= "#checkRPCCall"
 // --------------------------------
    rule <k> #checkRPCCall => . ...</k>
         <jsonrpc> "2.0" </jsonrpc>
         <method> _:String </method>
         <params> undef #Or [ _ ] #Or { _ } </params>
         <callid> _:String #Or null #Or _:Int #Or undef </callid>

    rule <k> #checkRPCCall => #rpcResponseError(-32600, "Invalid Request") ... </k>
         <callid> undef #Or [ _ ] #Or { _ } => null </callid> [owise]

    rule <k> #checkRPCCall => #rpcResponseError(-32600, "Invalid Request") ... </k>
         <callid> _:Int </callid> [owise]

    rule <k> #checkRPCCall => #rpcResponseError(-32600, "Invalid Request") ... </k>
         <callid> _:String </callid> [owise]

    syntax SExps ::= "#asSExp" "(" JSON ")"

    syntax KItem ::= "#runRPCCall" | "#exit" | "#done"
 // --------------------------------------------------

    rule <k> #exit => #putResponse({ "jsonrpc": "2.0" , "id": CALLID , "result": "RPC client shutting down!" }, FD) ~> #done </k>
         <callid> CALLID </callid>
         <rpc-output> FD </rpc-output>

    rule <k> #done ~> _ => . </k>

    rule <k> #runRPCCall => #exit ... </k>
         <method> "exit" </method>

endmodule

```


% Copyright (c) Prolog Development Center SPb

implement http_Client
    open core, ws_EventManager

facts
%    xmlHttp_P : serverXMLHTTP60 := serverXMLHTTP60::new()[immediate].
    server_Url_P:string:=erroneous.
    methodRequest_P:string:="". % "wsmRequest".
    methodDo_P:string:="". %wsmDo
    methodRequestChain_P:string:="". % "wsmRequestChain".
    methodNext_P:string:="". % "wsmNext"
    messageID_ParameterName_P:string:="". %"eventID".
    parameters_ParameterName_P:string:="". %"evParams".
    eventManager_V:ws_eventManager:=erroneous.

    transactionID_P:string:="". %"transID".
    sleepInterval_P:integer:=0.
    maxNoOfCyclingRequests_P:integer:=0.

clauses
    new(EventManager):-
        /*No output stream initialized here, no write of Debug operations available here*/
        eventManager_V:=EventManager.

facts
    transactionID:integer:=0.

clauses
    notifyViaHttp(MethodID,EventID,EventParameters):-
        transactionID:=transactionID+1,
        notifyViaHttp(transactionID,MethodID,EventID,EventParameters).

predicates
    notifyViaHttp:(integer TransactionID,notifyMethod_D MethodID,integer EventID,namedValue* EventParameters).
clauses
    notifyViaHttp(TransactionID,methodDo,EventID,EventParameters):-
        !,
        notifyViaHttp2(TransactionID,methodDo_P,EventID,EventParameters).
    notifyViaHttp(TransactionID,methodRequestChain,EventID,EventParameters):-
        !,
        notifyViaHttp2(TransactionID,methodRequestChain_P,EventID,EventParameters),
        requestDataChain(0,EventID,TransactionID).
    notifyViaHttp(TransactionID,methodRequest,EventID,EventParameters):-
        !,
        notifyViaHttp2(TransactionID,methodRequest_P,EventID,EventParameters),
        foreach
            retract(httpExchangeData_F(TransactionID,EventID_out,Parameters_out))
        do
            eventManager_V:eventMsg_P:notify(EventID_out, Parameters_out)
        end foreach.

predicates
    requestDataChain:(integer TryCounter, integer EventID, integer TransactionID).
clauses
    requestDataChain(TryCounter,_EventID,_TransactionID):-
        TryCounter>maxNoOfCyclingRequests_P,
        !.
    requestDataChain(TryCounter,EventID,TransactionID):-
        notifyViaHttp2(TransactionID,methodNext_P,EventID,[]),
        retract(httpExchangeData_F(TransactionID,EventID_out,Parameters_out)),
        not(EventID_out=wsBE_EndOfData_C),
        !,
        if EventID_out=wsBE_WillFollow_C then
            programControl::sleep(TryCounter*sleepInterval_P),
            requestDataChain(TryCounter+1,EventID,TransactionID)
        else
            eventManager_V:eventMsg_P:notify(EventID_out, Parameters_out),
            requestDataChain(0,EventID,TransactionID)
        end if.
    requestDataChain(_TryCounter,_EventID,_TransactionID).

predicates
    notifyViaHttp2:(integer TransactionID,string Method,integer EventID,namedValue_List EventParameters).
clauses
    notifyViaHttp2(TransactionID,Method,EventID,EventParameters):-
        JSON = jsonObject::new(),
        JSON:set_integer(messageID_ParameterName_P, EventID),
        JSON:set_integer(transactionID_P, TransactionID),
        JSON:set_string(parameters_ParameterName_P, toString(EventParameters)),
        httpPOST(Method, JSON).

predicates
    httpPost : (string Method, jsonObject JSON).
clauses
    httpPost(Method, JSON) :-
        RequestObject = jsonRpcRequest::newNextId(),
        RequestObject:method := Method,
        RequestObject:params := some(json::o(JSON)),
        RequestString = RequestObject:asString(),
        httpPost2(server_Url_P,Method,RequestString).

predicates
    httpPost2 : (string Url, string Method,string RequestString).
clauses
    httpPost2(URL, Method, RequestString) :-
        XmlHttp_P=serverXMLHTTP60::new(),
        try
            XmlHttp_P:open_predicate("POST", URL, comDomains::boolean(false), comDomains::null, comDomains::null),
            XmlHttp_P:setRequestHeader("Content-Type", "application/json-rpc; charset=UTF-8"),
            XmlHttp_P:send(comDomains::string(RequestString)),
            handleResponse(Method,XmlHttp_P)
        catch TraceId do
            if ComException = exception::tryGetDescriptor(TraceId, exceptionHandling_exception::genericComException)
                and unsigned(0x800C0005) = exception::tryGetExtraInfo(ComException, exceptionHandling_exception::hResult_parameter)
            then
                succeed()
            else
                ExtraInfo = [namedValue("request", string(RequestString)), namedValue("url", string(Url))],
                exception::continueDetailed(TraceId, exception::unknown, ExtraInfo)
            end if
        end try.

facts
    httpExchangeData_F:(integer TransactionID,integer EventID,namedValue* Parameters).

predicates
    handleResponse : (string Method,serverXMLHTTP60 XmlHttp).
clauses
    handleResponse(Method,XmlHttp_P) :-
        StatusCode = XmlHttp_P:status,
        if 200 = StatusCode then
            Resp = XmlHttp_P:responseText,
            if Method=methodRequestChain_P and Resp="" then
                succeed()
            else
                JsonResp = jsonObject::fromString(Resp),
                if Error = JsonResp:tryGet_object("error") then
                    if Code = Error:tryGet_integer("code") then
                        stdio::writef("Code: %\n", Code)
                    end if,
                    if Msg = Error:tryGet_string("message") then
                        stdio::writef("Message: %\n", Msg)
                    end if,
                    if Data = Error:tryGet_string("data") then
                        stdio::writef("Data: %\n", Data)
                    else
                        stdio::writef("Error: %\n", Error:asString())
                    end if
                elseif JsonObj = JsonResp:tryGet_Object("result") then
                    foreach Key=JsonObj:map:getKey_nd() do
                        if not(Key=toString(wsBE_NoData_C)) then
                            if TransactionID=JsonObj:tryGet_integer(transactionID_C) then
                            else
                                TransactionID=0
                            end if,
                            if not(Key=transactionID_C) then
                                NamedValueListAsString=JsonObj:get_String(Key),
                                Parameters=toTerm(namedValue_List,NamedValueListAsString),
                                assertz(httpExchangeData_F(TransactionID,toTerm(Key),Parameters))
                            end if
                        end if
                    end foreach
                else
                    succeed()
        %            stdio::writef("Unexpected response: %\n", Resp)
                end if
            end if
        else % 200 <> StatusCode
            StatusText = XmlHttp_P:statusText,
            stdio::writef("POST FAIL: status code: % - status text: \"%\"\n", StatusCode, StatusText)
        end if,
        XmlHttp_P:release.

end implement http_Client
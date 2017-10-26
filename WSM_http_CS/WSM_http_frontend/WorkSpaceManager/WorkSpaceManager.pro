%

implement workSpaceManager

open core,   json, ws_eventManager

constants
    backEndServiceIP_C="http://localhost:5558".
%    backEndServiceIP_C="http://localhost/SpbHostDemo/WSM_BE".
    backEndServicePath_C="jsonrpc".

facts
    wsForm_P : wsFE_Form := erroneous.
    wsFrontEnd_V:ws_FrontEnd:=erroneous.

clauses
    new():-
        EventManager=ws_EventManager::new(),
        HttpClient=http_Client::new(EventManager),
        HttpClient:messageID_ParameterName_P:="eventID",
        HttpClient:parameters_ParameterName_P:="evParams",
        HttpClient:sleepInterval_P:=5,
        HttpClient:transactionID_P:="transID",
        HttpClient:maxNoOfCyclingRequests_P:=80,
        HttpClient:server_Url_P:=string::format("%s/%s",backEndServiceIP_C,backEndServicePath_C),
        HttpClient:methodRequest_P:=methodRequest_C,
        HttpClient:methodRequestChain_P:=methodRequestChain_C,
        HttpClient:methodDo_P:=methodDo_C,
        HttpClient:methodNext_P:=methodNext_C,
        wsFrontEnd_V:=ws_FrontEnd::new(EventManager,This,HttpClient),
        succeed(). % just as the point for debug trace

clauses
    run():-
        wsForm_P:setParent(gui::getScreenWindow()),
        wsForm_P:show(),
        WsFE_TasksObj=wsFrontEnd_V:entityRegistry_P:getEntityByName_nd(ws_EventManager::wsFE_Tasks_C),!,
        WsFE_Tasks=convert(wsFE_Tasks,WsFE_TasksObj),
        WsFE_Tasks:loadOptionsFE(),
        messageLoop::run().
    run():-
        exception::raise_User("Not Planned Alternative").

end implement workSpaceManager
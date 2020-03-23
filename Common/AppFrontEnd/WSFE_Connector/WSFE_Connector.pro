% Copyright (c) Prolog Development Center SPb

implement wsFE_Connector
    open core, ws_eventManager

facts
    wsFE_P:ws_FrontEnd:=erroneous.
    httpClient_V:http_Client:=erroneous.

clauses
    new(WsFE):-
        wsFE_P:=WsFE,
        if HttpClientObj=WsFE:entityRegistry_P:getEntityByName_nd(wsFE_HttpClient_C),! then
            httpClient_V:=convert(http_Client,HttpClientObj)
        end if.

facts
    predicateDelayedQueue_P:monitorQueue{tuple{predicate{unsigned},unsigned}}:=monitorQueue::new() [immediate].
clauses
    performDelayed():-
        tuple(Predicate,ItemID)=predicateDelayedQueue_P:tryDequeue(),
        !,
        Predicate(ItemID).
    performDelayed().

clauses
    notify(NotyfyMethod,EventID,Parameters):-
        if workSpaceManager::isHttpClient=true then
            httpClient_V:notifyViaHttp(NotyfyMethod,EventID,Parameters),
            performDelayed()
        else
            wsFE_P:eventManager_P:eventTaskCall_P:notify(EventID,Parameters,uncheckedConvert(object,nullHandle))
        end if.

clauses
    wsFE_Tasks()=convert(wsFE_Tasks,GuiTasks):-
        GuiTasks=wsFE_P:entityRegistry_P:getEntityByName_nd(wsFE_Tasks_C),!.
    wsFE_Tasks()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsFE_Tasks_C)).

clauses
    wsFE_Form()=convert(wsFE_Form,WS_Form):-
        WS_Form=wsFE_P:entityRegistry_P:getEntityByName_nd(wsFE_Form_C),!.
    wsFE_Form()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsFE_Form_C)).

clauses
    wsBE_Messages()=convert(wsBE_Messages,WSFE_MSG):-
        WSFE_MSG=wsFE_P:entityRegistry_P:getEntityByName_nd(wsBE_Messages_C),!.
    wsBE_Messages()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsBE_Messages_C)).

clauses
    wsFE_SourceTree()=convert(wsFE_SourceTree,Object):-
        Object=wsFE_P:entityRegistry_P:getEntityByName_nd(wsFE_SourceTree_C),!.
    wsFE_SourceTree()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsFE_SourceTree_C)).

clauses
    wsFE_SourceList()=convert(wSFE_SourceList,Object):-
        Object=wsFE_P:entityRegistry_P:getEntityByName_nd(wsFE_SourceList_C),!.
    wSFE_SourceList()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsFE_SourceList_C)).

clauses
    wsFE_Images() = convert(wsFE_Images,ImageList):-
        ImageList = wsFE_P:entityRegistry_P:getEntityByName_nd(wsFE_Images_C),!.
    wsFE_Images()=_:-
        exception::raise_User(string::format("Component % not Registered!",wsFE_Images_C)).

clauses
    ws_Events()=wsFE_P:eventManager_P.

end implement wsFE_Connector
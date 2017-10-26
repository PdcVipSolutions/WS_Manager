% Copyright (c) PDCSPB

implement wsBE_Connector
    open core

facts
    wsBE_V:ws_BackEnd:=erroneous.
    taskQueues_P : tsMapM_redBlack{integer TaskKey, monitorQueue{tuple{integer,namedValue_list}} TaskQueue} := tsMapM_redBlack::new() [immediate].

clauses
    new(WsBE):-
        wsBE_V:=WsBE.

clauses
    notify(EventID,Parameters,TaskQueueObj):-
        if workSpaceManager::isHttpService=true then
            TaskQueue=convert(monitorQueue{tuple{integer,namedValue_list}},TaskQueueObj),
            TaskQueue:enqueue(tuple(EventID,Parameters))
        else
            wsBE_V:eventManager_P:eventMsg_P:notify(EventID,Parameters)
        end if.

clauses
    wsBE_Tasks()=convert(wsBE_Tasks,Object):-
        Object=wsBE_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsBE_Tasks_C),!.
    wsBE_Tasks()=_:-
        exception::raise_User(string::format("Component % not Registered!",ws_BackEnd::wsBE_Tasks_C)).

clauses
    wsBE_XmlDocument()=convert(xmlDocument,Object):-
        Object=wsBE_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsBE_XmlDocument_C),!.
    wsBE_XmlDocument()=_:-
        exception::raise_User(string::format("Component % not Registered!",ws_BackEnd::wsBE_XmlDocument_C)).

clauses
    wsFE_Messages()=convert(wsFE_Messages,Object):-
        Object=wsBE_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsFE_Messages_C),!.
    wsFE_Messages()=_:-
        exception::raise_User(string::format("Component % not Registered!",ws_BackEnd::wsFE_Messages_C)).

clauses
    wsBE_Performer()=convert(wsBE_Performer,Object):-
        Object=wsBE_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsBE_Performer_C),!.
    wsBE_Performer()=_:-
        exception::raise_User(string::format("Component % not Registered!",ws_BackEnd::wsBE_Performer_C)).

clauses
    wsBE_Options()=convert(wsBE_Options,Object):-
        Object=wsBE_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsBE_Options_C),!.
    wsBE_Options()=_:-
        exception::raise_User(string::format("Component % not Registered!",ws_BackEnd::wsBE_Options_C)).

clauses
    ws_Events()=wsBE_V:eventManager_P.

end implement wsBE_Connector
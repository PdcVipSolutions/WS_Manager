% Copyright (c) PDCSPB

implement workSpaceManager inherits jsonRpcServiceSupport
    open core, json,ws_EventManager,pfc\asynchronous\

facts
    ws_BackEnd_V:ws_BackEnd:=erroneous.
    wsBE_Tasks_V:wsBE_Tasks:=erroneous.

clauses
    new() :-
        EventManager = ws_EventManager::new(),
        ws_BackEnd_V := ws_BackEnd::new(EventManager),
        setProc_name(methodNext_C, beNext),
        setListener_name(methodRequestChain_C, beDoIt),
        setProc_name(methodRequest_C, beRequest),
        WSBE_TasksObj=ws_BackEnd_V:entityRegistry_P:getEntityByName_nd(ws_BackEnd::wsBE_Tasks_C),
        wsBE_Tasks_V:=tryConvert(wsBE_Tasks,WSBE_TasksObj),
        !.
    new() :-
        exception::raise_User("Unexpected alternative").

predicates
    beDoIt : jsonListener_name.
clauses
    beDoIt(_Context, ArgMap):-
        EventID = ArgMap:get_integer("eventID"),
        TransactionID = ArgMap:get_integer(transactionID_C),
        EventParams = toTerm(namedValue_list, ArgMap:get_string("evParams")),
        TaskQueue = monitorQueue{tuple{integer,namedValue_list}}::new(),
        wsBE_Tasks_V:getTaskQueues():tsSet(TransactionID, TaskQueue),
        _Thread=thread::start
            (
                {:-
                ws_BackEnd_V:eventManager_P:eventTaskCall_P:notify(EventID, EventParams,TaskQueue)
                }
            ).

predicates
    beNext : jsonProc_name.
clauses
    beNext(_Context, ArgMap) = o(Result) :-
        TransactionID = ArgMap:get_integer(transactionID_C),
        TaskQueue=wsBE_Tasks_V:getTaskQueues():tsGet(TransactionID),
        Result = jsonObject::new(),
        if tuple(EventIDnext,Parameters) = TaskQueue:tryDequeue() then
            Result:set_String(toString(EventIDnext),toString(Parameters)),
            Result:set_Integer(transactionID_C,TransactionID),
            if EventIDnext=wsBE_EndOfData_C then
                wsBE_Tasks_V:getTaskQueues():tsRemoveKey(TransactionID)
            end if
        else
            Result:set_String(toString(wsBE_WillFollow_C),toString([])),
            Result:set_Integer(transactionID_C,TransactionID)
        end if.

predicates
    beRequest : jsonProc_name.
clauses
    beRequest(_Context, ArgMap) = o(Result) :-
        EventID = ArgMap:get_integer("eventID"),
        EventParams = toTerm(namedValue_list, ArgMap:get_string("evParams")),
        TransactionID = ArgMap:get_integer(transactionID_C),
        TaskQueue = monitorQueue{tuple{integer,namedValue_list}}::new(),
        wsBE_Tasks_V:getTaskQueues():tsSet(TransactionID, TaskQueue),
        _Thread=thread::start(
            {:-
            ws_BackEnd_V:eventManager_P:eventTaskCall_P:notify(EventID, EventParams,TaskQueue)
            }
        ),
        tuple(EventIDnext,ParametersNext) = TaskQueue:dequeue(),
        Result = jsonObject::new(),
        Result:set_String(toString(EventIDnext),toString(ParametersNext)),
        Result:set_Integer(transactionID_C,TransactionID),
        wsBE_Tasks_V:getTaskQueues():tsRemoveKey(TransactionID).

end implement workSpaceManager
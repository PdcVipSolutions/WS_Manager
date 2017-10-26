%

implement workSpaceManager
    open core

facts
    wsFrontEnd_P:ws_FrontEnd:=erroneous.
    wsForm_P : wsFE_Form := erroneous.

clauses
    new():-
        EventManager=ws_EventManager::new(),
        _BackEnd=ws_BackEnd::new(EventManager),
        wsFrontEnd_P:=ws_FrontEnd::new(EventManager,This),
        wsFrontEnd_P:eventManager_P:appEvent_P:addListener(closeApplication),
        succeed().

clauses
    run(ParentWindow):-
        wsForm_P:setParent(ParentWindow),
        wsForm_P:show(),
        WsFE_TasksObj=wsFrontEnd_P:entityRegistry_P:getEntityByName_nd(ws_EventManager::wsFE_Tasks_C),!,
        WsFE_Tasks=convert(wsFE_Tasks,WsFE_TasksObj),
        WsFE_Tasks:loadOptionsFE(),
        succeed().
    run(_ParentWindow):-
        exception::raise_User("Not Planned Alternative").

predicates
    closeApplication:event1{integer EventID}::listener.
clauses
    closeApplication(_EventID):-
        wsFrontEnd_P:eventManager_P:appEvent_P:removeAllListeners(),
        wsFrontEnd_P:=erroneous,
        wsForm_P := erroneous.

end implement workSpaceManager
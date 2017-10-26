%

implement ws_BackEnd
    open core, ws_EventManager

facts
    eventManager_P:ws_EventManager:=erroneous.
    entityRegistry_P:entityRegistry{object}:=erroneous.

clauses
    new(EventManager):-
        entityRegistry_P:=entityRegistry{object}::new(),
        initComponents(EventManager).

predicates
    initComponents:(ws_eventManager EventManager).
clauses
    initComponents(EventManager):-
        eventManager_P:=EventManager,
        entityRegistry_P:register(wsBE_Tasks_C,wsBE_Tasks::new(This)),
        entityRegistry_P:register(wsFE_Messages_C,wsFE_Messages::new(This)),
        entityRegistry_P:register(wsBE_Options_C,wsBE_Options::new(This)),
        entityRegistry_P:register(wsBE_Performer_C,wsBE_Performer::new(This)),
        EventManager:appEvent_P:addListener(closeApplication),
        XML_Doc=xmlDocument::new("work_space"),
        XML_Doc:codePage_P:=utf8,
        XML_Doc:indent_P:=true,
        XML_Doc:xmlStandalone_P:=xmlLite::yes,
        entityRegistry_P:register(wsBE_XmlDocument_C,XML_Doc),
        succeed().

predicates
    closeApplication:event1{integer EventID}::listener.
clauses
    closeApplication(_EventID):-
        entityRegistry_P:unregisterAll(),
        eventManager_P:appEvent_P:removeListener(closeApplication),
        eventManager_P:=erroneous,
        entityRegistry_P:=erroneous.

end implement ws_BackEnd
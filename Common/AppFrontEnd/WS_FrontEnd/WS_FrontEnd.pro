%

implement ws_FrontEnd
    open core, ws_eventManager, pfc\log

facts
    entityRegistry_P:entityRegistry{object}:=erroneous.
    eventManager_P:ws_eventManager:=erroneous.

clauses
    new(EventManager, Service, HttpClient):-
        entityRegistry_P:=entityRegistry{object}::new(),
        entityRegistry_P:register(wsFE_HttpClient_C, HttpClient),
        initComponents(EventManager, Service),
        succeed().
clauses
    new(EventManager, Service):-
        entityRegistry_P:=entityRegistry{object}::new(),
        initComponents(EventManager, Service).

predicates
    initComponents:(ws_eventManager EventManager,workSpaceManager Service).
clauses
    initComponents(EventManager, Service):-
        eventManager_P:=EventManager,
        entityRegistry_P:register(wsFE_Tasks_C, wsFE_Tasks::new(This)),
        entityRegistry_P:register(wsFE_Images_C,wsFE_Images::new(This)),
        WsFE_Form = wsFE_Form::new(This),
        entityRegistry_P:register(wsBE_Messages_C,wsBE_Messages::new(This)),
        entityRegistry_P:register(wsFE_Form_C, WsFE_Form),
        Service:wsForm_P := WsFE_Form.

end implement ws_FrontEnd
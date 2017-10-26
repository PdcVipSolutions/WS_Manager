%

interface wsFE_Connector
    open core, ws_EventManager

predicates
    notify:(notifyMethod_D,integer EventID,namedValue* EventParameters).

properties
    wsFE_P:ws_FrontEnd.
    predicateDelayedQueue_P:monitorQueue{tuple{predicate{unsigned},unsigned}}.
predicates
    performDelayed:().

predicates
    wsFE_Tasks:()->wsFE_Tasks.
    wsFE_Form:()->wsFE_Form.
    wsFE_SourceTree:()->wsFE_SourceTree.
    wSFE_SourceList:()->wSFE_SourceList.
    wsBE_Messages:()->wsBE_Messages.
    ws_Events:()->ws_eventManager.

end interface wsFE_Connector
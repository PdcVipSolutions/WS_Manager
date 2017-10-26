%

interface wsBE_Connector
    open core

/*
domains
    notifyer_D=(integer EventID,namedValue* EventParameters).
properties
    notifyer_P:notifyer_D.
*/
properties
    taskQueues_P : tsMapM_redBlack{integer TaskKey, monitorQueue{tuple{integer,namedValue_list}} TaskQueue}.

predicates
    notify:(integer EventID,namedValue* EventParameters,object TaskQueueObj).

%predicates
%    tryHttpResponceData:()->tuple{integer EventID,namedValue*} determ.
%    tryHttpResponceData:()->tuple{integer EventID,namedValue*}.

predicates
    wsBE_Tasks:()->wsBE_Tasks.
    wsFE_Messages:()->wsFE_Messages.
    wsBE_XmlDocument:()->xmlDocument.
    wsBE_Performer:()->wsBE_Performer.
    wsBE_Options:()->wsBE_Options.
    ws_Events:()->ws_eventManager.

end interface wsBE_Connector
% SPBrSolutions

interface ws_eventManager
    open core

properties
    eventTaskCall_P:event3{integer EventID,namedValue* EventParameters,object TaskQueueObj}.
    eventMsg_P:event2{integer EventID,namedValue* EventParameters}.
    appEvent_P:event1{integer MessageID}.
    currentLng : string.

predicates
    setCurrentLng : (string Lng).
    getLanguageList : () -> tuple{boolean,string,string}* LanguageList.
    getLanguageWSMText : () -> string FileText.
    setLaungugeWSM : (string CurrentLng,string LanguageText).

predicates
    getString : (integer Key) -> string Value.
    getStringByLng : (integer Key,string Lng) -> string Value.

properties
    changeLanguageEvent : event0{} (o).

end interface ws_eventManager
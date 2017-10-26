%

interface ws_BackEnd
    open core

constants
%    wsBE_HttpClient_C="wsBE_HttpClient".
    wsBE_HttpServer_C="wsBE_HttpServer".
    wsBE_Tasks_C="wsBE_Tasks".
    wsBE_XmlDocument_C="wsBE_xmlDoc".
    wsFE_Messages_C="wsFE_Messages".
    wsBE_Source_C="wsBE_Source".
    wsBE_Performer_C="wsBE_Performer".
    wsBE_Options_C="wsBE_Options".

properties
    eventManager_P:ws_EventManager.
    entityRegistry_P:entityRegistry{object}.

end interface ws_BackEnd
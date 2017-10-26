% Copyright

interface http_Client
    open core, ws_EventManager

properties
%    xmlHttp_P:serverXMLHTTP60.
    server_Url_P:string.
    methodRequest_P:string.
    methodRequestChain_P:string.
    methodNext_P:string.
    methodDo_P:string.
    messageID_ParameterName_P:string.
    parameters_ParameterName_P:string.

    sleepInterval_P:integer.
    transactionID_P:string.
    maxNoOfCyclingRequests_P:integer.

predicates
    notifyViaHttp:(notifyMethod_D,integer EventID,namedValue* EventParameters).

end interface http_Client
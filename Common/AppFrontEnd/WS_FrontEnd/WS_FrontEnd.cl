%

class ws_FrontEnd : ws_FrontEnd
    open core

constructors
    new:(ws_eventManager EventManager,workSpaceManager Service).

constructors
    new:(ws_eventManager EventManager,workSpaceManager Service,http_Client HttpClient).

end class ws_FrontEnd
%

interface workSpaceManager
    open core

properties
    wsForm_P : wsFE_Form.
    wsFrontEnd_P:ws_FrontEnd.

predicates
    run:(window).

end interface workSpaceManager
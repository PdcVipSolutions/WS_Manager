/*****************************************************************************

                        Copyright ©

******************************************************************************/
class wsFE_Form : wsFE_Form [noDefaultConstructor]

constructors
    new : (ws_FrontEnd FrontEnd).

    new : (window Parent,ws_FrontEnd).

predicates
    display : (window Parent,ws_FrontEnd FrontEnd) -> wsFE_Form SolutionForm.

end class wsFE_Form
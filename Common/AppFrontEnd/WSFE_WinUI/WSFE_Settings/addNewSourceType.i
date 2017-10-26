%

interface addNewSourceType supports dialog
    open core

predicates
    getReturnValue : () -> optional{tuple{string,string}}.

properties
    editName_ctl : editControl.
    editValue_ctl : editControl.

end interface addNewSourceType
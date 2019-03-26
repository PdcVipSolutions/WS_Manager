%

interface addNewSourceType supports dialog
    open core

predicates
    getReturnValue : () -> optional{tuple{string,string}}.

properties
    editName_P : editControl (o).
    editValue_P : editControl (o).

end interface addNewSourceType
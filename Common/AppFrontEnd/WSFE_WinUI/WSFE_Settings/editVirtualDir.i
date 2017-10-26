%

interface editVirtualDir supports dialog
    open core

predicates
    setVirtualDirectory : (string Name,string Directory,namedValue* TitleList).
    getReturnValue : () -> optional{tuple{string,string}}.

end interface editVirtualDir
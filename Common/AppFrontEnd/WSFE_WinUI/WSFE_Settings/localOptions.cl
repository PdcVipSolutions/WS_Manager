%

class localOptions : localOptions
    open core

predicates
    display : (window Parent,ws_FrontEnd,string LocalOptions,string FileName,xmlDocument ExtOptions,namedValue* VirtualDirList) -> localOptions LocalOptions.

constructors
    new : (window Parent,ws_FrontEnd,string LocalOptions,string FileName,xmlDocument ExtOptions,namedValue* VirtualDirList).

end class localOptions
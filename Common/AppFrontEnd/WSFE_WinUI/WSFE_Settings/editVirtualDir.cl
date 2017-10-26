%

class editVirtualDir : editVirtualDir
    open core
    [noDefaultConstructor]

predicates
    display : (window Parent, string Name, string DirValue,namedValue* TitleList) -> optional{tuple{string,string}}.

end class editVirtualDir

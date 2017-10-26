%

class addNewSourceType : addNewSourceType
    open core
    [noDefaultConstructor]

predicates
    display : (window Parent,topLevelContainerWindow::validateResponder,namedValue* TitleList) -> optional{tuple{string,string}}.

end class addNewSourceType
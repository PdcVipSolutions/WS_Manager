%

class addErrorWarningWords : addErrorWarningWords
    open core

predicates
    display : (window Parent,namedValue* WordsList,namedValue* TitleList) -> optional{namedValue*} Result.

constructors
    new : (window Parent,namedValue* WordsList,namedValue* TitleList).

end class addErrorWarningWords
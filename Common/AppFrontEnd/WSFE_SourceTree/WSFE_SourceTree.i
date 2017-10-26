%

interface wsFE_SourceTree
    open core

properties
    treeControl_P:treeControl{treeNode_std}.

predicates
    addGroup:(command).
    addSubGroup:(command).
    addFolder:(command).
    addSubFolder:(command).

    deleteTreeEntity:(command).
    moveNodeUp:(command).
    moveNodeDown:(command).

predicates
    tryGetSelectedNodePath:()->tuple{namedValue* NodePath,treeNode_std*} determ.

predicates
    setSourceTree:(spbTree::tree{string,namedValue*} SourceTree,namedValue* SelectNodePath).
    setSourceInTask:(namedValue* PerformParams).

predicates
    addNewGroup:(namedValue* NodeParamList).

predicates
    addNewFolder:(namedValue* NodeParamList).

predicates
    removeTreeNode:(namedValue* Parameters).

predicates
    resetModel:().

end interface wsFE_SourceTree
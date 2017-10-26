%

class wsFE_SourceTree : wsFE_SourceTree
    open core

constructors
    new:(ws_FrontEnd,treeControl{treeNode_std}).

predicates
    getNodePath:(treeNode_std Node,tuple{namedValue* NodePath,treeNode_std* NodePAthObj})->tuple{namedValue* NodePath,treeNode_std* NodePAthObj}.

end class wsFE_SourceTree
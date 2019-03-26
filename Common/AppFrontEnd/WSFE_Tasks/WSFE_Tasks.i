%

interface wsFE_Tasks
    open core

properties
    sourceIsRunning_P:boolean.

predicates
    feFormShow:().

predicates
    loadOptionsFE : ().

predicates
    wsFE_InitLog:().

predicates
    newWorkSpace : ().
    on_WScreated:(string  NewWsTree).
    loadVipVirtualDir : ().

predicates
    loadWorkSpace : (). % task to FrontEnd
    setSourceTree:(string SourceTree). % Response from BackEnd
    trySaveNewWorkSpace : () determ.

predicates
    createNewGroup:(namedValue* NodePath). % task for BackEnd
    addNewGroup:(namedValue* NodeParamList). % response from BackEnd

predicates
    tryDefineNewFolder:(window ParentWindow)->string FolderPath determ.
    createNewFolder:(namedValue* NodePath). % task for BackEnd
    addNewFolder:(namedValue* NodeParamList). % response from BackEnd

predicates
    tryRemoveNode:(namedValue* NodePath) determ. % task for BackEnd
    removeTreeNode:(namedValue* NodePath).  %response from BackEnd

predicates
    moveAbove:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveBelow:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveOnTop:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveOnBottom:(namedValue* SourceNodePath,namedValue* TargetNodePath).

predicates
    setNewTitle:(namedValue* NodePath,string NewTitle).

predicates
    explain:(string ExplanationText).

predicates
    showNodeContent:(namedValue* NodePath).
    updateNodeContent:(namedValue* NodePath).

predicates
    tryAddSource:(namedValue* Parameters,boolean SetSelect) determ.

predicates
    invoke : (integer Index, boolean TrueIfAll,string* NodeIDList).

predicates
    checkFiles : ().

predicates
    run : (boolean TrueIfReRun,string* NodeIDList).

predicates
    stopRun:().

predicates
    pauseRun : (boolean Pause).

predicates
    continueRun:().

predicates
    execRun : (string* NodeIDList).

predicates
    resetStatus:(boolean ResetAll).

%predicates
%    extSetup:().

predicates
    addSource:().

predicates
    addSourceByMask:(namedValue* PerformParams).

predicates
    removeSource:().

predicates
    addFromFolder:().

predicates
    openSource:().

predicates
    showSourceInTree:().
    setSourceInTask:(namedValue* PerformParams).

predicates
    getSourceLocalOptions:().

predicates
    restoreExcludedSource:().

predicates
    moveSourceUp:(string ItemIDSelected, string ItemIDAbove).

predicates
    moveSourceDown:(string ItemIDSelected, string ItemIDBelow).

predicates
    moveSourceToTree:(boolean ToTop,string ItemIDSelected, namedValue* NodePath).
    cloneSourceToTree:(boolean ToTop,string ItemIDSelected, namedValue* NodePath).
    updateSourceAfterMove:(namedValue* PerformParams).

predicates
    showResult:(namedValue* PerformResultParams).

predicates
    showRunStatus:(namedValue* PerformParams).

predicates
    getShortFileName : (string FileName,predicate{string}).
    setShortFileName:(namedValue* PerformResultParams).

predicates
    getFullFileName : (string FileName,predicate{string}).
    setFullFileName:(namedValue* PerformResultParams).

predicates
    defineMacroSymbols:(namedValue* PerformParams).

predicates
    writeMessage:(namedValue* PerformParams).

predicates
    setExtOptions:(namedValue* PerformParams).
    setLocalExtOptions:(namedValue* PerformParams).
    saveLocalOptions:(namedValue* PerformParams).
    showLocalOptions : ().
    showLocalOptionsDialog : (namedValue* PerformParams).

predicates
    showSettingsDialog:(namedValue* PerformParams).
    updateWSVariables:(namedValue* PerformParams).

predicates
    about:().
    help:().

predicates
    setFrontEndOptions:(namedValue* PerformParams).

predicates
    setSourceColors:(namedValue* PerformParams).

predicates
    save_OptionsFE:().

predicates
    updateExtOptions : (namedValue* ExtOptionsList).

predicates
    showAll : (boolean IsShowAll).
    setFilter : (checkCommand ExtFilterCmd,string* ExtList).

predicates
    openSourceAtFrontEnd : (namedValue* PerformParams).
    execSourceAtFrontEnd : (namedValue* PerformParams).
    runSourceAtFrontEnd : (namedValue* PerformParams).
    stopRunSourceAtFrontEnd : ().

properties
    wsvUpdateResponder_P : predicate{namedValue*}.

end interface wsFE_Tasks
%

interface wSBE_Tasks
    open core

/*
predicates
    setNotifyer:(wsBE_Connector::notifyer_D).
*/
predicates
    wsBE_InitLog:(object TaskQueueObj).
    wsFE_Started:(object TaskQueueObj).

    create_WorkSpace:(string FullWorkSpaceName,object TaskQueueObj).
    open_WorkSpace:(string FullNameSpaceName,object TaskQueueObj).
    saveWorkSpace:().
    saveWorkSpaceAs:(string FullNameSpaceName).

    new_Group:(namedValue* PathList,object TaskQueueObj).
    new_Folder:(namedValue* PathList,object TaskQueueObj).
    setTitle:(namedValue* PathList,string Title).
    removeNode:(namedValue* ToParentNodePath,string Title,object TaskQueueObj).

predicates
    getTaskQueues:()->tsMapM_redBlack{integer TaskKey, monitorQueue{tuple{integer,namedValue_list}} TaskQueue}.
%    tryGetHttpResponceData:()->tuple{integer EventID,namedValue*} determ.
%    tryGetHttpResponceData:()->tuple{integer EventID,namedValue*}.

predicates
    moveAbove:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveBelow:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveOnTop:(namedValue* SourceNodePath,namedValue* TargetNodePath).
    moveOnBottom:(namedValue* SourceNodePath,namedValue* TargetNodePath).

predicates
    moveSourceUp:(string ItemIDSelected, string ItemIDAbove,namedValue* TreeNodePath,object TaskQueueObj).

predicates
    moveSourceDown:(string ItemIDSelected, string ItemIDBelow,namedValue* TreeNodePath,object TaskQueueObj).

predicates
    moveSourceToTree:(boolean ToTop, string ItemIDSelected, namedValue* TreeNodePath,object TaskQueueObj).
    cloneSourceToTree:(boolean ToTop, string ItemIDSelected, namedValue* TreeNodePath,object TaskQueueObj).

predicates
    getNodeContent:(namedValue* NodePath,object TaskQueueObj).
    updateNodeContent:(namedValue* NodePath,object TaskQueueObj).

predicates
    addSource:(namedValue* FileNameAndNodePath,object TaskQueueObj).

predicates
    deleteSourceList:(namedValue* SourceIDListAndNodePath).

predicates
    getExtListAddNewSource : (object TaskQueueObj).

predicates
    addSourcesFromFolder:(namedValue* FolderNameAndNodePath,object TaskQueueObj).

predicates
    perform:(namedValue* SourceIDList,namedValue* TargetPath,boolean IsContinue,object TaskQueueObj).
    doRunSource : (object TaskQueueObj).

predicates
    checkFile:(namedValue* TargetPath,object TaskQueueObj).

predicates
    pauseRun : (boolean Pause,object TaskQueueObj).

predicates
    stopRun:(object TaskQueueObj).

predicates
    handleStreamFrontEnd:(boolean EndRunSource,string Line,object TaskQueueObj).

predicates
    resetStatus:(namedValue* NodePath,object TaskQueueObj).
    selResetStatus:(namedValue* SourceIDList,namedValue* TargetPath,object TaskQueueObj).

predicates
    openSource:(namedValue* SourceParams,string NodeID,object TaskQueueObj).

predicates
    execute:(namedValue* SourceIDList,namedValue* TargetPath,object TaskQueueObj).

predicates
    showSourceInTree:(string NodeID,object TaskQueueObj).

predicates
    restoreDeletedSource:(string NodeID).

predicates
%    setupVirtualDirList:(object TaskQueueObj).
    getShortFileName:(string FullFileName,object TaskQueueObj).
    getFullFileName:(string ShortFileName,object TaskQueueObj).

predicates
    getExtOptionsList:(string NodeID,object TaskQueueObj).
    updateExtOptionsList : (namedValue* ExtOptionsList).
    updateExtOptions : (namedValue* ExtOptionsList,object TaskQueueObj).
    updateSourceLocalOptions : (namedValue* ExtOptionsList).
    updateSelectSourceType : (namedValue* SelectSourceType).

predicates
    getFrontEndOptions:(object TaskQueueObj).
    setFrontEndOptions : (namedValue* FrontEndOptions).
    updateSourceColors : (namedValue* SourceColorsList,object TaskQueueObj).
    updateUILanguage : (namedValue* FrontEndOptions).

predicates
    insertVirtualDir : (string Name,string NewDirValue).
    updateVirtualDir : (string Name,string NewDirValue).
    deleteVirtualDir : (string Name).
    addMacroSymbolDefinition:(namedValue DefinedMacroSym).
    setVipVirtualDir : (namedValue* VipVirtualDir).
    setWSVariableFile : (string NewWSVFile).
predicates
    getSettings:(object TaskQueueObj).
    updateWSVSettings:(object TaskQueueObj).
    getWSVariablesForLO:(object TaskQueueObj).

predicates
    updateOptionsNotifyFE : (namedValue*,object TaskQueueObj).

end interface wSBE_Tasks
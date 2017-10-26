% Copyright (c) PDCSPB

implement wsFE_Messages
    inherits wsBE_Connector
    open core, ws_EventManager

clauses
    new(Backend):-
        wsBE_Connector::new(Backend),
        ws_Events():eventTaskCall_P:addListener(wsFE_Message).

predicates
    wsFE_Message:event3{integer CommandID, namedValue* Parameters,object TaskQueue}::listener.
clauses
    wsFE_Message(frontEndStarted_C,[],TaskQueue):-
        !,
        wSBE_Tasks():wsBE_InitLog(TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(startedFE_C,[],TaskQueue):-
        !,
        wSBE_Tasks():wsFE_Started(TaskQueue).
    wsFE_Message(newWorkSpace_C,[namedValue("",string(FullNameSpaceName))],TaskQueue):-
        !,
        wSBE_Tasks():create_WorkSpace(FullNameSpaceName,TaskQueue).
    wsFE_Message(openWorkSpace_C,[namedValue("",string(FullNameSpaceName))],TaskQueue):-
        !,
        wSBE_Tasks():open_WorkSpace(FullNameSpaceName,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(saveAsWorkSpace_C,[namedValue("",string(FullNameSpaceName))],TaskQueue):-
        !,
        wSBE_Tasks():saveWorkSpaceAs(FullNameSpaceName),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(newGroup_C, NodePath,TaskQueue):-
        wSBE_Tasks():new_Group(NodePath,TaskQueue),
        !.
    wsFE_Message(newFolder_C, NodePath,TaskQueue):-
        wSBE_Tasks():new_Folder(NodePath,TaskQueue),
        !.
    wsFE_Message(newTitle_C, [namedValue(newTitleStr_C,string(Title))|NodePath],TaskQueue):-
        wSBE_Tasks():setTitle(NodePath,Title),
        !,
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(removeNode_C, [namedValue(title_C,string(Title))|ToParentNodePath],TaskQueue):-
        wSBE_Tasks():removeNode(ToParentNodePath,Title,TaskQueue),
        !.
    wsFE_Message(moveAbove_C, SourcePath_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourcePath_TargetPath,SourcePathDirty,TargetPath),
        SourcePath=list::remove(SourcePathDirty,namedValue("",none)),
        wSBE_Tasks():moveAbove(SourcePath,TargetPath),
        !,
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(moveBelow_C,  SourcePath_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourcePath_TargetPath,SourcePathDirty,TargetPath),
        SourcePath=list::remove(SourcePathDirty,namedValue("",none)),
        wSBE_Tasks():moveBelow(SourcePath,TargetPath),
        !,
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(moveOnTop_C, SourcePath_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourcePath_TargetPath,SourcePathDirty,TargetPath),
        SourcePath=list::remove(SourcePathDirty,namedValue("",none)),
        wSBE_Tasks():moveOnTop(SourcePath,TargetPath),
        !,
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(moveOnBottom_C, SourcePath_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourcePath_TargetPath,SourcePathDirty,TargetPath),
        SourcePath=list::remove(SourcePathDirty,namedValue("",none)),
        wSBE_Tasks():moveOnBottom(SourcePath,TargetPath),
        !,
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(getNodeContent_C, NodePath,TaskQueue):-
        !,
        wSBE_Tasks():getNodeContent(NodePath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(updateNodeContent_C, NodePath,TaskQueue):-
        !,
        wSBE_Tasks():updateNodeContent(NodePath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(getExtListAddNewSource_C, _,TaskQueue):-
        !,
        wSBE_Tasks():getExtListAddNewSource(TaskQueue).
    wsFE_Message(addNewSource_C, FileNameAndNodePath,TaskQueue):-
        !,
        wSBE_Tasks():addSource(FileNameAndNodePath,TaskQueue).
    wsFE_Message(deleteSourceList_C, SourceIDListAndNodePath,TaskQueue):-
        !,
        wSBE_Tasks():deleteSourceList(SourceIDListAndNodePath),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(addSourcesFromFolder_C, SourceIDListAndNodePath,TaskQueue):-
        !,
        wSBE_Tasks():addSourcesFromFolder(SourceIDListAndNodePath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(moveSourceUp_C,[namedValue(itemSelected_C,string(ItemIDSelected)),namedValue(itemAbove_C,string(ItemIDAbove))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():moveSourceUp(ItemIDSelected,ItemIDAbove,SourceNodePath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(moveSourceDown_C, [namedValue(itemSelected_C,string(ItemIDSelected)),namedValue(itemBelow_C,string(ItemIDBelow))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():moveSourceDown(ItemIDSelected,ItemIDBelow,SourceNodePath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(moveSourceTop_C, [namedValue(itemSelected_C,string(ItemIDSelected))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():moveSourceToTree(true, ItemIDSelected, SourceNodePath,TaskQueue).
    wsFE_Message(moveSourceEnd_C, [namedValue(itemSelected_C,string(ItemIDSelected))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():moveSourceToTree(false, ItemIDSelected, SourceNodePath,TaskQueue).
    wsFE_Message(cloneSourceTop_C, [namedValue(itemSelected_C,string(ItemIDSelected))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():cloneSourceToTree(true, ItemIDSelected, SourceNodePath,TaskQueue).
    wsFE_Message(cloneSourceEnd_C, [namedValue(itemSelected_C,string(ItemIDSelected))|SourceNodePath],TaskQueue):-
        !,
        wsBE_Tasks():cloneSourceToTree(false, ItemIDSelected, SourceNodePath,TaskQueue).
    wsFE_Message(runSourceList_C, SourceList_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourceList_TargetPath,SourceListDirty,TargetPath),
        SourceIDList=list::remove(SourceListDirty,namedValue("",none)),
        !,
        wSBE_Tasks():perform(SourceIDList,TargetPath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(pauseRun_C, [namedValue("pause",boolean(Pause))],TaskQueue):-
        !,
        wSBE_Tasks():pauseRun(Pause,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(stopRun_C, [],TaskQueue):-
        !,
        wSBE_Tasks():stopRun(TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(resetStatus_C, NodePath,TaskQueue):-
        !,
        wSBE_Tasks():resetStatus(NodePath,TaskQueue).
    wsFE_Message(selResetStatus_C, SourceList_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourceList_TargetPath,SourceListDirty,TargetPath),
        SourceIDList=list::remove(SourceListDirty,namedValue("",none)),
        !,
        wSBE_Tasks():selResetStatus(SourceIDList, TargetPath,TaskQueue).
    wsFE_Message(openSource_C, [namedValue(nodeId_C,string(NodeIdToOpen))|NodePath],TaskQueue):-
        !,
        wSBE_Tasks():openSource(NodePath,NodeIdToOpen,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(showSourceInTree_C, [namedValue(nodeId_C,string(NodeIdToOpen))],TaskQueue):-
        !,
        wSBE_Tasks():showSourceInTree(NodeIdToOpen,TaskQueue).
    wsFE_Message(restoreDeletedSource_C, [namedValue(nodeId_C,string(NodeId))],TaskQueue):-
        !,
        wSBE_Tasks():restoreDeletedSource(NodeId),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(execSourceList_C, SourceList_TargetPath,TaskQueue):-
        list::splitAfter({(Elem):-Elem=namedValue("",none)},SourceList_TargetPath,SourceListDirty,TargetPath),
        SourceIDList=list::remove(SourceListDirty,namedValue("",none)),
        !,
        wSBE_Tasks():execute(SourceIDList, TargetPath,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(getSourceLocalOptions_C, [namedValue(nodeId_C,string(NodeId))],TaskQueue):-
        !,
        wSBE_Tasks():getExtOptionsList(NodeId,TaskQueue).
    wsFE_Message(saveLocalExtOptions_C, LocalExtOptions,TaskQueue):-
        !,
        wSBE_Tasks():updateSourceLocalOptions(LocalExtOptions),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(getFrontEndOptions_C, [],TaskQueue):-
        !,
        wSBE_Tasks():getFrontEndOptions(TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(setFrontEndOptions_C, FEOptionsList,TaskQueue):-
        !,
        wSBE_Tasks():setFrontEndOptions(FEOptionsList),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(setupSettings_C, [],TaskQueue):-
        !,
        wSBE_Tasks():getSettings(TaskQueue).
    wsFE_Message(getShortName_C, [namedValue(_,string(FullFileName))],TaskQueue):-
        !,
        wSBE_Tasks():getShortFileName(FullFileName,TaskQueue).
    wsFE_Message(getFullName_C, [namedValue(_,string(FullFileName))],TaskQueue):-
        !,
        wSBE_Tasks():getFullFileName(FullFileName,TaskQueue).
    wsFE_Message(setupVirtualDir_C, [],TaskQueue):-
        !,
        wSBE_Tasks():setupVirtualDirList(TaskQueue).
    wsFE_Message(insertVirtualDir_C, [namedValue(NewName,string(NewDirValue))],TaskQueue):-
        !,
        wSBE_Tasks():insertVirtualDir(NewName, NewDirValue),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(updateVirtualDir_C, [namedValue(Name,string(NewDirValue))],TaskQueue):-
        !,
        wSBE_Tasks():updateVirtualDir(Name, NewDirValue),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(deleteVirtualDir_C, [namedValue(Name,_)],TaskQueue):-
        !,
        wSBE_Tasks():deleteVirtualDir(Name),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(defineMacroSymbol_C, [MacroSymDefinition],TaskQueue):-
        !,
        wSBE_Tasks():addMacroSymbolDefinition(MacroSymDefinition),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(updateExtOptionsList_C, ExtOptionsList,TaskQueue):-
        !,
        wSBE_Tasks():updateExtOptionsList(ExtOptionsList),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(updateSourceColors_C, SourceColorList,TaskQueue):-
        !,
        wSBE_Tasks():updateSourceColors(SourceColorList,TaskQueue).
    wsFE_Message(updateSelectSourceType_C, SelectSourceType,TaskQueue):-
        !,
        wSBE_Tasks():updateSelectSourceType(SelectSourceType),
        notify(wsBE_NoData_C, [],TaskQueue).
    wsFE_Message(updateExtOptions_C, ExtOptionsValues,TaskQueue):-
        !,
        wSBE_Tasks():updateExtOptions(ExtOptionsValues,TaskQueue).
    wsFE_Message(updateUILanguage_C, NewUILanguage,TaskQueue):-
        !,
        wSBE_Tasks():updateUILanguage(NewUILanguage),
        notify(wsBE_NoData_C, [],TaskQueue).

    wsFE_Message(handleSourceRun_C, [namedValue(_,string(Line))],TaskQueue):-
        !,
        wSBE_Tasks():handleStreamFrontEnd(false, Line,TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).

    wsFE_Message(endSourceRun_C, _,TaskQueue):-
        !,
        wSBE_Tasks():handleStreamFrontEnd(true, "",TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).

    wsFE_Message(EndSourceRun, _,TaskQueue):-
        EndSourceRun>200,
        !,
        wSBE_Tasks():handleStreamFrontEnd(true, "",TaskQueue),
        notify(wsBE_EndOfData_C, [],TaskQueue).
    wsFE_Message(_CmdCode,_ParList,_TaskQueue).
/*
    wsFE_Message(CmdCode,ParList,_TaskQueue):-
        ws_Events():eventMsg_P:notify(commandError_C,[namedValue("CmdCode",integer(CmdCode))|ParList]).
*/
end implement wsFE_Messages
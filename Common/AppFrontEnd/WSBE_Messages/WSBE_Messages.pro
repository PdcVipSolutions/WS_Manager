% SPBrSolutions
implement wsBE_Messages
    inherits wsFE_Connector
    open core, ws_EventManager

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        ws_Events():eventMsg_P:addListener(fromBE_Listener),
        notify(methodRequest,ws_EventManager::startedFE_C, []),
        succeed().

predicates
    fromBE_Listener:event2{integer CommandID, namedValue* Parameters}::listener.
clauses
    fromBE_Listener(weBE_GetVipVirtualDir_C,Msg):-
        stdio::writef("be: id:[%] msg [%]\n",weBE_GetVipVirtualDir_C,Msg),
        fail.
    fromBE_Listener(weBE_GetVipVirtualDir_C, _):-
        !,
        wsFE_Tasks():loadVipVirtualDir().
    fromBE_Listener(wsBE_StartedFE, [namedValue(CurrentLng,string(LanguageText))]):-
        !,
        ws_Events():setLaungugeWSM(CurrentLng, LanguageText).
    fromBE_Listener(wsBE_Created_C, [namedValue(_ID,string(SourceTree))]):-
        !,
        wsFE_Tasks():on_WSCreated(SourceTree).
    fromBE_Listener(wsBE_WSTermTree_C, [namedValue(_ID,string(SourceTree))]):-
        !,
        wsFE_Tasks():setSourceTree(SourceTree).
    fromBE_Listener(wsBE_SetApplicationTitle_C, [namedValue("wsFileName",string(WS_FileName)),namedValue("readOnly",boolean(ReadOnly))]):-
        !,
        wsFE_Form():setTitle(WS_FileName, ReadOnly).
    fromBE_Listener(wsBE_NewGroupCreated_C, NodeParamList):-
        !,
        wsFE_Tasks():addNewGroup(NodeParamList).
    fromBE_Listener(wsBE_NewFolderCreated_C, NodeParamList):-
        !,
        wsFE_Tasks():addNewFolder(NodeParamList).
    fromBE_Listener(wsBE_NodeRemoved_C, Parameters):-
        !,
        wsFE_Tasks():removeTreeNode(Parameters).
    fromBE_Listener(wsBE_AddSource_C, Parameters):-
        wsFE_Tasks():tryAddSource(Parameters, true),
        !.
    fromBE_Listener(wsBE_Source_C, Parameters):-
        wsFE_Tasks():tryAddSource(Parameters, false),
        !.
    fromBE_Listener(wsBE_UpdateSourceStatus_C,PerformParams):-
        wsFE_Tasks():showResult(PerformParams),
        !.
    fromBE_Listener(wsBE_SetShortName_C,PerformParams):-
        wsFE_Tasks():setShortFileName(PerformParams),
        !.
    fromBE_Listener(wsBE_SetFullName_C,PerformParams):-
        wsFE_Tasks():setFullFileName(PerformParams),
        !.
    fromBE_Listener(wsBE_WSUndefinedMacronames_C,PerformParams):-
        wsFE_Tasks():defineMacrosymbols(PerformParams),
        !.
    fromBE_Listener(wsBE_ShowNodeContent_C,PerformParams):-
        wsFE_Tasks():showNodeContent(PerformParams),
        !.
    fromBE_Listener(wsBE_UpdateNodeContent_C,PerformParams):-
        wsFE_Tasks():updateNodeContent(PerformParams),
        !.
    fromBE_Listener(wsBE_UpdateRunStatus_C,PerformParams):-
        wsFE_Tasks():showRunStatus(PerformParams),
        !.
    fromBE_Listener(weBE_WriteMessage_C,PerformParams):-
        wsFE_Tasks():writeMessage(PerformParams),
        !.
    fromBE_Listener(wsBE_ExtOptionsList_C,PerformParams):-
        wsFE_Tasks():setExtOptions(PerformParams),
        !.
    fromBE_Listener(wsBE_LocalExtOptionsList_C,PerformParams):-
        wsFE_Tasks():setLocalExtOptions(PerformParams),
        !.
    fromBE_Listener(wsBE_ExtFileList_C,PerformParams):-
        wsFE_Tasks():addSourceByMask(PerformParams),
        !.
    fromBE_Listener(wsBE_UpdateOptions_C,PerformParams):-
        wsFE_Tasks():updateExtOptions(PerformParams),
        !.
    fromBE_Listener(wsBE_Settings_C,PerformParams):-
        wsFE_Tasks():showSettingsDialog(PerformParams),
        !.
    fromBE_Listener(wsBE_GetWSVariablesForLO_C,PerformParams):-
        wsFE_Tasks():showLocalOptionsDialog(PerformParams),
        !.
    fromBE_Listener(wsBE_UpdateWSV_C,PerformParams):-
        wsFE_Tasks():updateWSVariables(PerformParams),
        !.
    fromBE_Listener(wsBE_ShowSourceInTree_C,PerformParams):-
        wsFE_Tasks():setSourceInTask(PerformParams),
        !.
    fromBE_Listener(wsBE_MoveToTree_C,PerformParams):-
        wsFE_Tasks():updateSourceAfterMove(PerformParams),
        !.
    fromBE_Listener(wsBE_FEOptionsList_C,PerformParams):-
        wsFE_Tasks():setFrontEndOptions(PerformParams),
        !.
    fromBE_Listener(wsBE_UpdateSourceColors_C,PerformParams):-
        wsFE_Tasks():setSourceColors(PerformParams),
        !.
    fromBE_Listener(weBE_OpenSource_C,PerformParams):-
        wsFE_Tasks():openSourceAtFrontEnd(PerformParams),
        !.
    fromBE_Listener(weBE_ExecSource_C,PerformParams):-
        wsFE_Tasks():execSourceAtFrontEnd(PerformParams),
        !.
    fromBE_Listener(weBE_RunSource_C,PerformParams):-
        wsFE_Tasks():runSourceAtFrontEnd(PerformParams),
        !.
    fromBE_Listener(weBE_StopRunSource_C,_):-
        wsFE_Tasks():stopRunSourceAtFrontEnd(),
        !.
    fromBE_Listener(_Any2, _Parameters).

end implement wsBE_Messages
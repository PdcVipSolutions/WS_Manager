%

implement wsFE_Tasks
    inherits wsFE_Connector
    open core, vpiDomains, ws_eventManager, pfc\log\

facts
    sourceIsRunning_P:boolean:=false.

    runDone_V:integer := 0.
    runFailed_V:integer := 0.
    runNoPath_V:integer := 0.

    runMode_V:string := run_C.
    stopRun_V : boolean := false.

    editorResponder : predicate{string} := erroneous.
    showItemID_F : listViewControl::itemId := erroneous.
    showAllFilter : boolean := true.
    filterSourceList : string* := [].
    nameChecked : string* := [].
    ribbonResetStatus : boolean := true.
    wsvUpdateResponder_P : predicate{namedValue*} := erroneous.

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        currentDirectory_V:=directory::getCurrentDirectory(),
        workSpace_LastFolder_V := currentDirectory_V,
        source_LastFolder_V := currentDirectory_V,
        folder_LastFolder_V := currentDirectory_V.

facts
    currentDirectory_V:string:=directory::getCurrentDirectory().

clauses
    save_OptionsFE() :-
        FEOptions =
        [
        namedValue(Name, string(Value)) ||
            Name = list::getMember_nd([workSpace_C, source_C, folder_C, fileName_C, nodeIDList_C,ribbonLayout_C,checkedFilter_C,ribbonState_C]),
        if workSpace_C = Name then
            workSpace_LastFolder_V = Value
        elseif source_C = Name then
            source_LastFolder_V = Value
        elseif folder_C = Name then
            folder_LastFolder_V = Value
        elseif fileName_C = Name, not(isErroneous(workSpace_LastOpenedFile_V)) then
            workSpace_LastOpenedFile_V = Value
        elseif nodeIDList_C = Name then
            Value = toString(wsFE_Form():selectNode_V)
        elseif ribbonLayout_C = Name then
            Value = toString(wsFE_Form():ribbonControl_P:layout)
        elseif ribbonState_C = Name then
            StateList = getRibbonStateList(),
            Value = toString(StateList)
        elseif checkedFilter_C = Name then
            if showAllFilter = true then
                FilterList = ["$All"|nameChecked]
            else
                FilterList = nameChecked
            end if,
            Value = toString(FilterList)
        else
            fail
        end if
        ],
        notify(methodRequest,setFrontEndOptions_C, FEOptions).

predicates
    getRibbonStateList : () -> namedValue*.
clauses
    getRibbonStateList() =
        [
        namedValue("reset", boolean(ribbonResetStatus)),
        namedValue("local", boolean(false))
        ].

clauses
    wsFE_InitLog():-
        notify(methodRequest,frontEndStarted_C,[]).

clauses
    feFormShow().

constants
    vpDirToolsKey = @"Software\Prolog Development Center\Visual Prolog6\settings\toolsDirList".
clauses
    loadVipVirtualDir():-
        try
            DirToolsList = registry::getAllValues(registry::currentUser, vpDirToolsKey)
        catch _ do
            DirToolsList = []
        end try,
        notify(methodRequest, frontEndVipVirtualDir_C, DirToolsList).

clauses
    newWorkSpace():-
        trySaveNewWorkSpace(),
        tryGetWorkSpaceFile(vpiDomains::dlgfn_Save)= WS_FileName,
        !,
        if file::existExactFile(WS_FileName) then
            file::delete(WS_FileName)
        end if,
        notify(methodRequest,newWorkSpace_C,[namedValue(emptyString_C,string(WS_FileName))]).
    newWorkSpace().

clauses
    on_WSCreated(SourceTreeTxt):-
        wsFE_SourceTree():resetModel(),
        SourceTree=toTerm(spbTree::tree{string,namedValue*},SourceTreeTxt),
        wsFE_SourceTree():setSourceTree(SourceTree, []).

predicates
    tryGetWorkSpaceFile:(integer FileNameSaveMode)->string WS_FileName determ.
clauses
    tryGetWorkSpaceFile(FileNameSaveMode)=WS_FileName:-
        currentDirectory_V:=directory::getCurrentDirectory(),
        WS_FileName = vpiCommonDialogs::getFileName
            (
            "*.wsm",
                [
                "WorkSpace Manager","*.wsm","All files","*.*"
                ],
                "WorkSpace File", [FileNameSaveMode], workSpace_LastFolder_V, _SelectedFiles),
        directory::setCurrentDirectory(currentDirectory_V),
        "wsm"=string::toLowerCase(fileName::getExtension(WS_FileName)),
        workSpace_LastFolder_V:=fileName::getPath(WS_FileName),
        workSpace_LastOpenedFile_V := WS_FileName.

clauses
    trySaveNewWorkSpace():-
        if isErroneous(workSpace_LastOpenedFile_V) then
            Answer =
                vpiCommonDialogs::messageBox(ws_Events():getString(questionTitle_C), ws_Events():getString(qstSaveNewWSM_C),
                                                                    mesbox_iconquestion, mesbox_buttonsYesNoCancel, mesbox_defaultfirst,
                                                                    mesbox_suspendapplication),
            !,
            Answer <> 3, % Cancel
            if Answer = 1 then
                tryGetWorkSpaceFile(vpiDomains::dlgfn_Save)= WS_FileName,
                notify(methodRequest,saveAsWorkSpace_C,[namedValue(emptyString_C,string(WS_FileName))])
            end if
        end if.

clauses
    tryDefineNewFolder(ParentWindow)=NewPath:-
        CurrentDirectory = directory::getCurrentDirectory(),
        LastDir = if directory::existExactDirectory(workSpace_LastFolder_V) then workSpace_LastFolder_V else CurrentDirectory end if,
        Answer = vpiDlgDir::getDirectoryName(ParentWindow:getVpiWindow(), LastDir, NewPath),
        directory::setCurrentDirectory(CurrentDirectory),
        Answer = b_true.

clauses
    explain(ExplanationText):-
        vpiCommonDialogs::note(ExplanationText).

clauses
    loadWorkSpace():-
        trySaveNewWorkSpace(),
        tryGetWorkSpaceFile(vpiDomains::dlgfn_filemustexist)=WS_FileName,
        !,
        notify(methodRequestChain,openWorkSpace_C,[namedValue(emptyString_C,string(WS_FileName))]).
    loadWorkSpace().

clauses
    loadOptionsFE():-
        notify(methodRequestChain,getFrontEndOptions_C,[]).

facts - lastFolder_FB
    workSpace_LastFolder_V:string := currentDirectory_V.
    source_LastFolder_V:string := currentDirectory_V.
    folder_LastFolder_V:string := currentDirectory_V.
    workSpace_LastOpenedFile_V:string := erroneous.
    initFileMask : string := "".

clauses
    setFrontEndOptions(FEOptionsList):-
        foreach namedValue(Name, string(Value)) in FEOptionsList do
            if workSpace_C = Name then
                workSpace_LastFolder_V := Value
            elseif source_C = Name then
                source_LastFolder_V := Value
            elseif folder_C =Name then
                folder_LastFolder_V := Value
            elseif fileName_C =Name then
                workSpace_LastOpenedFile_V := Value
            elseif nodeIDList_C =Name then
                wsFE_Form():selectNode_V := toTerm(namedValue_list, Value)
            elseif ribbonLayout_C =Name then
                wsFE_Form():ribbonControl_P:layout := toTerm(ribbonControl::layout, Value),
                wsFE_Form():wsFE_Command_P:predefinedLayout_V := wsFE_Form():ribbonControl_P:layout
            elseif ribbonState_C =Name then
                wsFE_Form():wsFE_Command_P:restoreRibbonState(toTerm(namedValue_list, Value))
            elseif checkedFilter_C =Name then
                wsFE_Form():setCheckedFilter(toTerm(string_list, Value))
            end if
        end foreach,
        loadLastWorkSpace().

clauses
    setSourceColors(SourceColorsList):-
        wSFE_SourceList():setSourceColors(SourceColorsList).

predicates
    loadLastWorkSpace : (). %
clauses
    loadLastWorkSpace():-
        if not(isErroneous(workSpace_LastOpenedFile_V)),
            file::existExactFile(workSpace_LastOpenedFile_V) then
            notify(methodRequestChain,openWorkSpace_C,[namedValue(emptyString_C,string(workSpace_LastOpenedFile_V))])
        else
            workSpace_LastOpenedFile_V := erroneous,
            TempWSFile = filename::createPath(directory::getTemporaryDirectory(), "Unknown.~wsm"),
            notify(methodRequest,newWorkSpace_C,[namedValue(emptyString_C,string(TempWSFile))])
        end if.

clauses
    setSourceTree(SourceTreeTxt):-
        wsFE_SourceTree():resetModel(),
        SourceTree=toTerm(spbTree::tree{string,namedValue*},SourceTreeTxt),
        wsFE_SourceTree():setSourceTree(SourceTree, wsFE_Form():selectNode_V).

clauses
    setSourceInTask(PerformParams):-
        wsFE_SourceTree():setSourceInTask(PerformParams).

clauses
    createNewGroup(NodePath):-
        notify(methodRequest,newGroup_C,NodePath).

    addNewGroup(NodeParamList):-
        wsFE_SourceTree():addNewGroup(NodeParamList).

clauses
    createNewFolder(NodePath):-
        notify(methodRequest,newFolder_C,NodePath).

    addNewFolder(NodeParamList):-
        wsFE_SourceTree():addNewFolder(NodeParamList).

clauses
    tryRemoveNode(NodePath):-
        idc_ok = vpiCommonDialogs::messageBox(ws_Events():getString(removeDlgTitle_C), ws_Events():getString( removeDlgText_C),
            mesbox_iconQuestion,mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
        log::write(log::info,"Yes, Remove\n"),
        notify(methodRequest,removeNode_C,NodePath).

clauses
    removeTreeNode(Parameters):-
        wsFE_SourceTree():removeTreeNode(Parameters).

clauses
    moveAbove(SourceNodePath,TargetNodePath):-
        notify(methodRequest,moveAbove_C,list::append(SourceNodePath,[namedValue(emptyString_C,none)|TargetNodePath])).

clauses
    moveBelow(SourceNodePath,TargetNodePath):-
        notify(methodRequest,moveBelow_C,list::append(SourceNodePath,[namedValue(emptyString_C,none)|TargetNodePath])).

clauses
    moveOnTop(SourceNodePath,TargetNodePath):-
        notify(methodRequest,moveOnTop_C,list::append(SourceNodePath,[namedValue(emptyString_C,none)|TargetNodePath])).

clauses
    moveOnBottom(SourceNodePath,TargetNodePath):-
        notify(methodRequest,moveOnBottom_C,list::append(SourceNodePath,[namedValue(emptyString_C,none)|TargetNodePath])).

clauses
    setNewTitle(NodePath,NewTitle):-
        notify(methodRequest,newTitle_C,[namedValue(newTitleStr_C,string(NewTitle))|NodePath]).

clauses
    showNodeContent(NodePath):-
        wSFE_SourceList():sourceList_P:deleteAllItems(),
        wSFE_SourceList():clearSourceItemGroup(),
        runDone_V := 0,
        runFailed_V := 0,
        runNoPath_V := 0,

        notify(methodRequestChain,getNodeContent_C,NodePath).

clauses
    updateNodeContent(NodePath):-
        notify(methodRequestChain,updateNodeContent_C,[namedValue("filter",string(if showAllFilter = true then "all" else toString(filterSourceList) end if))|NodePath]).

clauses
    tryAddSource(SourceParams, IsSelect):-
        source_C = namedValue::tryGetNamed_string(SourceParams,nodeID_C),
        ObjID = namedValue::tryGetNamed_string(SourceParams,xmlObj_C),
        SPathStr = namedValue::tryGetNamed_string(SourceParams,nodeIDList_C),
        File=namedValue::tryGetNamed_string(SourceParams,fileName_C),
        FullFile=namedValue::tryGetNamed_string(SourceParams,"fullFileName"),
        Ext=string::toLowerCase(fileName::getExtension(File,_Name)),
        if showAllFilter = true or Ext in filterSourceList then
            if string::hasPrefixIgnoreCase(File, "$(", _),
                string::splitStringBySeparators(File, ")", Head, _, File1) then
                VirtDir = string::concat(Head, ")")
            else
                VirtDir = "",
                File1 = File
            end if,
            filename::getPathAndName(File1, Path, Name),
            IconID = getIconID(FullFile),
            !,
            if Errors=namedValue::tryGetNamed_String(SourceParams,errors_C) then succeed() else Errors="0" end if,
            if Warnings=namedValue::tryGetNamed_String(SourceParams,warnings_C) then succeed() else Warnings="0" end if,
            Status = if StatusKey=tryToTerm(integer,namedValue::tryGetNamed_String(SourceParams,status_C)) then ws_Events():getString(StatusKey) else emptyString_C end if,
            if DateTime=namedValue::tryGetNamed_String(SourceParams,datetime_C) then succeed() else DateTime=emptyString_C end if,
            if Errors="0", Warnings="0" then
                ErrorsWarning=emptyString_C
            else
                ErrorsWarning = string::concat(Errors, "/", Warnings)
            end if,
            Item = listViewControl::item(toTerm(ObjID), Name, IconID, [], [string::concat(VirtDir,Path), Status, ErrorsWarning, DateTime]),
            wSFE_SourceList():sourceList_P:insertItem(Item),
            wSFE_SourceList():setSourceItemGroup(toTerm(ObjID), toTerm(SPathStr)),
            if IsSelect = true then
                wsFE_SourceList():sourceList_P:selectAndFocused(toTerm(ObjID))
            elseif not(isErroneous(showItemID_F)) and showItemID_F = toTerm(ObjID) then
                wsFE_SourceList():sourceList_P:selectAndFocused(showItemID_F),
                showItemID_F := erroneous
            end if,
            TotalItems = wSFE_SourceList():sourceList_P:getItemCount(),
            wsFE_Form():progress(4, string::format("%: %d", ws_Events():getString(sourceInGroup_C), TotalItems)),
            if Status = ws_Events():getString(doneStatus_C) then
                runDone_V := runDone_V + 1
            elseif Status = ws_Events():getString(failedStatus_C) then
                runFailed_V := runFailed_V + 1
            elseif Status = ws_Events():getString(noPathStatus_C) then
                runNoPath_V := runNoPath_V + 1
            end if,
            wsFE_Form():progress(3, string::format("%: %d/%d (D: %d, F: %d, NF: %d)",
                                                                    ws_Events():getString(totalReady_C),
                                                                    runDone_V + runFailed_V + runNoPath_V,
                                                                    TotalItems,
                                                                    runDone_V, runFailed_V, runNoPath_V)),
            wsFE_Form(): progress(1, "")
        end if.

predicates
    getIconID : (string SourceFile) ->  integer IconID.
clauses
    getIconID(SourceFile) = wsFE_Images():getSourceImageIdx(SourceFile).

clauses
    addSource():-
        notify(methodRequest,getExtListAddNewSource_C,[]).

clauses
    addSourceByMask([namedValue("mask", string(MaskListStr))]):-
        tuple(NodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        WS_FileName=tryGetSourceFile(toTerm(string_list, MaskListStr)),
        !,
        wSFE_SourceList():sourceList_P:select(wSFE_SourceList():sourceList_P:getSel(), false),
        notify(methodRequest,addNewSource_C,[namedValue(sourceFile_C,string(WS_FileName))|NodePath]).
    addSourceByMask(_).

predicates
    tryGetSourceFile:(string_list MaskList)->string WS_SourceFileName determ.
clauses
    tryGetSourceFile(MaskList)=WS_SourceFileName:-
        currentDirectory_V:=directory::getCurrentDirectory(),
        InitFileMask =
            if
                initFileMask <> "",
                IM = list::tryGetMemberEq({(IFM, MaskStr) :- _ = string::search(MaskStr, IFM, string::caseInsensitive)}, initFileMask, MaskList)
            then IM else initFileMask end if,
        WS_SourceFileName = vpiCommonDialogs::getFileName
            (InitFileMask,
             MaskList,
             "Source File", [dlgfn_filemustexist], source_LastFolder_V, _SelectedFiles),
        directory::setCurrentDirectory(currentDirectory_V),
        initFileMask := string::concat("*.", filename::getExtension(WS_SourceFileName)),
        source_LastFolder_V := fileName::getPath(WS_SourceFileName).

clauses
    removeSource():-
        if
            tuple(NodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
            ListSelected = wsFE_SourceList():sourceList_P:getSel(),
            Count = list::length(ListSelected),
            Count > 0,
            idc_ok = vpiCommonDialogs::messageBox(ws_Events():getString(removeDlgTitle_C), ws_Events():getString(removeSrcDlgText_C),
                mesbox_iconQuestion,mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication)
        then
            ListSelectedStr=[toString(NodeID)||NodeID in ListSelected,
                ItemIndex = wsFE_SourceList():sourceList_P:tryGetItemIndex(NodeID),
                Status = wsFE_SourceList():sourceList_P:getItemColumnText(ItemIndex, 3),
                if Status = ws_Events():getString(doneStatus_C) then
                    runDone_V := runDone_V - 1
                elseif Status = ws_Events():getString(failedStatus_C) then
                    runFailed_V := runFailed_V - 1
                elseif Status = ws_Events():getString(noPathStatus_C) then
                    runNoPath_V := runNoPath_V - 1
                end if,
                if tuple(folder_C, _, _) = wsFE_SourceList():tryGetSourceItemGroup(NodeID) then
                    wsFE_SourceList():showPerformStatus(toString(NodeID),ws_Events():getString(excludedStatus_C),"0","0","")
                else
                    wsFE_SourceList():sourceList_P:deleteItem(NodeID)
                end if
                ],
            TotalItems = wSFE_SourceList():sourceList_P:getItemCount(),
            wsFE_Form():progress(4, string::format("%: %d", ws_Events():getString(sourceInGroup_C), TotalItems)),
            wsFE_Form():progress(3, string::format("%: %d/%d (D: %d, F: %d, NF: %d)",
                                                                            ws_Events():getString(totalReady_C),
                                                                            runDone_V + runFailed_V + runNoPath_V,
                                                                            TotalItems,
                                                                            runDone_V, runFailed_V, runNoPath_V)),
            NewFocusID = wsFE_SourceList():sourceList_P:getFocus(),
            if listViewControl::itemId_null <>  NewFocusID then
                wsFE_SourceList():sourceList_P:selectAndFocused(NewFocusID)
            end if,
            notify(methodRequest,deleteSourceList_C,[namedValue(nodeIDList_C,string(toString(ListSelectedStr)))|NodePath]),
            succeed()
        end if.

clauses
    addFromFolder():-
        ParentWindow=convert(window,wsFE_Form()),
        currentDirectory_V:=directory::getCurrentDirectory(),
        LastDir = if directory::existExactDirectory(folder_LastFolder_V) then folder_LastFolder_V else currentDirectory_V end if,
        b_true=vpiDlgDir::getDirectoryName(ParentWindow:getVpiWindow(), LastDir, Directory),
        directory::setCurrentDirectory(currentDirectory_V),
        tuple(NodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        folder_LastFolder_V:=Directory,
        wSFE_SourceList():sourceList_P:select(wSFE_SourceList():sourceList_P:getSel(), false),
        notify(methodRequestChain,addSourcesFromFolder_C,[namedValue(folderName_C,string(Directory))|NodePath]),
        !.
    addFromFolder().

clauses
    moveSourceUp(ItemIDSelected,ItemIDAbove):-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        wsFE_SourceList():sourceList_P:deleteAllItems(),
        wSFE_SourceList():clearSourceItemGroup(),
        runDone_V := 0,
        runFailed_V := 0,
        runNoPath_V := 0,
        showItemID_F := toTerm(ItemIDSelected),
        notify(methodRequestChain,moveSourceUp_C,
                [
                namedValue(itemSelected_C,string(ItemIDSelected)),
                namedValue(itemAbove_C,string(ItemIDAbove))
                |SourceNodePath
                ]),
        !.
    moveSourceUp(_ItemIDSelected,_ItemIDAbove).

clauses
    moveSourceDown(ItemIDSelected,ItemIDBelow):-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        wsFE_SourceList():sourceList_P:deleteAllItems(),
        wSFE_SourceList():clearSourceItemGroup(),
        runDone_V := 0,
        runFailed_V := 0,
        runNoPath_V := 0,
        showItemID_F := toTerm(ItemIDSelected),
        notify(methodRequestChain,moveSourceDown_C,
                [
                namedValue(itemSelected_C,string(ItemIDSelected)),
                namedValue(itemBelow_C,string(ItemIDBelow))
                |SourceNodePath
                ]),
        !.
    moveSourceDown(_ItemIDSelected,_ItemIDBelow).

clauses
    moveSourceToTree(ToTop, ItemIDSelected, NodePath):-
        if true = ToTop then
            Event = moveSourceTop_C
        else
            Event = moveSourceEnd_C
        end if,
        notify(methodRequest,Event, [namedValue(itemSelected_C,string(ItemIDSelected))|NodePath]).

clauses
    cloneSourceToTree(ToTop, ItemIDSelected, NodePath):-
        if true = ToTop then
            Event = cloneSourceTop_C
        else
            Event = cloneSourceEnd_C
        end if,
        notify(methodRequest,Event, [namedValue(itemSelected_C,string(ItemIDSelected))|NodePath]).

clauses
    updateSourceAfterMove([namedValue(itemSelected_C, string(ItemIDSelected))]):-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        !,
        showItemID_F := toTerm(ItemIDSelected),
        showNodeContent(SourceNodePath).
    updateSourceAfterMove(_PerformParams).

clauses
    openSource():-
        tuple(NodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        [ItemToOpen]=wsFE_SourceList():sourceList_P:getSel(),
        !,
        notify(methodRequestChain,openSource_C,[namedValue(nodeId_C,string(toString(ItemToOpen)))|NodePath]).
    openSource().

clauses
    showSourceInTree():-
        if [ItemToOpen]=wsFE_SourceList():sourceList_P:getSel() then
            showItemID_F := ItemToOpen,
            notify(methodRequest,showSourceInTree_C,[namedValue(nodeId_C,string(toString(ItemToOpen)))])
        end if.

clauses
    getSourceLocalOptions():-
        if [ItemToSet]=wsFE_SourceList():sourceList_P:getSel() then
            notify(methodRequest,getSourceLocalOptions_C,[namedValue(nodeId_C,string(toString(ItemToSet)))])
        end if.

clauses
    restoreExcludedSource():-
        if [NodeID]=wsFE_SourceList():sourceList_P:getSel() then
            notify(methodRequest,restoreDeletedSource_C,[namedValue(nodeId_C,string(toString(NodeID)))]),
            wsFE_SourceList():showPerformStatus(toString(NodeID),"","0","0","")
        end if.

clauses
    execRun(ItemIDListSelected) :-
        if tuple(SourceNodePath,_ObjPAth) = wsFE_SourceTree():tryGetSelectedNodePath() then
            RequestDataList=list::append([namedValue(nodeId_C,string(NodeID))||NodeID=list::getMember_nd(ItemIDListSelected)],[namedValue(emptyString_C,none)|SourceNodePath]),
            notify(methodRequest, execSourceList_C, RequestDataList)
        end if.

clauses
    invoke(Index, TrueIfAll, ItemIDListSelected):-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        !,
        ItemIDListQueue = wsFE_SourceList():setAllInQueue(ItemIDListSelected, Index, TrueIfAll),
        wsFE_Form():progressBar_activate(list::length(ItemIDListQueue)),
        runMode_V := toString(Index),
        RequestDataList=list::append([namedValue(runMode_V,string(NodeID))||NodeID=list::getMember_nd(ItemIDListQueue)],[namedValue(emptyString_C,none)|SourceNodePath]),
        notify(methodRequestChain, invokeSourceList_C,RequestDataList).
    invoke(_Index, _TrueIfAll, _ItemIDListSelected).

clauses
    checkFiles():-
        if tuple(SourceNodePath,_ObjPAth) = wsFE_SourceTree():tryGetSelectedNodePath() then
            notify(methodRequestChain, checkSourceList_C, SourceNodePath)
        end if.

clauses
    run(_TrueIfReRun,_ItemIDListSelected).

clauses
    continueRun():-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        !,
        ItemIDListQueue = wsFE_SourceList():getAllInQueue(),
        wsFE_Form():progressBar_activate(list::length(ItemIDListQueue)),
        RequestDataList=list::append([namedValue(runMode_V,string(NodeID))||NodeID=list::getMember_nd(ItemIDListQueue)],[namedValue(emptyString_C,none)|SourceNodePath]),
        notify(methodRequestChain,continueInvokeSourceList_C,RequestDataList).
    continueRun():-
        exception::raise_User("wsFE_Tasks::continueRun(): Unexpected alternative").

clauses
    stopRun():-
        wsFE_Form():progressBar_remove(),
        notify(methodRequestChain,stopRun_C,[]).

clauses
    pauseRun(Pause):-
        wsFE_Form():progressBar_remove(),
        notify(methodRequestChain,pauseRun_C,[namedValue("pause",boolean(Pause))]).

clauses
    resetStatus(State):-
        ribbonResetStatus := State,
        fail.
    resetStatus(false):-
        tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath(),
        !,
        ItemsIDToReset=wsFE_SourceList():sourceList_P:getSel(),
        RequestDataList=list::append([namedValue("resetMode",string(toString(NodeID)))||NodeID=list::getMember_nd(ItemsIDToReset)],
                                                    [namedValue(emptyString_C,none)|SourceNodePath]),
        notify(methodRequest,selResetStatus_C, RequestDataList).
    resetStatus(_):-
        if tuple(SourceNodePath,_ObjPAth) = wsFE_SourceTree():tryGetSelectedNodePath() then
            notify(methodRequest,resetStatus_C, SourceNodePath)
        end if.

clauses
    showResult(PerformResultParams):-
        SourceID=namedValue::getNamed_string(PerformResultParams,"sourceID"),
        if Errors=namedValue::tryGetNamed_String(PerformResultParams,errors_C) then succeed() else Errors="0" end if,
        if Warnings=namedValue::tryGetNamed_String(PerformResultParams,warnings_C) then succeed() else Warnings="0" end if,
        Status = if StatusKey=tryToTerm(integer,namedValue::tryGetNamed_String(PerformResultParams,status_C)) then ws_Events():getString(StatusKey) else emptyString_C end if,
        if DateTime=namedValue::tryGetNamed_String(PerformResultParams,datetime_C) then succeed() else DateTime=emptyString_C end if,
        try
            wSFE_SourceList():showPerformStatus(SourceID,Status,Errors,Warnings,DateTime)
        catch _ do
            succeed
        end try,
        _ = vpi::processEvents().

clauses
    showRunStatus(PerformParams):-
        StatusPrefix=namedValue::getNamed_string(PerformParams,statusPrefix_C),
        RunTotalCount=namedValue::getNamed_integer(PerformParams,totalCount_C),
        RunDone=namedValue::getNamed_integer(PerformParams,runDone_C),
        RunFailed=namedValue::getNamed_integer(PerformParams,runFailed_C),
        RunNoPath=namedValue::getNamed_integer(PerformParams,runNoPath_C),
        RunStatusMessage=string::format("%s: %d/%d (D: %d, F: %d, NF: %d)",
                                                            StatusPrefix,
                                                            RunDone + RunFailed + RunNoPath,
                                                            RunTotalCount,
                                                            RunDone, RunFailed, RunNoPath),
        wsFE_Form():progress(3, RunStatusMessage),
        wsFE_Form():progressBar_progress(RunDone + RunFailed + RunNoPath),
        if RunFile = namedValue::tryGetNamed_string(PerformParams,runSourceFile_C) then
            if string::hasPrefixIgnoreCase(RunFile, "$(", _),
            string::splitStringBySeparators(RunFile, ")", Head, _, File1) then
                VirtDir = string::concat(Head, ")")
            else
                VirtDir = "",
                File1 = RunFile
            end if,
            filename::getPathAndName(File1, Path, Name),
            RunningMessage = string::format("%: %s (%s)", ws_Events():getString(nowRunning_C), Name, string::concat(VirtDir, Path))
        else
            wsFE_Form():progressBar_remove(),
            RunningMessage = ""
        end if,
        wsFE_Form():progress(1, RunningMessage).

clauses
    defineMacroSymbols(PerformParams):-
        TitleList =
                [
                namedValue("pbOk", string(ws_Events():getString(ttlOk_pb))),
                namedValue("pbCancel", string(ws_Events():getString(ttlCancel_pb))),
                namedValue("pbHelp", string(ws_Events():getString(ttlHelp_pb))),
                namedValue("pbBrowse", string(ws_Events():getString(ttlBrowse_pb))),
                namedValue("ttlCreateVirtDir", string(ws_Events():getString(ttlCreateVirtDir))),
                namedValue("ttlEditVirtDir", string(ws_Events():getString(ttlEditVirtDit))),
                namedValue("txtName", string(ws_Events():getString(txtName))),
                namedValue("txtDir", string(ws_Events():getString(txtDir))),
                namedValue("txtError", string(ws_Events():getString(txtError))),
                namedValue("msgFormat", string(ws_Events():getString(msgFormat)))
                ],
        foreach namedValue(Name,string(DirValue)) in PerformParams do
            if some(NewMacroDef) = editVirtualDir::display(wsFE_Form(), Name, DirValue, TitleList) then
                tuple(NewName,NewValue) = NewMacroDef,
                notify(methodRequest,defineMacroSymbol_C,[namedValue(NewName,string(NewValue))])
            end if
        end foreach.

clauses
    writeMessage([namedValue(_,string(Message))]):-
        !,
        stdio::write(Message).
    writeMessage(_).

clauses
    setExtOptions(PerformParams):-
        wsFE_Form():setExtOptionsList(PerformParams).

    setLocalExtOptions(PerformParams):-
        wsFE_Form():setLocalExtOptionsList(PerformParams).

    saveLocalOptions(PerformParams):-
        notify(methodRequest, saveLocalExtOptions_C, PerformParams).

    showLocalOptions():-
        notify(methodRequest, wsBE_GetWSVariablesForLO_C, []).

clauses
    showSettingsDialog(PerformParams):-
        _ = wsfe_Settings::display(wsFE_Form(), wsFE_P, PerformParams).

    showLocalOptionsDialog(PerformParams):-
        wsFE_Form():showLocalOptionsDialog(PerformParams).

clauses
    updateWSVariables(PerformParams):-
        wsvUpdateResponder_P(PerformParams).

clauses
    updateExtOptions(PerformParams):-
        foreach namedValue(Name, string(Value)) in PerformParams do
            if Name = execOn_C then
                wsFE_Form():wsFE_Command_P:setEnabledExecuteCmd(Value)
            end if
        end foreach.

clauses
    getShortFileName(FileName, Responder):-
        editorResponder := Responder,
        notify(methodRequest,getShortName_C, [namedValue("",string(FileName))]).

clauses
    setShortFileName(PerformParams):-
        if PerformParams = [namedValue(_,string(ShortFileName))], not(isErroneous(editorResponder)) then
            editorResponder(ShortFileName),
            editorResponder := erroneous
        end if.

clauses
    getFullFileName(FileName, Responder):-
        editorResponder := Responder,
        notify(methodRequest,getFullName_C, [namedValue("",string(FileName))]).

clauses
    setFullFileName(PerformParams):-
        if PerformParams = [namedValue(_,string(FullFileName))], not(isErroneous(editorResponder)) then
            editorResponder(FullFileName),
            editorResponder := erroneous
        end if.

clauses
    showAll(IsShowAll):-
        showAllFilter := IsShowAll,
        if tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath() then
            showNodeContent(SourceNodePath)
        end if.

    setFilter(ExtFilterCmd, ExtList):-
        if ExtFilterCmd:checked = true then
            filterSourceList := list::union(filterSourceList, ExtList),
            nameChecked := list::union(nameChecked, [ExtFilterCmd:menuLabel])
        else
            filterSourceList := list::difference(filterSourceList, ExtList),
            nameChecked := list::difference(nameChecked, [ExtFilterCmd:menuLabel])
        end if,
        if tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath() then
            showNodeContent(SourceNodePath)
        end if.

clauses
    openSourceAtFrontEnd(PerformParams):-
        if PerformParams = [namedValue(_,string(FullCommandLine))] then
            Performer=useExe::new(FullCommandLine),
            Performer:run()
        end if.

    execSourceAtFrontEnd(PerformParams):-
        if PerformParams = [namedValue(_,string(FullCommandLine))] then
            Performer=useExe::new(FullCommandLine),
            Performer:run()
        end if.

clauses
    runSourceAtFrontEnd(PerformParams):-
        FullCommandLine = namedValue::getNamed_string(PerformParams,sourcePerformer_C),
        StreamModeStr = namedValue::getNamed_string(PerformParams,streamMode_C),
        OutputStreamMode = toTerm(stream::mode, StreamModeStr),
        stopRun_V := false,
        Performer = useExe::new(FullCommandLine),
        Input = Performer:getFromProcessStream(OutputStreamMode),
        Performer:setShowWindow(false),
        Performer:setNativeCreationFlags(multiThread_native::create_new_process_group),
        Performer:run(),
        handleStream(Input),
        if stopRun_V = true then
            try
                Performer:terminate(777)
            catch _TraceId do
                succeed()
            end try
        end if,
        _ = Performer:wait(),
        Performer:close().

clauses
    stopRunSourceAtFrontEnd():-
        stopRun_V := true.

predicates
    handleStream : (inputStream Input).
clauses
    handleStream(_Input):-
        stopRun_V = true,
        !.
    handleStream(Input):-
        if not(Input:endOfStream()) then
            Line = Input:readLine(),
            notify(methodRequest,handleSourceRun_C, [namedValue("",string(Line))]),
            _ = vpi::processEvents(),
            handleStream(Input)
        else
            notify(methodRequestChain,endSourceRun_C, [])
        end if.

clauses
    help():-
        mainExe::getFilename(Path, _),
        HelpPath = filename::createPath(Path, @"wsmAppData\"),
        HelpFile = filename::setPath(ws_Events():getString(helpFile), HelpPath),
        shell_api::shellOpen(HelpFile).

constants
    aboutShadow_C:binary=#bininclude(@"Doc\VicShadow.jpg").
clauses
    about():-
        Icon=icon::createFromImages([bitmap::createFromBinary(aboutShadow_C)]),
        DlgObj=aboutDialog::new(wsFE_Form()),
        _Font1=vpi::fontCreate(ff_Times,[fs_Bold ],11),

        DlgObj:productFamilyFont_P:=vpi::fontCreate(ff_Times,[fs_Bold ],11),
        DlgObj:productFamily_P:="SPBrSolutions",

        DlgObj:productNameFont_P:=vpi::fontCreate(ff_Times,[fs_Bold ],11),
        DlgObj:productName_P:="WorkSpace Manager\nVersion 2.0",

        DlgObj:copyrightFont_P:=vpi::fontCreate(ff_Times,[fs_Bold ],11),
        DlgObj:copyright_P:="(c) Prolog Development Center SPb.",

        DlgObj:companyNameFont_P:=vpi::fontCreate(ff_Helvetica,[],11),
        DlgObj:companyName_P:="Developed by Victor Yukhtenko\nAssisted by Boris Belov",

        DlgObj:contentFont_P:=vpi::fontCreate(ff_Helvetica,[],10),
        DlgObj:content_P:="WorkSpace manager.\nProvides the creation of the structured list of files of different types, which are used daily.\nAny of these files may be handled in the special way.\n\nVisual Prolog is a registered trademark of Prolog Development Center A/S (Denmark).",
        DlgObj:image_P:=Icon:getImage(64,65),

        DlgObj:show().

end implement wsFE_Tasks
% SPBrSolutions

class ws_eventManager : ws_eventManager
    open core

domains
    notifyMethod_D=
        methodRequest;
        methodDo;
        methodRequestChain.

constants
    wsFE_HttpClient_C="wsFE_HttpClient".
    wsFE_Form_C="wsFE_Form".
    wsBE_Messages_C="wsBE_Messages".
    wsFE_Tasks_C="wsFE_Tasks".
    wsFE_SourceTree_C="wsFE_SourceTree".
    wSFE_SourceList_C="wSFE_SourceList".

constants
    methodRequest_C="wsmRequest".
    methodDo_C="wsmDo".
    methodRequestChain_C="wsmRequestChain".
    methodNext_C="wsmNext".

constants
% WorkSpace Xml Node Titles
    workSpace_C="work_space".
    backEndOptions_C="be_options".
    frontEndOptions_C="fe_options".
    groupNode_C= "group".
    folder_C="folder".
    path_C="path".
    nodeID_C= "nodeID".
    title_C="title".
    xmlObj_C="xmlObj".
    unNamed_C="UnNamed".
    source_C="source".
    fileName_C="filename".
    virtDir_C="VirtDir".
    virtDirName_C="VirtName".
    extName_C="ext".
    name_C="name".
    unknownName_C="Unknown".
    selectSourceType_C="select".

    sourcePerformer_C = "run".
    argPerformer_C = "argRun".
    reargPerformer_C = "reargRun".
    suffixPerformer_C = "suffixRun".
    componentName_C = "compRun".
    streamMode_C = "streamMode".
    codePage_C = "codePage".

    sourceEditor_C = "open".
    argEditor_C = "argOpen".

    sourceExecute_C = "exec".
    cmdExecute_C = "cmdExec".
    argExecute_C = "argExec".
    execOn_C = "execOn".

    value_C = "value".
    addMode_C = "addMode".
    feMode_C = "feMode".

% WorkSpace TreeControl Node Titles
    label_C="label".
    bitmapIdx_C="bitmapIdx".
    selectedBitmapIdx_C="selectedBitmapIdx".
    font_C="font".
    textColor_C="textColor".
    backColor_C="backColor".

% WorkSpace Source Item's Status Titles

    noPathStatus_C = 1.
    stoppedStatus_C = 2.
    builtStatus_C = 3.
    doneStatus_C = 4.
    exceptedStatus_C = 5.
    failedStatus_C = 6.
    noRuleStatus_C = 7.
    excludedStatus_C = 8.

    sourceInGroup_C = 9.
    totalReady_C = 10.
    totalRunning_C = 11.
    questionTitle_C = 12.
    qstSaveNewWSM_C = 13.
    removeDlgTitle_C = 14.
    removeDlgText_C = 15.
    nowRunning_C = 16.
    userRefusedVN_C = 17.
    cmdDeleteNode_C = 18.
    tipDeleteNode_C = 19.
    cmdMoveNodeUp_C = 20.
    tipMoveNodeUp_C = 21.
    cmdMoveNodeDown_C = 22.
    tipMoveNodeDown_C = 23.
    cmdDeleteSrc_C = 24.
    tipDeleteSrc_C = 25.
    cmdMoveSrcUp_C = 26.
    tipMoveSrcUp_C = 27.
    cmdMoveSrcDown_C = 28.
    tipMoveSrcDown_C = 29.
    cmdRun_C = 30.
    tipRun_C = 31.
    cmdReRun_C = 32.
    tipReRun_C = 33.
    cmdResetAll_C = 34.
    tipResetAll_C = 35.
    cmdResetSel_C = 36.
    tipResetSel_C = 37.
    cmdStop_C = 38.
    tipStop_C = 39.
    cmdPause_C = 40.
    tipPause_C = 41.
    cmdExec_C = 42.
    tipExec_C = 43.
    cmdRunAll_C = 44.
    tipRunAll_C = 45.
    cmdReRunAll_C = 46.
    tipReRunAll_C = 47.
    cmdAddSrc_C = 48.
    tipAddSrc_C = 49.
    cmdAddFromFolder_C = 50.
    tipAddFromFolder_C = 51.
    cmdOpenSrc_C = 52.
    tipOpenSrc_C = 53.
    cmdAddGroup_C = 54.
    tipAddGroup_C = 55.
    cmdAddSubGroup_C = 56.
    tipAddSubGroup_C = 57.
    cmdAddFolder_C = 58.
    tipAddFolder_C = 59.
    cmdAddSubFolder_C = 60.
    tipAddSubFolder_C = 61.
    cmdNewWS_C = 62.
    tipNewWS_C = 63.
    cmdOpenWS_C = 64.
    tipOpenWS_C = 65.
    cmdAbout_C = 66.
    tipAbout_C = 67.
    cmdDesign_C = 68.
    tipDesign_C = 69.
    cmdOptions_C = 70.
    tipOptions_C = 71.
    cmdLocalOptions_C = 72.
    tipLocalOptions_C = 73.
    sctWorkspace = 74.
    sctManipulate = 75.
    sctSrcEditing = 76.
    sctSrcActions = 77.
    sctHelpAbout = 78.
    pmnAdd = 79.
    pmnReplace = 80.
    txtExecCmdDisabled = 81.
    txtNewWS = 82.
    txtReadOnly = 83.
    colCmdArgument = 84.
    colLocalValue = 85.
    colAddMode = 86.
    colResultStr = 87.
    rowOpen = 88.
    rowRunMode = 89.
    rowReRunMode = 90.
    rowSuffix = 91.
    rowExecOn = 92.
    rowExecute = 93.
    qstMoveSource = 94.
    colSourceFile = 95.
    colPath = 96.
    colStatus = 97.
    colErrors = 98.
    colDateTime = 99.
    pmnShowInTree = 100.
    pmnExplore = 101.
    pmnSrcMoveAbove = 102.
    pmnSrcMoveBelow = 103.
    pmnSrcMoveTop = 104.
    pmnSrcMoveLast = 105.
    pmnSrcRestore = 106.
    performingStatus_C = 107.
    inQueueStatus_C = 108.
    msgUniqNodeName = 109.
    msgRootNodeDeleted = 110.
    pmnRename = 111.
    pmnInsertToGroup = 112.
    pmnAddAboveNode = 113.
    pmnAddBelowNode = 114.
    pmnPutOnTop = 115.
    pmnPutOnLast = 116.
    ttlOk_pb = 117.
    ttlCancel_pb = 118.
    ttlHelp_pb = 119.
    ttlAddNewSourceType = 120.
    ttlName_nst = 121.
    ttlValue_nst = 122.
    txtTypeExists = 123.
    txtExtAdded = 124.
    txtExtRequired = 125.
    cmdOpen_C = 126.
    ttlAdd_pb = 127.
    ttlDelete_pb = 128.
    ttlCmpName = 129.
    txtResultStr = 130.
    ttlBrowse_pb = 131.
    txtOpenFormat = 132.
    txtEditorFile = 133.
    txtArguments = 134.
    txtFormatCmd = 135.
    txtCodePage = 136.
    txtStreamMode = 137.
    txtRunFile = 138.
    gbArgForRun = 139.
    txtRunMode = 140.
    txtReRunMode = 141.
    txtSuffix = 142.
    txtRunFormat = 143.
    txtExecFormat = 144.
    txtCommandLine = 145.
    cbCmdEnabled = 146.
    ttlNew_pb = 147.
    ttlEdit_pb = 148.
    ttlCreateVirtDir = 149.
    ttlEditVirtDit = 150.
    txtName = 151.
    txtDir = 152.
    txtError = 153.
    cmdClearFilter_C = 154.
    tipClearFilter_C = 155.
    tipDnD_C = 156.
    colWhereMode = 157.
    pmnBackEnd = 158.
    pmnFrontEnd = 159.
    txtPerformFE = 160.
    ttlFontColor_pb = 161.
    ttlSourceFontColors_st = 162.
    ttlBGColor_pb = 163.
    ttlLanguage_st = 164.
    ttlSourceFile_clm = 165.
    ttlColorValue_clm = 166.
    cmdFilter_C = 167.
    tipFilter_C = 168.
    ttlMiscTab = 169.
    ttlSourceTypeTab = 170.
    ttlVirtDirTab = 171.

    groupSource_C = "Group Source".
    folderSource_C = "Folder Source".
    excludedSource_C = "Excluded Source".

constants
    appMessage_C=0. % Start/Stop messages
    taskCall_C=1. % from FrontEnd
    message_C=2. % from BackEnd
    asyncRequest_C=3. %

constants % messageDataIds
    emptyString_C:string="".
    itemSelected_C:string="itemSelected".
    itemAbove_C:string="itemAbove".
    itemBelow_C:string="itemBelow".
    newTitleStr_C:string="newTitle".
    nodeIDList_C:string="nodeIDList".
    folderName_C:string="folderName".
    sourceFile_C:string="sourceFile".
    run_C="run".
    reRun_C="reRun".
    errors_C:string="errors".
    warnings_C:string="warnings".
    status_C:string="status".
    datetime_C:string="datetime".
    ribbonLayout_C="ribbon".
    checkedFilter_C="filter".
    ribbonState_C="rbState".

    statusPrefix_C:string="stbPrefix".
    runSourceFile_C:string="runSource".
    totalCount_C:string="totalCount".
    runDone_C:string="runDone".
    runFailed_C:string="runFailed".
    runNoPath_C:string="runNoPath".
    runNoRule_C:string="runNoRule".
    transactionID_C="transID".


constants % to BE commands
    emptyRequest_C=0.
    setConnection_C=emptyRequest_C+1.
    startedFE_C=setConnection_C+1.
    newWorkSpace_C=startedFE_C+1.
    openWorkSpace_C=newWorkSpace_C+1.
    saveAsWorkSpace_C=openWorkSpace_C+1.
    getFrontEndOptions_C=saveAsWorkSpace_C+1.
    setFrontEndOptions_C=getFrontEndOptions_C+1.
    newGroup_C=setFrontEndOptions_C+1.
    newFolder_C=newGroup_C+1.
    newTitle_C=newFolder_C+1.
    frontEndStarted_C=newTitle_C+1.
    removeNode_C=frontEndStarted_C+1.
    moveAbove_C=removeNode_C+1.
    moveBelow_C=moveAbove_C+1.
    moveOnTop_C=moveBelow_C+1.
    moveOnBottom_C=moveOnTop_C+1.
    getNodeContent_C=moveOnBottom_C+1.
    updateNodeContent_C=getNodeContent_C+1.
    getExtListAddNewSource_C=updateNodeContent_C+1.
    addNewSource_C=getExtListAddNewSource_C+1.
    deleteSourceList_C=addNewSource_C+1.
    runSourceList_C=deleteSourceList_C+1.
    stopRun_C=runSourceList_C+1.
    pauseRun_C=stopRun_C+1.
    handleSourceRun_C=pauseRun_C+1.
    endSourceRun_C=handleSourceRun_C+1.
    resetStatus_C=endSourceRun_C+1.
    selResetStatus_C=resetStatus_C+1.
    openSource_C=selResetStatus_C+1.
    execSourceList_C=openSource_C+1.
    setupSettings_C=execSourceList_C+1.
    getExtOptionsList_C=setupSettings_C+1.
    updateExtOptionsList_C=getExtOptionsList_C+1.
    updateExtOptions_C=updateExtOptionsList_C+1.
    getShortName_C=updateExtOptions_C+1.
    getFullName_C=getShortName_C+1.
    showSourceInTree_C=getFullName_C+1.
    getSourceLocalOptions_C=showSourceInTree_C+1.
    saveLocalExtOptions_C=getSourceLocalOptions_C+1.
    restoreDeletedSource_C=saveLocalExtOptions_C+1.
    setupVirtualDir_C=restoreDeletedSource_C+1.
    insertVirtualDir_C=setupVirtualDir_C+1.
    updateVirtualDir_C=insertVirtualDir_C+1.
    deleteVirtualDir_C=updateVirtualDir_C+1.
    updateSourceColors_C=deleteVirtualDir_C+1.
    updateSelectSourceType_C=updateSourceColors_C+1.
    updateUILanguage_C=updateSelectSourceType_C+1.
    wsBE_WSUndefinedMacronames_C=updateUILanguage_C+1.

constants % Messages from FrontEnd
    addGroup_C=wsBE_WSUndefinedMacronames_C+1.
    addSubGroup_C=addGroup_C+1.
    removeGroup_C=addSubGroup_C+1.
    moveGroupUp_C=removeGroup_C+1.
    moveGroupDown_C=moveGroupUp_C+1.
    insertGroupAfter_C=moveGroupDown_C+1.
    addSource_C=insertGroupAfter_C+1.
    addSourcesFromFolder_C=addSource_C+1.
    defineMacroSymbol_C=addSourcesFromFolder_C+1.

constants % Menu Commands
    removeSource_C=defineMacroSymbol_C+1.
    removeFolder_C=removeSource_C+1.
    moveSourceUp_C=removeFolder_C+1.
    moveSourceDown_C=moveSourceUp_C+1.
    moveSourceTop_C=moveSourceDown_C+1.
    moveSourceEnd_C=moveSourceTop_C+1.
    cloneSourceTop_C=moveSourceEnd_C+1.
    cloneSourceEnd_C=cloneSourceTop_C+1.
    editSource_C=cloneSourceEnd_C+1.

    runSource_C=editSource_C+1.
    reRunSource_C=runSource_C+1.
    runAllSource_C=reRunSource_C+1.
    reRunAllSource_C=runAllSource_C+1.
    stopRunning_C=reRunAllSource_C+1.

constants % errorCodes
    commandError_C=1.

constants % from BE messages
    wsBE_StartedFE=100.
    wsBE_EndOfData_C=wsBE_StartedFE+1. %1
    wsBE_WillFollow_C=wsBE_EndOfData_C+1. %2
    wsBE_NoData_C=wsBE_WillFollow_C+1. %3
    wsBE_Created_C=wsBE_NoData_C+1. %4
    wsBE_WSTermTree_C=wsBE_Created_C+1. %5
    wsBE_SourceFile_C=wsBE_WSTermTree_C+1. %6
    wsBE_SourceFolder_C=wsBE_SourceFile_C+1. %7
    wsBE_SourceTransferComplete_C=wsBE_SourceFolder_C+1. %8
    wsBE_NewGroupCreated_C=wsBE_SourceTransferComplete_C+1. %9
    wsBE_NewFolderCreated_C=wsBE_NewGroupCreated_C+1. %10
    wsBE_NodeRemoved_C=wsBE_NewFolderCreated_C+1. %11
    wsBE_AddSource_C=wsBE_NodeRemoved_C+1. %12
    wsBE_Source_C=wsBE_AddSource_C+1. %13
    wsBE_UpdateSourceStatus_C=wsBE_Source_C+1. %14
    wsBE_UpdateRunStatus_C=wsBE_UpdateSourceStatus_C+1. %15
    wsBE_SetupVirtualDir_C=wsBE_UpdateRunStatus_C+1. %16
    wsBE_ShowNodeContent_C=wsBE_SetupVirtualDir_C+1. %17
    wsBE_UpdateNodeContent_C=wsBE_ShowNodeContent_C+1. %18
    wsBE_SetApplicationTitle_C=wsBE_UpdateNodeContent_C+1. %19
    weBE_WriteMessage_C=wsBE_SetApplicationTitle_C+1. %20
    wsBE_ExtOptionsList_C=weBE_WriteMessage_C+1. %21
    wsBE_LocalExtOptionsList_C=wsBE_ExtOptionsList_C+1. %22
    wsBE_SetShortName_C=wsBE_LocalExtOptionsList_C+1.  %23
    wsBE_SetFullName_C=wsBE_SetShortName_C+1.  %24
    wsBE_ExtFileList_C=wsBE_SetFullName_C+1.  %25
    wsBE_UpdateOptions_C=wsBE_ExtFileList_C+1. %26
    wsBE_Settings_C=wsBE_UpdateOptions_C+1. %27
    wsBE_ShowSourceInTree_C=wsBE_Settings_C+1. %28
    wsBE_MoveToTree_C=wsBE_ShowSourceInTree_C+1. %29
    wsBE_FEOptionsList_C=wsBE_MoveToTree_C+1. %30
    wsBE_UpdateSourceColors_C=wsBE_FEOptionsList_C+1.  %31
    weBE_OpenSource_C=wsBE_UpdateSourceColors_C+1. %32
    weBE_ExecSource_C=weBE_OpenSource_C+1. %33
    weBE_RunSource_C=weBE_ExecSource_C+1. %34
    weBE_StopRunSource_C=weBE_RunSource_C+1. %35

end class ws_eventManager
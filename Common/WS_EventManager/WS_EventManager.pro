% SPBrSolutions

implement ws_eventManager
    open core, xmlNavigate

constants
    defaultLng : string = "eng".

facts
    eventMsg_P:event2{integer EventID,namedValue* EventParameters}:=erroneous.
    eventTaskCall_P:event3{integer EventID,namedValue* Parameters,object TaskQueueObj}:=erroneous.
    appEvent_P:event1{integer MessageID}:=erroneous.

    xmlLng : xmlDocument.
    isAddNewString : boolean := false.
    currentLng : string := defaultLng.
    currentLng_P : varM{string}.

clauses
    changeLanguageEvent() = currentLng_P:modified.

clauses
    new():-
        eventMsg_P:=event2::new(),
        eventTaskCall_P:=event3::new(),
        appEvent_P:=event1::new(),

        xmlLng := xmlDocument::new("lng"),
        xmlLng:codePage_P:=utf8,
        xmlLng:indent_P:=true,
        xmlLng:xmlStandalone_P:=xmlLite::yes,
        currentLng_P := varM::new(defaultLng),
        loadLanguageWSM(),
        !.

clauses
    getLanguageWSMText() =
        toString(file::readBinary(languageWSMFile_C)).

constants
    languageWSMFile_C = @"LanguageWSM.xml".
predicates
    loadLanguageWSM : ().
#if workSpaceManager::isBackEnd_app = true #then
clauses
    loadLanguageWSM():-
        if file::existExactFile(languageWSMFile_C) then
            XmlOptions = inputStream_file::openFile(languageWSMFile_C, stream::binary),
            spbXmlLigntSupport::read(XmlOptions, xmlLng),
            XmlOptions:close()
        end if,
        loadCurrentLng(),
        setDefaultLanguageWSM().

predicates
    loadCurrentLng : ().
clauses
    loadCurrentLng():-
        if _LngNode = xmlLng:getNode_nd([root(), child("lng", {(_)})]) then
        else
            NewLngNode = xmlElement::new("", "lng",xmlLng:root_P),
%            NewLngNode:name_P := "lng",
            xmlLng:root_P:addNode(NewLngNode),
            NewLngNode:addAttribute("eng", "English"),
            NewLngNode:addAttribute("rus", "Русский"),
            NewLngNode:addAttribute("dk", "Dansk"),
            isAddNewString := true
        end if.

predicates
    saveLanguageWSM : ().
clauses
    saveLanguageWSM():-
        OutputStream = outputStream_file::create(languageWSMFile_C, stream::binary),
        xmlLng:saveXml(OutputStream).

predicates
    setDefaultLanguageWSM : ().
clauses
    setDefaultLanguageWSM():-
        foreach lngStringElement(Key, Value, Comment) do
            addStringElement(Key, Value, Comment)
        end foreach,
        if isAddNewString = true then saveLanguageWSM() end if,
        !.

predicates
    addStringElement : (integer Key,string Value,string Comment).
clauses
    addStringElement(Key, Value, Comment):-
        if _ = xmlLng:getNode_nd([root(), child(string::concat("row",toString(Key)), {(_)})]) then
        else
            StrElement = xmlElement::new("", string::concat("row",toString(Key)),xmlLng:root_P),
%            StrElement:name_P := string::concat("row",toString(Key)),
            xmlLng:root_P:addNode(StrElement),
            StrElement:addAttribute("comment", Comment),
            LngElement = xmlElement::new("",defaultLng, StrElement),
%            LngElement:name_P := defaultLng,
            LngElement:addText(Value),
            StrElement:addNode(LngElement),
            isAddNewString := true
        end if.
#else
clauses
    loadLanguageWSM().
#endif

clauses
    setLaungugeWSM(CurrentLng, LanguageText):-
        Binary = toTerm(binary,LanguageText),
        XmlOptions = inputStream_binary::new(Binary),
        spbXmlLigntSupport::read(XmlOptions, xmlLng),
        XmlOptions:close(),
        setCurrentLng(CurrentLng).

predicates
    lngStringElement : (integer Key [out],string Value [out],string Comment [out]) multi.
    lngStringElement_dt : (integer Key,string Value [out],string Comment [out]).
clauses
    lngStringElement_dt(KeyIn, Value, Comment):-
        lngStringElement(Key, Value, Comment),
        Key = KeyIn,
        !.
    lngStringElement_dt(_, "Unexpected", "").

    lngStringElement(noPathStatus_C, "NoPath", "Status").
    lngStringElement(stoppedStatus_C, "Stopped", "Status").
    lngStringElement(builtStatus_C, "Built", "Status").
    lngStringElement(doneStatus_C, "Done", "Status").
    lngStringElement(exceptedStatus_C, "Excepted", "Status").
    lngStringElement(failedStatus_C, "Failed", "Status").
    lngStringElement(noRuleStatus_C, "NoRule", "Status").
    lngStringElement(excludedStatus_C, "Excluded", "Status").
    lngStringElement(sourceInGroup_C, "Files in Group", "Rigth bottom StatusBar").
    lngStringElement(totalReady_C, "Total ready", "Central bottom StatusBar").
    lngStringElement(totalRunning_C, "Total Running", "Central bottom StatusBar").
    lngStringElement(questionTitle_C, "Question", "Dialog Title").
    lngStringElement(qstSaveNewWSM_C, "Save contents to new workspace?", "Question Text").
    lngStringElement(removeDlgTitle_C, "Remove Node confirmation", "Dialog Title").
    lngStringElement(removeDlgText_C, "Are you sure you want to delete node", "Question Text").
    lngStringElement(nowRunning_C, "Running", "Left bottom StatusBar").
    lngStringElement(userRefusedVN_C, "User refused to define VirtualName", "Log message").
    lngStringElement(cmdDeleteNode_C, "Delete Node", "Menu label of Ribbon button").
    lngStringElement(tipDeleteNode_C, "Delete Selected Node", "Tooltip of Ribbon button").
    lngStringElement(cmdMoveNodeUp_C, "Node Up", "Menu label of Ribbon button").
    lngStringElement(tipMoveNodeUp_C, "Move Selected Node Up", "Tooltip of Ribbon button").
    lngStringElement(cmdMoveNodeDown_C, "Node Down", "Menu label of Ribbon button").
    lngStringElement(tipMoveNodeDown_C, "Move Selected Node Down", "Tooltip of Ribbon button").
    lngStringElement(cmdDeleteSrc_C, "Delete File", "Menu label of Ribbon button").
    lngStringElement(tipDeleteSrc_C, "Delete Selected File", "Tooltip of Ribbon button").
    lngStringElement(cmdMoveSrcUp_C, "File Up", "Menu label of Ribbon button").
    lngStringElement(tipMoveSrcUp_C, "Move Selected File Up", "Tooltip of Ribbon button").
    lngStringElement(cmdMoveSrcDown_C, "File Down", "Menu label of Ribbon button").
    lngStringElement(tipMoveSrcDown_C, "Move Selected File Down", "Tooltip of Ribbon button").
    lngStringElement(cmdRun_C, "Run", "Menu label of Ribbon button").
    lngStringElement(tipRun_C, "Run the file", "Tooltip of Ribbon button").
    lngStringElement(cmdReRun_C, "ReRun", "Menu label of Ribbon button").
    lngStringElement(tipReRun_C, "ReRun the File", "Tooltip of Ribbon button").
    lngStringElement(cmdResetAll_C, "Reset All", "Menu label of Ribbon button").
    lngStringElement(tipResetAll_C, "Reset All Files", "Tooltip of Ribbon button").
    lngStringElement(cmdResetSel_C, "Reset\n Selected", "Menu label of Ribbon button").
    lngStringElement(tipResetSel_C, "Reset Selected Files", "Tooltip of Ribbon button").
    lngStringElement(cmdStop_C, "Stop", "Menu label of Ribbon button").
    lngStringElement(tipStop_C, "Stop run All Files", "Tooltip of Ribbon button").
    lngStringElement(cmdPause_C, "Pause", "Menu label of Ribbon button").
    lngStringElement(tipPause_C, "Pause/Run All Files", "Tooltip of Ribbon button").
    lngStringElement(cmdExec_C, "Execute", "Menu label of Ribbon button").
    lngStringElement(tipExec_C, "Execute File", "Tooltip of Ribbon button").
    lngStringElement(cmdRunAll_C, "Run\n All", "Menu label of Ribbon button").
    lngStringElement(tipRunAll_C, "Run All Files", "Tooltip of Ribbon button").
    lngStringElement(cmdReRunAll_C, "ReRun\n All", "Menu label of Ribbon button").
    lngStringElement(tipReRunAll_C, "ReRun All Files", "Tooltip of Ribbon button").
    lngStringElement(cmdAddSrc_C, "Add File", "Menu label of Ribbon button").
    lngStringElement(tipAddSrc_C, "Choose File to be added to this WorkSpace", "Tooltip of Ribbon button").
    lngStringElement(cmdAddFromFolder_C, "Add From Folder", "Menu label of Ribbon button").
    lngStringElement(tipAddFromFolder_C, "Add Files From the Choosen Folder", "Tooltip of Ribbon button").
    lngStringElement(cmdOpenSrc_C, "Open File", "Menu label of Ribbon button").
    lngStringElement(tipOpenSrc_C, "Open File for Edit", "Tooltip of Ribbon button").
    lngStringElement(cmdAddGroup_C, "Add Group", "Menu label of Ribbon button").
    lngStringElement(tipAddGroup_C, "Create New Group", "Tooltip of Ribbon button").
    lngStringElement(cmdAddSubGroup_C, "Add SubGroup", "Menu label of Ribbon button").
    lngStringElement(tipAddSubGroup_C, "Create New SubGroup", "Tooltip of Ribbon button").
    lngStringElement(cmdAddFolder_C, "Add Folder", "Menu label of Ribbon button").
    lngStringElement(tipAddFolder_C, "Create New Folder", "Tooltip of Ribbon button").
    lngStringElement(cmdAddSubFolder_C, "Add SubFolder", "Menu label of Ribbon button").
    lngStringElement(tipAddSubFolder_C, "Create New SubFolder", "Tooltip of Ribbon button").
    lngStringElement(cmdNewWS_C, "New", "Menu label of Ribbon button").
    lngStringElement(tipNewWS_C, "Create New Empty WorkSpace at given place", "Tooltip of Ribbon button").
    lngStringElement(cmdOpenWS_C, "Open", "Menu label of Ribbon button").
    lngStringElement(tipOpenWS_C, "Open WorkSpace", "Tooltip of Ribbon button").
    lngStringElement(cmdAbout_C, "About", "Menu label of Ribbon button").
    lngStringElement(tipAbout_C, "About WorkSpaceManager", "Tooltip of Ribbon button").
    lngStringElement(cmdDesign_C, "Design\n Ribbon", "Menu label of Ribbon button").
    lngStringElement(tipDesign_C, "Design ribbon, sections and commands", "Tooltip of Ribbon button").
    lngStringElement(cmdOptions_C, "Options", "Menu label of Ribbon button").
    lngStringElement(tipOptions_C, "Setup WorkSpace Options", "Tooltip of Ribbon button").
    lngStringElement(cmdLocalOptions_C, "Local\n Options", "Menu label of Ribbon button").
    lngStringElement(tipLocalOptions_C, "Show Panel of File Local Options", "Tooltip of Ribbon button").
    lngStringElement(sctWorkspace, "WorkSpace Editing", "Label of Ribbon section").
    lngStringElement(sctManipulate, "Manipulate Entity", "Label of Ribbon section").
    lngStringElement(sctSrcEditing, "File Editing", "Label of Ribbon section").
    lngStringElement(sctSrcActions, "File Actions", "Label of Ribbon section").
    lngStringElement(sctHelpAbout, "Help & About", "Label of Ribbon section").
    lngStringElement(pmnAdd, "Add", "AddMode item of Local options panel").
    lngStringElement(pmnReplace, "Replace", "AddMode item of Local options panel").
    lngStringElement(pmnFrontEnd, "FrontEnd", "Where item of Local options panel").
    lngStringElement(pmnBackEnd, "BackEnd", "Where item of Local options panel").
    lngStringElement(txtExecCmdDisabled, "Execute command disabled", "Result String for Execute command in Local option panel").
    lngStringElement(txtNewWS, "New workspace", "Application TitleBar").
    lngStringElement(txtReadOnly, "[ReadOnly]", "Application TitleBar").
    lngStringElement(colCmdArgument, "Command Argument", "Column Label of Local options panel").
    lngStringElement(colLocalValue, "Local Value", "Column Label of Local options panel").
    lngStringElement(colAddMode, "Add Mode", "Column Label of Local options panel").
    lngStringElement(colResultStr, "Result string", "Column Label of Local options panel").
    lngStringElement(colWhereMode, "Where", "Column Label of Local options panel").
    lngStringElement(rowOpen, "Open", "Command Argument Name of Local options panel").
    lngStringElement(rowRunMode, "Run mode", "Command Argument Name of Local options panel").
    lngStringElement(rowReRunMode, "ReRun mode", "Command Argument Name of Local options panel").
    lngStringElement(rowSuffix, "Run Suffix", "Command Argument Name of Local options panel").
    lngStringElement(rowExecOn, "Execute enabled", "Command Argument Name of Local options panel").
    lngStringElement(rowExecute, "Execute", "Command Argument Name of Local options panel").
    lngStringElement(qstMoveSource, "Are you sure you want to move file to group", "Question Text").
    lngStringElement(colSourceFile, "File", "Column Label of Source's ListView Control").
    lngStringElement(colPath, "Path", "Column Label of Source's ListView Control").
    lngStringElement(colStatus, "State", "Column Label of Source's ListView Control").
    lngStringElement(colErrors, "Err/Warn", "Column Label of Source's ListView Control").
    lngStringElement(colDateTime, "Date,Time", "Column Label of Source's ListView Control").
    lngStringElement(pmnShowInTree, "Show in Tree", "Popup menu item of Source's ListView Control").
    lngStringElement(pmnExplore, "Explore...", "Popup menu item of Source's ListView Control").
    lngStringElement(pmnSrcMoveAbove, "Move Above Selected File", "D&D Popup menu item of Source's ListView Control").
    lngStringElement(pmnSrcMoveBelow, "Move Below Selected File", "D&D Popup menu item of Source's ListView Control").
    lngStringElement(pmnSrcMoveTop, "Move to Group as Topmost", "D&D Popup menu item of Source's ListView Control").
    lngStringElement(pmnSrcMoveLast, "Move to Group as Last", "D&D Popup menu item of Source's ListView Control").
    lngStringElement(pmnSrcRestore, "Restore Excluded File", "Popup menu item of Source's ListView Control").
    lngStringElement(performingStatus_C, "Performing...", "Status").
    lngStringElement(inQueueStatus_C, "in Queue", "Status").
    lngStringElement(msgUniqNodeName, "Nodes of the same level must have uniq names.\nSee - there is the node with the name \"%\" already", "Dialog Information Text").
    lngStringElement(msgRootNodeDeleted, "Sorry, the  Root Node can not be deleted", "Dialog Information Text").
    lngStringElement(pmnRename, "Rename", "Popup menu item of Group Tree Control").
    lngStringElement(pmnInsertToGroup, "Insert To Group", "Popup menu item of Group Tree Control").
    lngStringElement(pmnAddAboveNode, "Add Above Selected Node", "D&D Popup menu item of Group Tree Control").
    lngStringElement(pmnAddBelowNode, "Add Below Selected Node", "D&D Popup menu item of Group Tree Control").
    lngStringElement(pmnPutOnTop, "Put in Group as Topmost", "D&D Popup menu item of Group Tree Control").
    lngStringElement(pmnPutOnLast, "Put in Group as Last", "D&D Popup menu item of Group Tree Control").
    lngStringElement(ttlOk_pb, "&Ok", "Dialog button text").
    lngStringElement(ttlCancel_pb, "&Cancel", "Dialog button text").
    lngStringElement(ttlHelp_pb, "&Help", "Dialog button text").
    lngStringElement(ttlAddNewSourceType, "Add New File Type", "Static text of Options Dialog").
    lngStringElement(ttlName_nst, "File Type Name:", "Static text of Options Dialog").
    lngStringElement(ttlValue_nst, "Extensions List:", "Static text of Options Dialog").
    lngStringElement(txtTypeExists, "The File type name <%s> exists!", "Dialog Error message").
    lngStringElement(txtExtAdded, "Extension(s) <%s> already added into the file type name <%s>!", "Dialog Error message").
    lngStringElement(txtExtRequired, "File extension is required!", "Dialog Error message").
    lngStringElement(cmdOpen_C, "Open", "Tab label of Options Dialog").
    lngStringElement(ttlAdd_pb, "&Add", "Dialog button text").
    lngStringElement(ttlDelete_pb, "&Delete", "Dialog button text").
    lngStringElement(ttlCmpName, "File Type Name:", "Static text of Options Dialog").
    lngStringElement(txtResultStr, " Result % Command Line ", "Groupbox title template text of Options Dialog").
    lngStringElement(ttlBrowse_pb, "Browse...", "Dialog button text").
    lngStringElement(txtOpenFormat, "[%] [%] \"$(SourceFile)\"", "Template of Format Command (Open)").
    lngStringElement(txtEditorFile, "Editor File", "Static Text of Open Command Tab").
    lngStringElement(txtArguments, "Arguments", "Static Text of Open/Exec Command Tab").
    lngStringElement(txtFormatCmd, "Format Command", "Prefix of Static Text").
    lngStringElement(txtCodePage, "Code page", "Static Text of Run Command Tab").
    lngStringElement(txtStreamMode, "Stream mode:", "Static Text of Run Command Tab").
    lngStringElement(txtRunFile, "Run File", "Static Text of Run Command Tab").
    lngStringElement(gbArgForRun, " Arguments for: ", "Groupbox Text of Run Command Tab").
    lngStringElement(txtRunMode, "Run Mode:", "Static Text of Run Command Tab").
    lngStringElement(txtReRunMode, "ReRun Mode:", "Static Text of Run Command Tab").
    lngStringElement(txtSuffix, "Suffix", "Static Text of Run Command Tab").
    lngStringElement(txtRunFormat, "[%] [%] \"$(SourceFile)\" [%]", "Template of Format Command (Run)").
    lngStringElement(txtExecFormat, "[%] [%]", "Template of Format Command (Execute)").
    lngStringElement(txtCommandLine, "Command Line", "Suffix of Groupbox title Result Command Line").
    lngStringElement(cbCmdEnabled, "Execute command enabled", "Checkbox text of Execute Command Tab").
    lngStringElement(ttlNew_pb, "&New", "Dialog button text").
    lngStringElement(ttlEdit_pb, "&Edit", "Dialog button text").
    lngStringElement(ttlCreateVirtDir, "Create Virtual Directory", "VirtDir Dialog title (New)").
    lngStringElement(ttlEditVirtDit, "Edit Virtual Directory", "VirtDir Dialog title (Edit)").
    lngStringElement(txtName, "Name:", "Static text of VirtDir Dialog").
    lngStringElement(txtDir, "Directory:", "Static text of VirtDir Dialog").
    lngStringElement(txtError, "This Name exists!", "Warning text of VirtDir Dialog").
    lngStringElement(cmdClearFilter_C, "Show All Files", "Popup menu item of Filter Ribbon button").
    lngStringElement(tipClearFilter_C, "Show All Files", "Tooltip menu item of Filter Ribbon button").
    lngStringElement(tipDnD_C, "Choose Target Node for to Move Files", "Tooltip text of D&D operation").
    lngStringElement(txtPerformFE, "Perform at Frontend Side", "Checkbox text of Execute Command Tab").
    lngStringElement(ttlFontColor_pb, "Set FG Color...", "Button text of Color Tab").
    lngStringElement(ttlSourceFontColors_st, "Font Color of File List:", "Static text of Color Tab").
    lngStringElement(ttlBGColor_pb, "Set BG Color...", "Button text of Color Tab").
    lngStringElement(ttlLanguage_st, "UI Language", "Static text of Color Tab").
    lngStringElement(ttlSourceFile_clm, "File", "Column Label of Color's ListView Control").
    lngStringElement(ttlColorValue_clm, "Color Value", "Column Label of Color's ListView Control").
    lngStringElement(cmdFilter_C, "Filter", "Menu label of Ribbon button").
    lngStringElement(tipFilter_C, "Choose Show File Type", "Tooltip of Ribbon button").
    lngStringElement(ttlMiscTab, "Misc", "Title Misc Tab of Settings dialog").
    lngStringElement(ttlSourceTypeTab, "File Type", "Title Source Tab of Settings dialog").
    lngStringElement(ttlVirtDirTab, "Virtual Directory", "Title VirtDir Tab of Settings dialog").

clauses
    setCurrentLng(CurrentLng):-
        currentLng := CurrentLng,
        currentLng_P:value := CurrentLng.

clauses
    getLanguageList() =
        if LngNode = xmlLng:getNode_nd([root(), child("lng", {(_)})]) then
            [tuple(toBoolean(currentLng = AttrName), AttrName, AttrValue)||tuple(_, AttrName, AttrValue) = LngNode:getAttribute_nd()]
        else
            [tuple(true, defaultLng, "English")]
        end if.

clauses
    getString(Key) = getStringByLng(Key, currentLng).
clauses
    getStringByLng(Key, Lng) = Value :-
        Node = xmlLng:getNode_nd([root(), child(string::concat("row",toString(Key)), {(_)})]),
        if LngNode = xmlLng:getNode_nd([current(Node), child(Lng, {(_)})]) then
        else
            LngNode = xmlLng:getNode_nd([current(Node), child(defaultLng, {(_)})])
        end if,
        xmlElement::text(Value) = LngNode:tryGetItem(xmlElement::text, 1),
        !.
    getStringByLng(Key, _Lng) = Value :-
        lngStringElement_dt(Key, Value, _Comment),
        !.

end implement ws_eventManager
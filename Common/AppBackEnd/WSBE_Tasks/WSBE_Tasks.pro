%

implement wSBE_Tasks inherits wsBE_Connector
    open core, ws_EventManager, xmlNavigate, pfc\log

clauses
    new(BackEnd) :-
        wsBE_Connector::new(BackEnd).

facts
    wsFile_V : string := erroneous.
    resetState_V : boolean := false.
    pauseRunFlag_V : boolean := false.

clauses
    getTaskQueues()=taskQueues_P.

clauses
    wsBE_InitLog(_TaskQueue) :-
        succeed().

clauses
    wsFE_Started(TaskQueueObj):-
        CurrentLng = ws_Events():currentLng_P:value,
        notify(wsBE_StartedFE, [namedValue(CurrentLng, string(ws_Events():getLanguageWSMText()))],TaskQueueObj).

clauses
    saveWorkSpace() :-
        OutputStream = outputStream_file::create(wsFile_V, stream::binary),
        wsBE_XmlDocument():saveXml(OutputStream).

clauses
    saveWorkSpaceAs(FullNameSpaceFileName) :-
        wsBE_XmlDocument() = XML_Doc,
        XML_Doc:root_P:modifyAttribute(wsFile_C, FullNameSpaceFileName),
        if unknownName_C = XML_Doc:root_P:attribute(title_C) then
            XML_Doc:root_P:modifyAttribute(title_C, filename::getName(FullNameSpaceFileName))
        end if,
        OutputStream = outputStream_file::create(FullNameSpaceFileName, stream::binary),
        XML_Doc:saveXml(OutputStream).

clauses
    create_WorkSpace(FullWorkSpaceName, TaskQueueObj) :-
        wsFile_V := FullWorkSpaceName,
        if "wsm" = filename::getExtension(FullWorkSpaceName) then
            Name = filename::getName(FullWorkSpaceName)
        else
            Name = unknownName_C
        end if,
        wsBE_XmlDocument() = XML_Doc,
        XML_Doc:root_P:clearContent(),
        XML_Doc:codePage_P := utf8,
        XML_Doc:indent_P := true,
        XML_Doc:xmlStandalone_P := xmlLite::yes,
        XML_Doc:root_P:addAttribute(wsFile_C, wsFile_V),
        XML_Doc:root_P:addAttribute(title_C, Name),
        saveWorkSpace(),
        notify(wsBE_SetApplicationTitle_C, [namedValue("wsFileName", string(wsFile_V)), namedValue("readOnly", boolean(false))],TaskQueueObj),
        XmlNodeTree = XML_Doc:getNodeTree(),
        XmlTermTree = mapXmlObjTree2xmlTermTree(XmlNodeTree),
        notify(wsBE_Created_C, [namedValue("", string(toString(XmlTermTree)))],TaskQueueObj).

    open_WorkSpace(FullNameSpaceName, TaskQueueObj) :-
        wsFile_V := FullNameSpaceName,
        !,
        XmlWorkSpace = inputStream_file::openFile(FullNameSpaceName, stream::binary),
        wsBE_XmlDocument() = XML_Doc,
        spbXmlLigntSupport::read(XmlWorkSpace, XML_Doc),
        XmlWorkSpace:close(),
        file::getFileProperties(wsFile_V, Attributes, _Size, _Creation, _LastAccess, _LastChange),
        loadProjectWSVariable(XML_Doc),
        notify(wsBE_SetApplicationTitle_C,
            [namedValue("wsFileName", string(wsFile_V)), namedValue("readOnly", boolean(toBoolean(fileSystem_api::readOnly in Attributes)))],TaskQueueObj),
        if UndefinedMacroSymList = tryUndefinedMacroSymbols(XML_Doc) then
            notify(wsBE_WSUndefinedMacronames_C, UndefinedMacroSymList,TaskQueueObj)
        end if,
        loadFolderSources(),
        XmlNodeTree = XML_Doc:getNodeTree(),
        XmlTermTree = mapXmlObjTree2xmlTermTree(XmlNodeTree),
        notify(wsBE_WSTermTree_C, [namedValue("", string(toString(XmlTermTree)))], TaskQueueObj).

predicates
    loadProjectWSVariable : (xmlDocument XML_Doc).
clauses
    loadProjectWSVariable(XML_Doc):-
        if WSVFile = XML_Doc:root_P:attribute("wsv") and file::existExactFile(WSVFile) then
            wSBE_Options():setWSVariableFile(WSVFile)
        else
            wSBE_Options():setWSVariableFile("")
        end if.

facts
    isModifyed : boolean := false.
predicates
    loadFolderSources : ().
clauses
    loadFolderSources():-
        isModifyed := false,
        foreach
            FolderObj = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", {(O) :- O:name_P = folder_C})]),
            FullFolderPath = wSBE_Options():getFullFileName(FolderObj:attribute(path_C))
        do
            ExtList = list::appendList([ExtList || wSBE_Options():getExtOptions_nd(_, ExtList)]),
            FolderSourceList = [SourceObj:attribute(fileName_C)||SourceObj = wsBE_XmlDocument():getNode_nd([current(FolderObj), descendant("*", {(O) :- O:name_P = source_C})])],
            DirFileLIst = [wSBE_Options():getShortFileName(File) ||
                File=directory::getFilesInDirectoryAndSub_nd(FullFolderPath),
                Ext = string::toLowerCase(fileName::getExtension(File, _Name)),
                list::isMember(Ext, ExtList)
                ],
            AddSourceList = list::differenceEq(string::equalIgnoreCase, DirFileLIst, FolderSourceList),
            addNewSources(FolderObj, AddSourceList),
            DelSourceList = list::differenceEq(string::equalIgnoreCase, FolderSourceList, DirFileLIst),
            removeNotExistSources(FolderObj, DelSourceList)
        end foreach,
        if isModifyed = true then
            saveWorkSpace()
        end if.

predicates
    addNewSources : (xmlElement FolderObj,string_list AddSourceList).
clauses
    addNewSources(FolderObj, AddSourceList):-
        foreach NewSourceFile in AddSourceList do
            NewSource = xmlElement::new("",source_C, FolderObj),
            NewSource:addAttribute(fileName_C, NewSourceFile),
            FolderObj:addNode(NewSource),
            isModifyed := true
        end foreach.

predicates
    removeNotExistSources : (xmlElement FolderObj,string_list DelSourceList).
clauses
    removeNotExistSources(FolderObj, DelSourceList):-
        foreach
            SourceObj = wsBE_XmlDocument():getNode_nd(
                [current(FolderObj), descendant("*", {(O) :- O:name_P = source_C, O:attribute(fileName_C) in DelSourceList})])
        do
            FolderObj:removeNode(SourceObj),
            isModifyed := true
        end foreach.

predicates
    tryUndefinedMacroSymbols : (xmlDocument XmlDoc) -> namedValue* UndefinedMacroSymList determ.
clauses
    tryUndefinedMacroSymbols(XmlDoc) = UndefinedMacroSymList :-
        ActualPath = getActualPath([]),
        UndefinedMacroSymList =
            list::append(
                [namedvalue(WSVName, string(DirValue)) ||
                wSBE_Options():getVirtualDir_nd(Name, DirValue, FlagVip, _),
                FlagVip = false,
                not(directory::existExactDirectory(DirValue)),
                string::hasPrefixIgnoreCase(Name, "$(", WSVRest),
                string::splitStringBySeparators(WSVRest, ")", WSVName, _, _)
                ],
                list::removeDuplicates(
                    [namedvalue(VirtualFName, string("")) ||
                    FolderObj = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", {(O) :- O:name_P = folder_C})]),
                    FolderPath = FolderObj:attribute(path_C),
                    string::hasPrefixIgnoreCase(FolderPath, "$(", FRest),
                    string::splitStringBySeparators(FRest, ")", VirtualFName, _, _),
                    not(wSBE_Options():existVirtualName(string::concat("$(", VirtualFName, ")")))
                    ]
                ),
                list::removeDuplicates(
                    [namedvalue(VirtualName, string("")) ||
                    Source = XmlDoc:getNode_nd(ActualPath),
                    NodeAttr = getNodeAttributes(Source),
                    NodeAttr = [namedValue(nodeID_C, string(source_C)), namedValue(xmlObj_C, _) | SourceParams],
                    File = namedValue::getNamed_String(SourceParams, fileName_C),
                    string::hasPrefixIgnoreCase(File, "$(", Rest),
                    string::splitStringBySeparators(Rest, ")", VirtualName, _, _),
                    not(wSBE_Options():existVirtualName(string::concat("$(", VirtualName, ")")))
                    ]
                )
            ),
        !,
        UndefinedMacroSymList <> [].

clauses
    addMacroSymbolDefinition(namedValue(Name, string(NewDirValue))) :-
        !,
        insertVirtualDir(Name, NewDirValue).
    addMacroSymbolDefinition(_) :-
        exception::raise_User("Unexpected Alternative").

clauses
    new_Group([namedValue(RootName, string(RootTitle)) | NodePath],TaskQueueObj) :-
        XmlPath =
            [ XmlStep ||
                namedValue(NodeId, string(Title)) = list::getMember_nd(NodePath),
                XmlStep = child(NodeId, { (O) :- O:attribute(title_C) = Title })
            ],
        XmlElement =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    XmlPath
                ]),
        !,
        LabelList = [Label||xmlElement::node(_, ChildElement) = XmlElement:getItem_nd(), list::isMember(ChildElement:name_P,[folder_C,groupNode_C]), Label = ChildElement:attribute(title_C)],
        TitleName = getNewNodeName(unNamed_C, LabelList, 0),
        NewGroup = xmlElement::new("", groupNode_C,XmlElement),
        NewGroup:addAttribute(title_C, TitleName),
        XmlElement:addNode(NewGroup),
        notify(wsBE_NewGroupCreated_C, getNodeAttributes(NewGroup, groupNode_C),TaskQueueObj),
        saveWorkSpace().
    new_Group(PathListAsString,_TaskQueueObj) :-
        exception::raise_User(string::format("NewGroup::No path % exists", PathListAsString)).

    new_Folder([namedValue(path_C, string(FullFolderName)), namedValue(RootName, string(RootTitle)) | NodePath],TaskQueueObj) :-
        XmlPath =
            [ XmlStep ||
                namedValue(NodeId, string(Title)) = list::getMember_nd(NodePath),
                XmlStep = child(NodeId, { (O) :- O:attribute(title_C) = Title })
            ],
        XmlElement =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    XmlPath
                ]),
        !,
        filename::getLastDirectory(FullFolderName, _Path) = LastDir,
        LabelList = [Label||
            xmlElement::node(_, ChildElement) = XmlElement:getItem_nd(),
                list::isMember(ChildElement:name_P,[folder_C,groupNode_C]),
                Label = ChildElement:attribute(title_C)],
        TitleName = getNewNodeName(LastDir, LabelList, 0),
        NewFolder = xmlElement::new("", folder_C, XmlElement),
        NewFolder:addAttribute(title_C, TitleName),
        NewFolder:addAttribute(path_C, wSBE_Options():getShortFileName(FullFolderName)),
        XmlElement:addNode(NewFolder),
        ExtList = list::appendList([ExtList ||
            wSBE_Options():getExtOptions_nd(_, ExtList)]),
        DirFileLIst = [wSBE_Options():getShortFileName(File) ||
            File=directory::getFilesInDirectoryAndSub_nd(FullFolderName),
            Ext = string::toLowerCase(fileName::getExtension(File, _Name)),
            list::isMember(Ext, ExtList)
            ],
        addNewSources(NewFolder, DirFileLIst),
        notify(wsBE_NewFolderCreated_C, getNodeAttributes(NewFolder, folder_C),TaskQueueObj),
        saveWorkSpace().
    new_Folder(PathListAsString,_TaskQueue) :-
        exception::raise_User(string::format("NewFolder::No path % exists", PathListAsString)).

class predicates
%predicates
    getNewNodeName : (string Name,string* LabelList,integer Index) -> string NewName.
clauses
    getNewNodeName(Name, LabelList, Index) = NewName :-
        Name1 = if Index = 0 then Name else string::format("%s%d", Name, Index) end if,
        if list::isMemberEq(string::equalIgnoreCase, Name1, LabelList) then
            NewName = getNewNodeName(Name, LabelList, Index+1)
        else
            NewName = Name1
        end if.

clauses
    setTitle([namedValue(RootName, string(RootTitle)) | NodePath], NewTitle) :-
        XmlPath =
            [ XmlStep ||
                namedValue(NodeId, string(Title)) = list::getMember_nd(NodePath),
                XmlStep = child(NodeId, { (O) :- O:attribute(title_C) = Title })
            ],
        XmlElement =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    XmlPath
                ]),
        !,
        XmlElement:addAttribute(title_C, NewTitle),
        saveWorkSpace().
    setTitle(PathListAsString, Title) :-
        exception::raise_User(string::format("SetTitle::No path % exists for %", PathListAsString, Title)).

clauses
    removeNode([namedValue(RootName, string(RootTitle)) | NodePath], Title,TaskQueueObj) :-
        XmlPath =
            [ XmlStep ||
                namedValue(NodeId, string(StepTitle)) = list::getMember_nd(NodePath),
                XmlStep = child(NodeId, { (O) :- O:attribute(title_C) = StepTitle })
            ],
        XmlElement =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    XmlPath
                ]),
        ElToBeDeleted = wsBE_XmlDocument():getNode_nd([current(XmlElement), child("*", { (O) :- O:attribute(title_C) = Title })]),
        NodeAttributes = getNodeAttributes(ElToBeDeleted, ElToBeDeleted:name_P),
        !,
        XmlElement:removeNode(ElToBeDeleted),
        notify(wsBE_NodeRemoved_C, NodeAttributes,TaskQueueObj),
        saveWorkSpace().
    removeNode(PathList, Title,_TaskQueueObj) :-
        exception::raise_User(string::format("RemoveNode: No Node exists for path % and Nodewith Title %", PathList, Title)).

clauses
    moveAbove(SourcePath, TargetPath) :-
        tuple(Source, Target, Position_T) = prepareObjectsToMove(SourcePath, TargetPath),
        Parent_T = Target:parent_P,
        convert(xmlElement, Parent_T):tryInsertItem(xmlElement::node, Position_T - 1, xmlElement::node(Source:name_P, Source)),
        !,
        saveWorkSpace().
    moveAbove(_SourcePath, _TargetPath) :-
        exception::raise_User("Unexpected Alternative").

clauses
    moveBelow(SourcePath, TargetPath) :-
        tuple(Source, Target, Position_T) = prepareObjectsToMove(SourcePath, TargetPath),
        Parent_T = Target:parent_P,
        convert(xmlElement, Parent_T):tryInsertItem(xmlElement::node, Position_T, xmlElement::node(Source:name_P, Source)),
        !,
        saveWorkSpace().
    moveBelow(_SourceNodePath, _TargetNodePath) :-
        exception::raise_User("Unexpected Alternative").

clauses
    moveOnTop(SourcePath, TargetPath) :-
        tuple(Source, Target, _Position_T) = prepareObjectsToMove(SourcePath, TargetPath),
        Target:tryInsertItem(xmlElement::node, 0, xmlElement::node(Source:name_P, Source)),
        !,
        saveWorkSpace().
    moveOnTop(_SourcePath, _TargetPath) :-
        exception::raise_User("Unexpected Alternative").

clauses
    moveOnBottom(SourcePath, TargetPath) :-
        tuple(Source, Target, _Position_T) = prepareObjectsToMove(SourcePath, TargetPath),
        NoOfNodes = Target:count(xmlElement::node),
        Target:tryInsertItem(xmlElement::node, NoOfNodes, xmlElement::node(Source:name_P, Source)),
        !,
        saveWorkSpace().
    moveOnBottom(_SourcePath, _TargetPath) :-
        exception::raise_User("Unexpected Alternative").

clauses
    addSource([namedValue(sourceFile_C, string(FileName)), _Root | NodePath],TaskQueueObj) :-
        SourcePath = getSourcePath(NodePath),
        ActualPath = [root(), self({ (_) }) | SourcePath],
        tuple(GroupObj, SPath) = wsBE_XmlDocument():getNodeAndPath_nd(ActualPath),
        NewSource = xmlElement::new("",source_C, GroupObj),
        NewSource:addAttribute(fileName_C, wSBE_Options():getShortFileName(FileName)),
        GroupObj:addNode(NewSource),
        saveWorkSpace(),
        notify(wsBE_AddSource_C, [namedValue(nodeIDList_C, string(toString(tuple(GroupObj:name_P,GroupObj:attribute(title_C),toString([GroupObj|SPath])))))|getNodeAttributes(NewSource)],TaskQueueObj),
        !.
    addSource(_NodePath,_TaskQueueObj) :-
        exception::raise_User("Unexpected Alternative").

clauses
    deleteSourceList([namedValue(nodeIDList_C, string(NodeIdList)), _Root | NodePath]) :-
        SourcePath = getSourcePath(NodePath),
        ActualPath = [root(), self({ (_) }) | SourcePath],
        GroupObj = wsBE_XmlDocument():getNode_nd(ActualPath),
        !,
        foreach NodeObjID  in toTerm(NodeIdList) do
            foreach Source = wsBE_XmlDocument():getNode_nd([current(GroupObj), descendant("*", { (O) :- toString(O) = NodeObjID })]) do
                if folder_C = convert(xmlElement, Source:parent_P):name_P then
                    if _ = Source:tryGetAttribute(status_C) then
                        Source:modifyAttribute(status_C, toString(excludedStatus_C))
                    else
                        Source:addAttribute(status_C, toString(excludedStatus_C))
                    end if
                else
                    convert(xmlElement, Source:parent_P):removeNode(Source)
                end if
            end foreach
        end foreach,
        saveWorkSpace().
    deleteSourceList(_ParamList) :-
        exception::raise_User("Unexpected Alternative").

clauses
    getExtListAddNewSource(TaskQueueObj):-
        ExtFileList = list::appendList([[SourceTypeName, MaskList] ||
            wSBE_Options():getExtOptions_nd(SourceTypeName, ExtList),
            MaskList = string::concatWithDelimiter([string::concat("*.", Ext)||Ext in ExtList],"; ")]),
        notify(wsBE_ExtFileList_C, [namedValue("mask", string(toString(ExtFileList)))],TaskQueueObj).

clauses
    addSourcesFromFolder([namedValue(folderName_C, string(FolderName)), _Root | NodePath],TaskQueueObj) :-
        SourcePath = getSourcePath(NodePath),
        ActualPath = [root(), self({ (_) }) | SourcePath],
        tuple(GroupObj, SPath) = wsBE_XmlDocument():getNodeAndPath_nd(ActualPath),
        ExtList = list::appendList([ExtList || wSBE_Options():getExtOptions_nd(_, ExtList)]),
        !,
        foreach
            File = directory::getFilesInDirectoryAndSub_nd(FolderName),
                Ext = string::toLowerCase(fileName::getExtension(File, _Name)),
                list::isMember(Ext, ExtList),
            NewSource = xmlElement::new("", source_C, GroupObj),
            NewSource:addAttribute(fileName_C, wSBE_Options():getShortFileName(File)),
            GroupObj:addNode(NewSource),
            Attribute=GroupObj:attribute(title_C)
        do
            notify(wsBE_AddSource_C, [namedValue(nodeIDList_C, string(toString(tuple(GroupObj:name_P,Attribute,toString([GroupObj|SPath])))))|getNodeAttributes(NewSource)],TaskQueueObj)
        end foreach,
        saveWorkSpace().
    addSourcesFromFolder(_ParamList,_TaskQueueObj) :-
        exception::raise_User("Unexpected Alternative").

clauses
    getNodeContent([_Root | NodePath],TaskQueueObj) :-
        !,
        SourcePath = getSourcePath(NodePath),
        ActualPath = getActualPath(SourcePath),
        foreach
            tuple(Source,[_|SPath]) = wsBE_XmlDocument():getNodeAndPath_nd(ActualPath),
                SPath = [Group|_],
                GroupName = Group:tryGetAttribute(title_C)
        do
            notify(wsBE_Source_C, [namedValue(nodeIDList_C, string(toString(tuple(Group:name_P,GroupName,toString(SPath)))))|getNodeAttributes(Source)],TaskQueueObj)
        end foreach,
        wSBE_Options():updateOptionsNotifyFE(TaskQueueObj).
    getNodeContent(_NodePath,_TaskQueueObj).

facts
    runDone_V:integer := 0.
    runFailed_V:integer := 0.
    runNoPath_V:integer := 0.
    runTotalCount_V:integer := 0.
clauses
    updateNodeContent([Filter, _Root | NodePath],TaskQueueObj) :-
        Filter = namedValue("filter", string(FilterStr)),
        !,
        SourcePath = getSourcePath(NodePath),
        ActualPath = getActualPath(SourcePath),
        runTotalCount_V := 0,
        runDone_V := 0,
        runFailed_V := 0,
        runNoPath_V := 0,
        foreach
            Source = wsBE_XmlDocument():getNode_nd(ActualPath),
            if FilterList = tryToTerm(string_list, FilterStr) then
                FileName = Source:tryGetAttribute(fileName_C),
                Ext = string::toLowerCase(fileName::getExtension(FileName,_Name)),
                Ext in FilterList
            end if
        do
            notify(wsBE_UpdateSourceStatus_C, [namedValue("sourceID",string(toString(Source)))|getNodeAttributes(Source)],TaskQueueObj),
            runTotalCount_V := runTotalCount_V + 1,
            if Status = tryToTerm(integer,Source:tryGetAttribute(status_C)) then
                if Status = doneStatus_C then
                    runDone_V := runDone_V + 1
                elseif Status = failedStatus_C then
                    runFailed_V := runFailed_V + 1
                elseif Status = noPathStatus_C then
                    runNoPath_V := runNoPath_V + 1
                end if
            end if
        end foreach,
        notify(wsBE_UpdateRunStatus_C,
            [
            namedValue(statusPrefix_C,string(ws_Events():getString(totalReady_C))),
            namedValue(totalCount_C,integer(runTotalCount_V)),
            namedValue(runDone_C,integer(runDone_V)),
            namedValue(runFailed_C,integer(runFailed_V)),
            namedValue(runNoPath_C,integer(runNoPath_V))
            ],TaskQueueObj),
        wSBE_Options():updateOptionsNotifyFE(TaskQueueObj).
    updateNodeContent(_NodePath,_TaskQueueObj).

clauses
    moveSourceUp(ItemIDSelected, TargetItemID, TreeNodePath,TaskQueueObj) :-
        moveSource(-1, ItemIDSelected, TargetItemID, TreeNodePath,TaskQueueObj).

clauses
    moveSourceDown(ItemIDSelected, TargetItemID, TreeNodePath,TaskQueueObj) :-
        moveSource(0, ItemIDSelected, TargetItemID, TreeNodePath,TaskQueueObj).

clauses
    moveSourceToTree(ToTop, ItemIDSelected, [_Parent | TreeNodePath],TaskQueueObj) :-
        SourcePath = getSourcePath(TreeNodePath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        SourceObj = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = ItemIDSelected })]),
        Parent_S = SourceObj:parent_P,
        convert(xmlElement, Parent_S):removeNode(SourceObj),
        if true = ToTop then
            TreeNodeObj:tryInsertItem(xmlElement::node, 0, xmlElement::node(SourceObj:name_P, SourceObj))
        else
            TreeNodeObj:addNode(SourceObj)
        end if,
        !,
        notify(wsBE_MoveToTree_C, [namedValue(itemSelected_C, string(ItemIDSelected))],TaskQueueObj),
        saveWorkSpace().
    moveSourceToTree(_ToTop, _ItemIDSelected, _TreeNodePath,_TaskQueueObj).

clauses
    cloneSourceToTree(ToTop, ItemIDSelected, [_Parent | TreeNodePath],TaskQueueObj) :-
        SourcePath = getSourcePath(TreeNodePath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        SourceObj = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = ItemIDSelected })]),
        CloneObj = xmlElement::clone(SourceObj),
        if true = ToTop then
            TreeNodeObj:tryInsertItem(xmlElement::node, 0, xmlElement::node(CloneObj:name_P, CloneObj))
        else
            TreeNodeObj:addNode(CloneObj)
        end if,
        !,
        notify(wsBE_MoveToTree_C, [namedValue(itemSelected_C, string(ItemIDSelected))],TaskQueueObj),
        saveWorkSpace().
    cloneSourceToTree(_ToTop, _ItemIDSelected, _TreeNodePath,_TaskQueueObj).

predicates
    moveSource : (integer Direction, string ItemIDSelected, string TargetItemID, namedValue* TreeNodePath,object TaskQueueObj).
clauses
    moveSource(Direction, ItemIDSelected, TargetItemID, [_Parent | TreeNodePath],TaskQueueObj) :-
        SourcePath = getSourcePath(TreeNodePath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        SourceObj = wsBE_XmlDocument():getNode_nd([current(TreeNodeObj), descendant("*", { (O) :- toString(O) = ItemIDSelected })]),
        TargetObj = wsBE_XmlDocument():getNode_nd([current(TreeNodeObj), descendant("*", { (O) :- toString(O) = TargetItemID })]),
        Parent_S = SourceObj:parent_P,
        Parent_T = TargetObj:parent_P,
        convert(xmlElement, Parent_S):removeNode(SourceObj),
        Position_T = TargetObj:position(),
        convert(xmlElement, Parent_T):tryInsertItem(xmlElement::node, Position_T + Direction, xmlElement::node(SourceObj:name_P, SourceObj)),
        !,
        ActualPath = getActualPath(SourcePath),
        foreach
            tuple(Source,[_|SPath]) = wsBE_XmlDocument():getNodeAndPath_nd(ActualPath),
            SPath = [Group|_],
            GroupName = Group:tryGetAttribute(title_C)
        do
            notify(wsBE_Source_C, [namedValue(nodeIDList_C, string(toString(tuple(Group:name_P,GroupName,toString(SPath)))))|getNodeAttributes(Source)],TaskQueueObj)
        end foreach,
        saveWorkSpace().
    moveSource(_Direction, _ItemIDSelected, _ItemIDBelow, _TreeNodePath,_TaskQueueObj) :-
        exception::raise_User("Unexpected Alternative").

clauses
    execute(SourceIDList, SourceNodePath,TaskQueueObj) :-
        SourceNodePath = [_Parent | TargetPath],
        SourcePath = getSourcePath(TargetPath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        !,
        foreach namedValue(_, string(NodeID)) in SourceIDList do
            if false = pauseRunFlag_V then
                foreach
                    SourceObj = wsBE_XmlDocument():getNode_nd([current(TreeNodeObj), descendant("*", { (O1) :- toString(O1) = NodeID })]),
                    FileName = SourceObj:tryGetAttribute(fileName_C)
                do
                    wsBE_Performer():execSource(SourceObj, FileName,TaskQueueObj)
                end foreach
            end if
        end foreach.
    execute(_SourceIDList, _SourceNodePath,_TaskQueueObj).

clauses
    checkFile(SourceNodePath, TaskQueueObj):-
        foreach
            SourceNodePath = [_Parent | TargetPath],
            SourcePath = getSourcePath(TargetPath),
            TreePath = [root(), self({ (_) }) | SourcePath],
            TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
            !,
            SourceObj = wsBE_XmlDocument():getNode_nd([current(TreeNodeObj), descendant("*", { (O) :- O:name_P = source_C })]) and
            FileName = SourceObj:tryGetAttribute(fileName_C)
        do
            wsBE_Performer():checkFile(SourceObj, FileName, TaskQueueObj)
        end foreach.

clauses
    perform(SourceIDList, SourceNodePath, IsContinue, TaskQueueObj) :-
        SourceNodePath = [_Parent | TargetPath],
        SourcePath = getSourcePath(TargetPath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        !,
        if false = IsContinue then
            wsBE_Performer():runDone_V := 0,
            wsBE_Performer():runFailed_V := 0,
            wsBE_Performer():runNoPath_V := 0,
            wsBE_Performer():runNoRule_V := 0,
            wsBE_Performer():runTotalCount_V := list::length(SourceIDList)
        end if,
        wsBE_Performer():stopRunFlag_P := false,
        performSourceIDList_F := SourceIDList,
        treeNodeObj_F := TreeNodeObj,
        sourceNodePath_F := SourceNodePath,
        doRunSource(TaskQueueObj).
    perform(_SourceIDList, _TargetPath,_IsContinue,_TaskQueueObj) :-
        exception::raise_User("Unexpected Alternative").

facts
    performSourceIDList_F : namedValue_list := [].
    treeNodeObj_F : xmlElement := erroneous.
    sourceNodePath_F : namedValue_list := [].
clauses
    doRunSource(TaskQueueObj):-
        if [namedValue(RunMode, string(NodeID))|RestList] = performSourceIDList_F then
            performSourceIDList_F := RestList,
            foreach
                SourceObj = wsBE_XmlDocument():getNode_nd([current(treeNodeObj_F), descendant("*", { (O1) :- toString(O1) = NodeID })]) and
                FileName = SourceObj:tryGetAttribute(fileName_C)
            do
                wsBE_Performer():runSource(SourceObj, RunMode, FileName,TaskQueueObj)
            end foreach
        else
            saveWorkSpace(),
            if false = pauseRunFlag_V then
                wsBE_Performer():stopRunFlag_P := false,
                notify(wsBE_UpdateNodeContent_C, sourceNodePath_F,TaskQueueObj)
            end if
        end if.

clauses
    stopRun(TaskQueueObj) :-
        performSourceIDList_F := [],
        wsBE_Performer():stopRun(TaskQueueObj).

clauses
    pauseRun(Pause,TaskQueueObj) :-
        pauseRunFlag_V := Pause,
        if true = Pause then
            stopRun(TaskQueueObj)
        else
            wsBE_Performer():stopRunFlag_P := false
        end if.

clauses
    handleStreamFrontEnd(EndRunSource, Line,TaskQueueObj):-
        wsBE_Performer():handleStreamFrontEnd(EndRunSource, Line,TaskQueueObj),
        if EndRunSource = true then
            doRunSource(TaskQueueObj)
        end if.


clauses
    resetStatus(SourceNodePath,TaskQueueObj) :-
        SourceNodePath = [_Parent | TargetPath],
        !,
        SourcePath = getSourcePath(TargetPath),
        ActualPath = getActualPath(SourcePath),
        SourceList = [ Source || Source = wsBE_XmlDocument():getNode_nd(ActualPath) ],
        setResetStatus(SourceList),
        notify(wsBE_UpdateNodeContent_C, SourceNodePath,TaskQueueObj).
    resetStatus(_,_TaskQueueObj) .

clauses
    selResetStatus(SourceIDList, SourceNodePath,TaskQueueObj):-
        SourceNodePath = [_Parent | TargetPath],
        SourcePath = getSourcePath(TargetPath),
        TreePath = [root(), self({ (_) }) | SourcePath],
        TreeNodeObj = wsBE_XmlDocument():getNode_nd(TreePath),
        !,
        SourceList = [Source||
            namedValue(_, string(NodeID)) in SourceIDList,
            Source = wsBE_XmlDocument():getNode_nd(
                [current(TreeNodeObj),
                descendant("*", { (O1) :- toString(O1) = NodeID })
                ])
            ],
        setResetStatus(SourceList),
        notify(wsBE_UpdateNodeContent_C, SourceNodePath,TaskQueueObj).
    selResetStatus(_, _,_TaskQueueObj).

predicates
    setResetStatus : (xmlElement* SourceList).
clauses
    setResetStatus(SourceList):-
        if
            clearIfExistNoDoneStatus(SourceList)
        then
            foreach
                Source1 in SourceList,
                    Status = tryToTerm(integer,Source1:tryGetAttribute(status_C)),
                    excludedStatus_C <> Status
            do
                Source1:removeAttribute(status_C),
                if _ = Source1:tryGetAttribute(warnings_C) then
                    Source1:removeAttribute(warnings_C)
                end if,
                if _ = Source1:tryGetAttribute(datetime_C) then
                    Source1:removeAttribute(datetime_C)
                end if
            end foreach
        end if,
        saveWorkSpace().

predicates
    clearIfExistNoDoneStatus : (xmlElement* SourceList) determ.
clauses
    clearIfExistNoDoneStatus(SourceList) :-
        resetState_V := false,
        StatusList = [toString(StatusC)||StatusC in [noPathStatus_C,stoppedStatus_C,exceptedStatus_C,noRuleStatus_C,failedStatus_C]],
        Source in SourceList,
            Source:tryGetAttribute(status_C) in StatusList,
                Source:removeAttribute(status_C),
                if _ = Source:tryGetAttribute(errors_C) then
                    Source:removeAttribute(errors_C)
                end if,
                if _ = Source:tryGetAttribute(warnings_C) then
                    Source:removeAttribute(warnings_C)
                end if,
                if _ = Source:tryGetAttribute(datetime_C) then
                    Source:removeAttribute(datetime_C)
                end if,
                resetState_V := true,
        fail.
    clearIfExistNoDoneStatus(_) :-
        resetState_V = false.

clauses
    getShortFileName(FullFileName, TaskQueueObj):-
        notify(wsBE_SetShortName_C, [namedValue(FullFileName, string(wSBE_Options():getShortFileName(FullFileName)))],TaskQueueObj).

clauses
    getFullFileName(ShortFileName,TaskQueueObj):-
        notify(wsBE_SetFullName_C, [namedValue(ShortFileName, string(wSBE_Options():getFullFileName(ShortFileName)))],TaskQueueObj).

clauses
    getExtOptionsList(NodeId,TaskQueueObj) :-
        OptionsList = wSBE_Options():loadOptionsBySource(NodeId),
        notify(wsBE_LocalExtOptionsList_C, [namedValue(NodeId, string(toString(OptionsList)))],TaskQueueObj).

clauses
    getFrontEndOptions(TaskQueueObj) :-
        ExtOptionsList = getExtOptionsList(),
        notify(wsBE_ExtOptionsList_C, ExtOptionsList,TaskQueueObj),
        FEOptionsList = wSBE_Options():getFrontEndOptionsList(),
        notify(wsBE_FEOptionsList_C, FEOptionsList,TaskQueueObj),
        SourceColorsList = [namedValue(ColorName, string(toString(tuple(FGColor, BGColor)))) || wSBE_Options():getSourceColor_nd(ColorName, FGColor, BGColor)],
        notify(wsBE_UpdateSourceColors_C, SourceColorsList,TaskQueueObj),
        !.

clauses
    setFrontEndOptions(FEOptionsList):-
        wSBE_Options():setFrontEndOptions(FEOptionsList).

clauses
    getSettings(TaskQueueObj):-
        ExtOptionsList = getExtOptionsList(),
        VirtualDirList = [ namedValue(Name, string(toString(tuple(VirtualDir, IsVip)))) || wSBE_Options():getVirtualDir_nd(Name, VirtualDir, IsVip) ],
        SourceColorsList = [namedValue(ColorName, string(toString(tuple(FGColor, BGColor)))) || wSBE_Options():getSourceColor_nd(ColorName, FGColor, BGColor)],
        SelectSourceType = wSBE_Options():getSelectSourceType(),
        PerformGroupList = [namedValue(Synname, string(CompName)) || wSBE_Options():getCompRun_nd(CompName, Synname)],
        LanguageList = ws_Events():getLanguageList(),
        SettingsList = list::append([namedValue("lang",string(toString(LanguageList)))],
                                                [namedValue("count", string(toString(tuple(list::length(ExtOptionsList),list::length(VirtualDirList),SelectSourceType,wSBE_Options():wsv_file))))],
                                                ExtOptionsList, VirtualDirList, SourceColorsList),
        SettingsList6 = list::append(SettingsList, PerformGroupList),
        notify(wsBE_Settings_C, SettingsList6,TaskQueueObj).

clauses
    updateWSVSettings(TaskQueueObj):-
        VirtualDirList = [ namedValue(Name, string(toString(tuple(VirtualDir, IsVip)))) || wSBE_Options():getVirtualDir_nd(Name, VirtualDir, IsVip) ],
        notify(wsBE_UpdateWSV_C, VirtualDirList, TaskQueueObj).

clauses
    getWSVariablesForLO(TaskQueueObj):-
        VirtualDirList = [ namedValue(Name, string(toString(tuple(VirtualDir, IsVip)))) || wSBE_Options():getVirtualDir_nd(Name, VirtualDir, IsVip) ],
        notify(wsBE_GetWSVariablesForLO_C, VirtualDirList, TaskQueueObj).

predicates
    getExtOptionsList : () -> namedValue*.
clauses
    getExtOptionsList() =
        [ namedValue(Name, string(toString(OptionsList))) ||
          wSBE_Options():getExtOptions_nd(Name, ExtList),
                [Ext|_] = ExtList,
                OptionsList = wSBE_Options():loadOptionsByExt(Ext)
        ].

clauses
    updateExtOptionsList(ExtOptionsList):-
        wSBE_Options():updateExtOptionsList(ExtOptionsList).

clauses
    updateSourceLocalOptions(ExtOptionsList):-
        wSBE_Options():updateSourceLocalOptions(ExtOptionsList).

clauses
    updateSelectSourceType(SelectSourceType):-
        wSBE_Options():updateSelectSourceType(SelectSourceType).

clauses
    updateSourceColors(SourceColorsList,TaskQueueObj):-
        wSBE_Options():updateSourceColors(SourceColorsList),
        notify(wsBE_UpdateSourceColors_C, SourceColorsList,TaskQueueObj),
        ExtOptionsList = getExtOptionsList(),
        notify(wsBE_ExtOptionsList_C, ExtOptionsList,TaskQueueObj).

clauses
    updateUILanguage(NewUILanguage):-
        wSBE_Options():updateUILanguage(NewUILanguage).

clauses
    updateExtOptions([namedValue(NameExt, string(OptionsValuesStr))],TaskQueueObj):-
        !,
        wSBE_Options():updateOptionsByExt(NameExt, OptionsValuesStr,TaskQueueObj).
    updateExtOptions(_,_TaskQueueObj).

clauses
    updateOptionsNotifyFE(NameValues,TaskQueueObj):-
        notify(wsBE_UpdateOptions_C, NameValues,TaskQueueObj).

clauses
    insertVirtualDir(Name, NewDirValue) :-
        wSBE_Options():insertVirtualDir(Name, NewDirValue),
        updateNoPathStatus().

clauses
    setVipVirtualDir(VipVirtualDir):-
        wSBE_Options():setVipVirtualDir(VipVirtualDir).

clauses
    setWSVariableFile(NewWSVFile):-
        wSBE_Options():setWSVariableFile(NewWSVFile),
        wsBE_XmlDocument() = XML_Doc,
        if file::existExactFile(NewWSVFile) then
            if _ = XML_Doc:root_P:attribute("wsv") then
                XML_Doc:root_P:modifyAttribute("wsv", NewWSVFile)
            else
                XML_Doc:root_P:addAttribute("wsv", NewWSVFile)
            end if
        elseif NewWSVFile = "", XML_Doc:root_P:tryRemoveAttribute("wsv")  then
        end if,
        saveWorkSpace().

predicates
    updateNoPathStatus : ().
clauses
    updateNoPathStatus() :-
        ActualPath = getActualPath([]),
        foreach
            Source = wsBE_XmlDocument():getNode_nd(ActualPath),
                NodeAttr = getNodeAttributes(Source),
                NodeAttr = [namedValue(nodeID_C, string(source_C)), namedValue(xmlObj_C, _) | SourceParams],
                toString(noPathStatus_C) = namedValue::tryGetNamed_String(SourceParams, status_C),
                File = namedValue::getNamed_String(SourceParams, fileName_C),
                Full = wsBE_Options():getFullFileName(File),
                file::existExactFile(Full)
        do
            Source:removeAttribute(status_C)
        end foreach,
        saveWorkSpace().

class predicates
% predicates
    getActualPath : (xmlNavigate::step_D* SourcePath) -> xmlNavigate::step_D* ActualPath.
clauses
    getActualPath(SourcePath) =
            list::append([root(), self({ (_) })|SourcePath],
                [
                    descendant_or_self("*",
                        { (O) :-
                            (O:name_P = source_C),
                            !
                        })
                ]).

class predicates
% predicates
    getSourcePath : (namedValue* NodePath) -> xmlNavigate::step_D* SourcePath.
clauses
    getSourcePath(NodePath) =
            [ XmlStep_S ||
                namedValue(NodeId_S, string(Title_S)) = list::getMember_nd(NodePath),
                XmlStep_S = child(NodeId_S, { (O) :- O:attribute(title_C) = Title_S })
            ].

clauses
    updateVirtualDir(Name, NewDirValue) :-
        wSBE_Options():updateVirtualDir(Name, NewDirValue).

clauses
    deleteVirtualDir(Name) :-
        wSBE_Options():deleteVirtualDir(Name).

clauses
    openSource([_ | NodePath], NodeIdToOpen,TaskQueueObj) :-
        SourcePath = getSourcePath(NodePath),
        TreeNodeObj = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }) | SourcePath]),
        SourceFileNameList =
            [ tuple(SourceObj, FileName) ||
                % there MUST be the only one element!
                SourceObj = wsBE_XmlDocument():getNode_nd([current(TreeNodeObj), descendant("*", { (O1) :- toString(O1) = NodeIdToOpen })]),
                FileName = SourceObj:tryGetAttribute(fileName_C),
                !
            ],
        SourceFileNameList = [tuple(SourceObj, SourceFileName)],
        !,
        wsBE_Performer():openSource(SourceObj, SourceFileName,TaskQueueObj).

    openSource(_NodePath, _NodeIdToOpen,_TaskQueueObj) :-
        exception::raise_User("Unexpected Alternative").

clauses
    showSourceInTree(NodeID,TaskQueueObj):-
        tuple(_, [_|NPRest]) = wsBE_XmlDocument():getNodeAndPath_nd([root(), self({ (_) }), descendant("*", { (O1) :- toString(O1) = NodeId })]),
        !,
        NodePath = list::reverse(NPRest),
        SelectNodePath = [namedValue(Node:name_P,string(Node:attribute(title_C)))||Node in NodePath],
        notify(wsBE_ShowSourceInTree_C, SelectNodePath,TaskQueueObj).
    showSourceInTree(_NodeID,_TaskQueueObj).

clauses
    restoreDeletedSource(NodeID):-
        Source = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O1) :- toString(O1) = NodeId })]),
        !,
        Source:removeAttribute(status_C),
        saveWorkSpace().
    restoreDeletedSource(_NodeID).

/***** Common Predicates *******/
predicates
    prepareObjectsToMove : (namedValue* SourceNodePath, namedValue* TargetNodePath) -> tuple{xmlElement Source, xmlElement Target, integer Position_T}
        nondeterm.
clauses
    prepareObjectsToMove([namedValue(RootName, string(RootTitle)) | SourceNodePath], [_Head | TargetNodePath]) = tuple(Source, Target, Position_T) :-
        SourcePath = getSourcePath(SourceNodePath),
        Source =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    SourcePath
                ]),
        Parent_S = Source:parent_P,
        convert(xmlElement, Parent_S):removeNode(Source),
        TargetPath = getSourcePath(TargetNodePath),
        Target =
            wsBE_XmlDocument():getNode_nd(
                [
                    root(),
                    self(
                        { (O) :-
                            O:name_P = RootName,
                            O:attribute(title_C) = RootTitle
                        }) |
                    TargetPath
                ]),
        Position_T = Target:position().

predicates
    mapXmlObjTree2xmlTermTree : (spbTree::tree{string NodeName, xmlElement}) -> spbTree::tree{string NodeId, namedValue*}.
clauses
    mapXmlObjTree2xmlTermTree(spbTree::tree(NodeID, XmlElement, ChildTreeList)) = spbTree::tree(NodeID, NodeAttributes, []) :-
        [] =
            [ "" ||
                spbTree::tree(_NodeName, NodeObj, _ChildList) in ChildTreeList,
                _Title = NodeObj:tryGetAttribute(title_C)
            ],
        !,
        NodeAttributes = getNodeAttributes(XmlElement, NodeId).

    mapXmlObjTree2xmlTermTree(spbTree::tree(NodeID, XmlElement, [])) = spbTree::tree(NodeID, NodeAttributes, []) :-
        !,
        NodeAttributes = getNodeAttributes(XmlElement, NodeId).

    mapXmlObjTree2xmlTermTree(spbTree::tree(NodeID, XmlElement, [LeftNode | RestNodeList])) =
            spbTree::tree(NodeID, NodeAttributes, [NewLeftNode | NewRestNodeList]) :-
        NodeAttributes = getNodeAttributes(XmlElement, NodeId),
        NewLeftNode = mapXmlObjTree2xmlTermTree(LeftNode),
        !,
        spbTree::tree(_NodeID, _NodeAttributes, NewRestNodeList) = mapXmlObjTree2xmlTermTree(spbTree::tree(NodeID, XmlElement, RestNodeList)).

predicates
    getNodeAttributes : (xmlElement XmlElement) -> namedValue*.
    getNodeAttributes : (xmlElement XmlElement, string NodeId) -> namedValue*.

clauses
    getNodeAttributes(XmlElement) = getNodeAttributes(XmlElement, XmlElement:name_P).

    getNodeAttributes(XmlElement, NodeId) =
            [namedValue(nodeID_C, string(NodeId)), namedValue(xmlObj_C, string(toString(XmlElement))) | FullNodeAttributes] :-
        NodeAttributes =
            [ NodeAttribute ||
                tuple(_AttrPrefix, AttrName, AttrValue) = XmlElement:getAttribute_nd(),
                NodeAttribute = namedValue(AttrName, string(AttrValue))
            ],
        if FileName = XmlElement:attribute(fileName_C) then
            FullNodeAttributes = [namedValue("fullFileName", string(wsBE_Options():getFullFileName(FileName)))| NodeAttributes]
        else
            FullNodeAttributes = NodeAttributes
        end if.

end implement wSBE_Tasks

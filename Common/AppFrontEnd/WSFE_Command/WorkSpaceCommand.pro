%
implement workSpaceCommand
    inherits wsFE_Connector
    open core, stdio, vpiDomains, treeControl{treeNode_std}, ribbonControl, ws_eventManager,wSFE_Command

facts - command_FB
    newWorkSpaceCmd : command:=erroneous.
    loadWorkSpaceCmd : command:=erroneous.
    workSpaceMenuCmd : menucommand:=erroneous.

    addGroupCmd : command:=erroneous.
    addSubGroupCmd : command:=erroneous.
    addFolderCmd : command:=erroneous.
    addSubFolderCmd : command:=erroneous.
    treeMenuCmd : menucommand:=erroneous.

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        wsFE_SourceTree():treeControl_P:addSelectEndListener(onTreeNodeSelected),
        wsFE_SourceList():sourceList_P:addSelectEndListener(onListRowSelected).

clauses
    addChangeListener():-
        ws_Events():changeLanguageEvent:addListener(
            {:-
            newWorkSpaceCmd:menuLabel:=ws_Events():getString(cmdNewWS_C),
            newWorkSpaceCmd:ribbonLabel:=string::concat(ws_Events():getString(cmdNewWS_C), " WS"),
            newWorkSpaceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipNewWS_C)),
            loadWorkSpaceCmd:menuLabel:=ws_Events():getString(cmdOpenWS_C),
            loadWorkSpaceCmd:ribbonLabel:=string::concat(ws_Events():getString(cmdOpenWS_C), " WS"),
            loadWorkSpaceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipOpenWS_C)),
            addGroupCmd:menuLabel:=ws_Events():getString(cmdAddGroup_C),
            addGroupCmd:ribbonLabel:=ws_Events():getString(cmdAddGroup_C),
            addGroupCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddGroup_C)),
            addSubGroupCmd:menuLabel:=ws_Events():getString(cmdAddSubGroup_C),
            addSubGroupCmd:ribbonLabel:=ws_Events():getString(cmdAddSubGroup_C),
            addSubGroupCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSubGroup_C)),
            addFolderCmd:menuLabel:=ws_Events():getString(cmdAddFolder_C),
            addFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddFolder_C),
            addFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddFolder_C)),
            addSubFolderCmd:menuLabel:=ws_Events():getString(cmdAddSubFolder_C),
            addSubFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddSubFolder_C),
            addSubFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSubFolder_C))
            }).

clauses
    initWS_Menu(Win)=WorkSpaceMenu:-
        _LoadWorkSpaceBlock=initCommandBlock("loadWorkSpace",Win,loadWorkSpace),
        _NewWorkSpaceBlock=initCommandBlock("newWorkSpace",Win,newWorkSpace),
        WorkSpaceMenu=initCommandBlock("workSpaceMenu",Win,dummyRun).

clauses
    initWSTree_Menu(Win)=TreeMenu:-
        _NewGroupBlock=initCommandBlock("addGroup",Win,wsFE_SourceTree():addGroup),
        _NewSubGroupBlock=initCommandBlock("addSubGroup",Win,wsFE_SourceTree():addSubGroup),
        _AddFolderBlock=initCommandBlock("addFolder",Win,wsFE_SourceTree():addFolder),
        _AddSubFolderBlock=initCommandBlock("addSubFolder",Win,wsFE_SourceTree():addSubFolder),
        TreeMenu=initCommandBlock("treeMenu",Win,dummyRun).

clauses
    initCommandBlock("newWorkSpace",Win,Predicate)=block([  [cmd(newWorkSpaceCmd:id, imageAndText(vertical))]  ]):-
        !,
        newWorkSpaceCmd := command::new(Win,@"WorkSpace\New"),
        newWorkSpaceCmd:menuLabel:=ws_Events():getString(cmdNewWS_C),
        newWorkSpaceCmd:ribbonLabel:=string::concat(ws_Events():getString(cmdNewWS_C), " WS"),
        newWorkSpaceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipNewWS_C)),
        newWorkSpaceCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(newWSIcon16_C),bitmap::createFromBinary(newWSIcon32_C)])),
        newWorkSpaceCmd:run := Predicate,
        newWorkSpaceCmd:acceleratorKey := key(vpiDomains::k_N, vpiDomains::c_Control).
    initCommandBlock("loadWorkSpace",Win,Predicate)=block([  [cmd(loadWorkSpaceCmd:id, imageAndText(vertical))]  ]):-
        !,
        loadWorkSpaceCmd := command::new(Win,@"WorkSpace\Open"),
        loadWorkSpaceCmd:menuLabel:=ws_Events():getString(cmdOpenWS_C),
        loadWorkSpaceCmd:ribbonLabel:=string::concat(ws_Events():getString(cmdOpenWS_C), " WS"),
        loadWorkSpaceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipOpenWS_C)),
        loadWorkSpaceCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(openWSIcon16_C),bitmap::createFromBinary(openWSIcon32_C)])),
        loadWorkSpaceCmd:run := Predicate,
        loadWorkSpaceCmd:acceleratorKey := key(vpiDomains::k_L, vpiDomains::c_Control).

    initCommandBlock("workSpaceMenu",Win,_Predicate)=block([  [cmd(workSpaceMenuCmd:id, imageAndText(vertical))]  ]):-
        !,
        workSpaceMenuCmd:=menuCommand::new(Win,"workSpace"),
%        workSpaceMenuCmd:menuLabel:="workSpace",
%        workSpaceMenuCmd:tipTitle:=toolTip::tip("workSpace"),
        workSpaceMenuCmd:style :=menuCommand::toolMenu,
        workSpaceMenuCmd:layout:=menuCommand::menuStatic(
            [
            menuCommand::cmd(loadWorkSpaceCmd),
            menuCommand::cmd(newWorkSpaceCmd)
            ]),
        workSpaceMenuCmd:enabled := true.

    initCommandBlock("addGroup",Win,Predicate)=block([  [cmd(addGroupCmd:id, imageAndText(vertical))]  ]):-
        !,
        addGroupCmd := command::new(Win,"addGroup"),
        addGroupCmd:menuLabel:=ws_Events():getString(cmdAddGroup_C),
        addGroupCmd:ribbonLabel:=ws_Events():getString(cmdAddGroup_C),
        addGroupCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddGroup_C)),
        addGroupCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addGroupIcon16_C),bitmap::createFromBinary(addGroupIcon32_C)])),
        addGroupCmd:run := Predicate,
        addGroupCmd:enabled := true.
    initCommandBlock("addSubGroup",Win,Predicate)=block([  [cmd(addSubGroupCmd:id, imageAndText(vertical))]  ]):-
        !,
        addSubGroupCmd := command::new(Win,"addSubGroup"),
        addSubGroupCmd:menuLabel:=ws_Events():getString(cmdAddSubGroup_C),
        addSubGroupCmd:ribbonLabel:=ws_Events():getString(cmdAddSubGroup_C),
        addSubGroupCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSubGroup_C)),
        addSubGroupCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addSubGroupIcon16_C),bitmap::createFromBinary(addSubGroupIcon32_C)])),
        addSubGroupCmd:run := Predicate,
        addSubGroupCmd:enabled := false.

    initCommandBlock("addFolder",Win,Predicate)=block([  [cmd(addFolderCmd:id, imageAndText(vertical))]  ]):-
        !,
        addFolderCmd := command::new(Win,"addFolder"),
        addFolderCmd:menuLabel:=ws_Events():getString(cmdAddFolder_C),
        addFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddFolder_C),
        addFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddFolder_C)),
        addFolderCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addFolderIcon16_C),bitmap::createFromBinary(addFolderIcon32_C)])),
        addFolderCmd:run := Predicate,
        addFolderCmd:enabled := false.

    initCommandBlock("addSubFolder",Win,Predicate)=block([  [cmd(addSubFolderCmd:id, imageAndText(vertical))]  ]):-
        !,
        addSubFolderCmd := command::new(Win,"addSubFolder"),
        addSubFolderCmd:menuLabel:=ws_Events():getString(cmdAddSubFolder_C),
        addSubFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddSubFolder_C),
        addSubFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSubFolder_C)),
        addSubFolderCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addSubFolderIcon16_C),bitmap::createFromBinary(addSubFolderIcon32_C)])),
        addSubFolderCmd:run := Predicate,
        addSubFolderCmd:enabled := false.
    initCommandBlock("treeMenu",Win,_Predicate)=block([  [cmd(treeMenuCmd:id, imageAndText(vertical))]  ]):-
        !,
        treeMenuCmd:=menuCommand::new(Win,"treeMenu"),
%        treeMenuCmd:menuLabel:="treeMenu",
%        treeMenuCmd:tipTitle:=toolTip::tip("treeMenu"),
%        treeMenuCmd:tipBody:=toolTip::tip("Win kind of menu button toggles tools in and out. Whenever a new tool is selected, it becomes the default tool shown on menu button."),
        treeMenuCmd:style :=menuCommand::toolMenu,
        treeMenuCmd:layout:=menuCommand::menuStatic(
            [
            menuCommand::cmd(addGroupCmd),
            menuCommand::cmd(addSubGroupCmd),
            menuCommand::cmd(addFolderCmd),
            menuCommand::cmd(addSubFolderCmd)
            ]),
        treeMenuCmd:enabled := false.
    initCommandBlock(_Any,_Win,_Predicate)=_:-
        exception::raise_User("Non planned Alternative").

predicates
    onTreeNodeSelected:selectEndListener.
clauses
    onTreeNodeSelected(_TreeControl, _SlectionOld,_SlectionNew):-
        setMenuAbility().

predicates
    onListRowSelected: listViewControl::selectEndListener.
clauses
    onListRowSelected(_Source, _ItemId, _Select) :-
        wsFE_Tasks():getSourceLocalOptions(),
        setMenuAbility().

predicates
    setMenuAbility:().
clauses
    setMenuAbility():-
        if NodeSelected = wSFE_SourceTree():treeControl_P:tryGetSelected()
        then
            Selected = true,
            if string(folder_C)=NodeSelected:tryGetValue(nodeID_C)
            then
                AddSubEntity=false
            else
                AddSubEntity=true
            end if
        else
            Selected = false,
            AddSubEntity = false
        end if,
        if Selected=true, wsFE_Tasks():sourceIsRunning_P=true then
            workSpaceMenuCmd :enabled := false,
                newWorkSpaceCmd :enabled := false,
                loadWorkSpaceCmd :enabled := false,

            treeMenuCmd :enabled := false,
                addGroupCmd :enabled := false,
                addSubGroupCmd :enabled := false,
                addFolderCmd :enabled := false,
                addSubFolderCmd :enabled := false

        elseif wsFE_Tasks():sourceIsRunning_P=false then
            workSpaceMenuCmd :enabled := Selected,
                newWorkSpaceCmd :enabled := Selected,
                loadWorkSpaceCmd :enabled := Selected,

            treeMenuCmd :enabled := Selected,
                addGroupCmd :enabled := Selected,
                addSubGroupCmd :enabled := boolean::logicalAnd(Selected,AddSubEntity),
                addFolderCmd :enabled := Selected,
                addSubFolderCmd :enabled := boolean::logicalAnd(Selected,AddSubEntity)

        end if.

predicates
    dummyRun : (command).
clauses
    dummyRun(_).

predicates
    loadWorkSpace : (command).
clauses
    loadWorkSpace(_):-
        wsFE_Tasks():loadWorkSpace().

predicates
    newWorkSpace : (command).
clauses
    newWorkSpace(_):-
        wsFE_Tasks():newWorkSpace().

end implement workSpaceCommand
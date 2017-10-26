%

implement sourceContentCommand
    inherits wsFE_Connector
    open core, stdio, vpiDomains, treeControl{treeNode_std}, ws_eventManager, ribbonControl,wSFE_Command

facts
    addSourceCmd : command:=erroneous.
    addFromFolderCmd : command:=erroneous.
    editSourceCmd : command:=erroneous.

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        wsFE_SourceTree():treeControl_P:addSelectEndListener(onTreeNodeSelected),
        wsFE_SourceList():sourceList_P:addSelectEndListener(onListRowSelected).

clauses
    initWS_AddRemoveSource_Menu(WS_Form)=AddEntityBlockMenu:-
        _AddSourceBlock=initCommandBlock("addSource",WS_Form,addSource),
        _AddFolderBlock=initCommandBlock("addFromFolder",WS_Form,addFromFolder),
        _EditSourcerBlock=initCommandBlock("openSource",WS_Form,openSource),
        AddEntityBlockMenu=initCommandBlock("AddEntity",WS_Form,dummyRun),
        ws_Events():changeLanguageEvent:addListener(
            {:-
            addSourceCmd:ribbonLabel:=ws_Events():getString(cmdAddSrc_C),
            addSourceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSrc_C)),
            addFromFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddFromFolder_C),
            addFromFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddFromFolder_C)),
            editSourceCmd:ribbonLabel := ws_Events():getString(cmdOpenSrc_C),
            editSourceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipOpenSrc_C))
            }).

clauses
    initCommandBlock("addSource",Win,Predicate)=block([  [cmd(addSourceCmd:id, imageAndText(vertical))]  ]):-
        !,
        addSourceCmd := command::new(Win,"addSource"),
        addSourceCmd:menuLabel:=ws_Events():getString(cmdAddSrc_C),
        addSourceCmd:ribbonLabel:=ws_Events():getString(cmdAddSrc_C),
        addSourceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddSrc_C)),
        addSourceCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addSourceIcon16_C),bitmap::createFromBinary(addSourceIcon32_C)])),
        addSourceCmd:run := Predicate,
        addSourceCmd:enabled := false.

    initCommandBlock("addFromFolder",Win,Predicate)=block([  [cmd(addFromFolderCmd:id, imageAndText(vertical))]  ]):-
        !,
        addFromFolderCmd := command::new(Win,"addFromFolder"),
        addFromFolderCmd:menuLabel:=ws_Events():getString(cmdAddFromFolder_C),
        addFromFolderCmd:ribbonLabel:=ws_Events():getString(cmdAddFromFolder_C),
        addFromFolderCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipAddFromFolder_C)),
        addFromFolderCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(addSourcesFromFolderIcon16_C),bitmap::createFromBinary(addSourcesFromFolderIcon32_C)])),
        addFromFolderCmd:run := Predicate,
        addFromFolderCmd:enabled := false.
    initCommandBlock("openSource",Win,Predicate)=block([  [cmd(editSourceCmd:id, imageAndText(vertical))]  ]):-
        !,
        editSourceCmd := command::new(Win,"openSource"),
        editSourceCmd:menuLabel:=ws_Events():getString(cmdOpenSrc_C),
        editSourceCmd:ribbonLabel := ws_Events():getString(cmdOpenSrc_C),
        editSourceCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipOpenSrc_C)),
        editSourceCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(editIcon16_C),bitmap::createFromBinary(editIcon32_C)])),
        editSourceCmd:run :=Predicate,
        editSourceCmd:enabled := false.
    initCommandBlock("AddEntity",_Win,_Predicate)=block([
        [cmd(addSourceCmd:id, imageAndText(vertical))],
        [cmd(addFromFolderCmd:id, imageAndText(vertical))],
        [cmd(editSourceCmd:id, imageAndText(vertical))]
        ]):-
        !.
    initCommandBlock(_Any,_Win,_Predicate)=_:-
        exception::raise_User("Unexpected Alternative!").

predicates
    onTreeNodeSelected:selectEndListener.
clauses
    onTreeNodeSelected(_TreeControl, _SlectionOld,_SlectionNew):-
        setMenuAblity().

predicates
    onListRowSelected: listViewControl::selectEndListener.
clauses
    onListRowSelected(_Source, _ItemId, _Select) :-
        setMenuAblity().

predicates
    setMenuAblity:().
clauses
    setMenuAblity():-
        if NodeSelected = wSFE_SourceTree():treeControl_P:tryGetSelected()
        then
            TreeNodeSelected = true,
            if NodeSelected:tryGetValue(nodeID_C)=string(folder_C),!
            then
                IsGroup=false
            else
               IsGroup=true
            end if
        else
            TreeNodeSelected=false,
            IsGroup=false
        end if,
        ListSelected = toBoolean( [] <> wSFE_SourceList():sourceList_P:getSel() ),
        if wsFE_Tasks():sourceIsRunning_P=true then
                addSourceCmd:enabled := false,
                addFromFolderCmd:enabled := false,
                editSourceCmd:enabled := false
        else
                addSourceCmd:enabled := boolean::logicalAnd(boolean::logicalOr(TreeNodeSelected,ListSelected),IsGroup),
                addFromFolderCmd:enabled := boolean::logicalAnd(boolean::logicalOr(TreeNodeSelected,ListSelected),IsGroup),
                editSourceCmd:enabled := ListSelected
        end if.

predicates
    dummyRun : (command).
clauses
    dummyRun(_).

predicates
    addSource : (command).
clauses
    addSource(_) :-
        wsFE_Tasks():addSource().

predicates
    addFromFolder : (command).
clauses
    addFromFolder(_) :-
        wsFE_Tasks():addFromFolder().

predicates
    openSource : (command).
clauses
    openSource(_):-
        wsFE_Tasks():openSource().

end implement sourceContentCommand
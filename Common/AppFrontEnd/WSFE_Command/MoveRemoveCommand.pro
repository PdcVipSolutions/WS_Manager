%

implement moveRemoveCommand
    inherits wsFE_Connector
    open core, stdio, vpiDomains, treeControl{treeNode_std}, ribbonControl, ws_eventManager,wSFE_Command

facts - command_FB
    moveEntityUpCmd : command:=erroneous.
    moveEntityDownCmd : command:=erroneous.
    deleteEntityCmd : command:=erroneous.
    moveRemoveMenuCmd : menucommand:=erroneous.

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        wsFE_SourceTree():treeControl_P:addGetFocusListener(onSourceTreeIsActive),
        wsFE_SourceTree():treeControl_P:addSelectEndListener(onTreeNodeSelected),
        wsFE_SourceList():sourceList_P:addGetFocusListener(onSourceListIsActive),
        wsFE_SourceList():sourceList_P:addSelectEndListener(onListRowSelected).

%clauses
%    addChangeListener():-
%        ws_Events():changeLanguageEvent:addListener(
%            {:-
%            }).

clauses
    initMoveRemoveNode_Menu(Win)=MoveNodeMenu:-
        _NewGroupBlock=initCommandBlock("moveNodeUp",Win,dummyRun),
        _RemoveTreeEntityBlock=initCommandBlock("moveNodeDown",Win,dummyRun),
        _RemoveEntityBlock=initCommandBlock("deleteEntity",Win,dummyRun),
        MoveNodeMenu=initCommandBlock("moveNodeMenu",Win,dummyRun).

clauses
    initCommandBlock("deleteEntity",Win,Predicate)=block([  [cmd(deleteEntityCmd:id, imageAndText(vertical))]  ]):-
        !,
        deleteEntityCmd := command::new(Win,"DeleteEntity"),

        deleteEntityCmd:menuLabel:="Delete Source",
        deleteEntityCmd:ribbonLabel:="Delete Source",
        deleteEntityCmd:tipTitle:=toolTip::tip(""),

        deleteEntityCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(removeIcon16_C),bitmap::createFromBinary(removeIcon32_C)])),
        deleteEntityCmd:run := Predicate,
        deleteEntityCmd:enabled := false.

    initCommandBlock("moveNodeUp",Win,Predicate)=block([  [cmd(moveEntityUpCmd:id, imageAndText(vertical))]  ]):-
        !,
        moveEntityUpCmd := command::new(Win,@"MoveNodeUp"),

        moveEntityUpCmd:menuLabel:="Move Entity Up",
        moveEntityUpCmd:ribbonLabel:="Move Entity Up",
        moveEntityUpCmd:tipTitle:=toolTip::tip(""),

 %       moveEntityUpCmd:acceleratorKey := key(k_up, c_Alt),

        moveEntityUpCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(moveUpIcon16_C),bitmap::createFromBinary(moveUpIcon32_C)])),
        moveEntityUpCmd:run := Predicate,
        moveEntityUpCmd:enabled := false.
    initCommandBlock("moveNodeDown",Win,Predicate)=block([  [cmd(moveEntityDownCmd:id, imageAndText(vertical))]  ]):-
        !,
        moveEntityDownCmd := command::new(Win,@"MoveNodeDown"),
        moveEntityDownCmd:menuLabel:="Move Entity Down",
        moveEntityDownCmd:ribbonLabel:="Move Entity Down",
        moveEntityDownCmd:tipTitle:=toolTip::tip(""),
        moveEntityDownCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(moveDownIcon16_C),bitmap::createFromBinary(moveDownIcon32_C)])),
        moveEntityDownCmd:run := Predicate,
        moveEntityDownCmd:enabled := false.

    initCommandBlock("moveNodeMenu",Win,_Predicate)=
        block([
            [cmd(deleteEntityCmd:id, imageAndText(vertical))],
            [cmd(moveEntityUpCmd:id, imageAndText(vertical))],
            [cmd(moveEntityDownCmd:id, imageAndText(vertical))]
            ]):-
        !,
        moveRemoveMenuCmd:=menuCommand::new(Win,"moveNodeMenu"),
        moveRemoveMenuCmd:style :=menuCommand::toolMenu,
        moveRemoveMenuCmd:enabled := true.

    initCommandBlock(_Any,_Win,_Predicate)=_:-
        exception::raise_User("Non planned Alternative").

predicates
    dummyRun : (command).
clauses
    dummyRun(_).


predicates
    onSourceTreeIsActive:window::getFocusListener.
clauses
    onSourceTreeIsActive(_TreeControl):-
        deleteEntityCmd:run := wSFE_SourceTree():deleteTreeEntity,
        moveEntityUpCmd:run:=wSFE_SourceTree():moveNodeUp,
        moveEntityDownCmd:run:=wSFE_SourceTree():moveNodeDown,

        deleteEntityCmd:ribbonLabel := ws_Events():getString(cmdDeleteNode_C),
        moveEntityUpCmd:ribbonLabel:=ws_Events():getString(cmdMoveNodeUp_C),
        moveEntityDownCmd:ribbonLabel:=ws_Events():getString(cmdMoveNodeDown_C),

        deleteEntityCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipDeleteNode_C)),
        moveEntityUpCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipMoveNodeUp_C)),
        moveEntityDownCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipMoveNodeDown_C)),

        deleteEntityCmd:menuLabel := ws_Events():getString(cmdDeleteNode_C),
        moveEntityUpCmd:menuLabel:=ws_Events():getString(cmdMoveNodeUp_C),
        moveEntityDownCmd:menuLabel:=ws_Events():getString(cmdMoveNodeDown_C).

predicates
    onTreeNodeSelected:selectEndListener.
clauses
    onTreeNodeSelected(TreeControl, _SlectionOld,_SlectionNew):-
        if
            NodeSelected = TreeControl:tryGetSelected(),
            string(workSpace_C)=NodeSelected:tryGetValue(nodeID_C)
        then
            NotIsRoot=false
        else
            NotIsRoot=true
        end if,
        moveRemoveMenuCmd:enabled := NotIsRoot,
        moveEntityUpCmd :enabled :=NotIsRoot,
        moveEntityDownCmd :enabled :=NotIsRoot,
        deleteEntityCmd :enabled := NotIsRoot.

predicates
    onSourceListIsActive:window::getFocusListener.
clauses
    onSourceListIsActive(_Source):-
        deleteEntityCmd:run := removeSource,
        moveEntityUpCmd:run:=wSFE_SourceList():sourceUp,
        moveEntityDownCmd:run:=wSFE_SourceList():sourceDown,

        deleteEntityCmd:ribbonLabel := ws_Events():getString(cmdDeleteSrc_C),
        moveEntityUpCmd:ribbonLabel:=ws_Events():getString(cmdMoveSrcUp_C),
        moveEntityDownCmd:ribbonLabel:=ws_Events():getString(cmdMoveSrcDown_C),

        deleteEntityCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipDeleteSrc_C)),
        moveEntityUpCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipMoveSrcUp_C)),
        moveEntityDownCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipMoveSrcDown_C)),

        deleteEntityCmd:menuLabel := ws_Events():getString(cmdDeleteSrc_C),
        moveEntityUpCmd:menuLabel:=ws_Events():getString(cmdMoveSrcUp_C),
        moveEntityDownCmd:menuLabel:=ws_Events():getString(cmdMoveSrcDown_C).

predicates
    onListRowSelected: listViewControl::selectEndListener.
clauses
    onListRowSelected(_Source, _ItemId, _Select) :-
        if NodeSelected = wSFE_SourceTree():treeControl_P:tryGetSelected()
        then
%            if NodeSelected:tryGetValue(nodeID_C)=string(folder_C)
%            then
%                IsFolder=true
%            else
%               IsFolder=false
%            end if,
            if _Child=NodeSelected:getChild_nd(),!
            then
                NoChild=false
            else
               NoChild=true
            end if
        else
%            IsFolder=true,
            NoChild=true
        end if,
%        NotIsFolder=boolean::logicalNot(IsFolder),
        ListSelected = toBoolean( [] <> wSFE_SourceList():sourceList_P:getSel() ),
        if wsFE_Tasks():sourceIsRunning_P=true, ListSelected=true then
                moveEntityUpCmd:enabled := false,
                moveEntityDownCmd:enabled := false,
                deleteEntityCmd:enabled := false
        elseif wsFE_Tasks():sourceIsRunning_P=false then
                deleteEntityCmd:enabled := ListSelected,
                moveEntityUpCmd:enabled := boolean::logicalAnd(ListSelected,NoChild),
                moveEntityDownCmd:enabled := boolean::logicalAnd(ListSelected,NoChild)
        end if.

predicates
    removeSource : (command).
clauses
    removeSource(_) :-
        wsFE_Tasks():removeSource().

end implement moveRemoveCommand
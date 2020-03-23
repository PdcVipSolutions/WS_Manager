/*****************************************************************************
Copyright (c) Prolog Development Center SPB

******************************************************************************/
implement wsFE_Form
    inherits formWindow
    inherits formSplittedLayout{string}
    inherits wsFE_Connector
    open core, vpiDomains, ribbonControl, ws_eventManager, xmlNavigate, pfc\log, listViewControl

facts - options_FB
    selectNode_V:namedValue* := [].

facts
    wsFE_Command_P:wsFE_Command := erroneous.
    progress : core::predicate{integer,string} := {(_,_):-succeed}.

    localOptionsPanel_P : groupBox := erroneous.
    panelControl_P : listviewcontrol := erroneous.
    edit_ctl : editControl := erroneous.
    listButton_ctl : listButton := erroneous.
    editSource_V : listViewControl::itemId := listViewControl::itemId_zero.
    newSourceLocalOptions_V : namedValue_list := [].
    currentEditColumn_V : unsigned := 0.
    addModeListValue_V : string_list := [].
    columnList_V : listViewControl::column* := [].

    xml_ExtOptions : (string ExtName, string* ExtList, xmlDocument).
    filterMenuCommand : (string ExtName, checkCommand).

    localOptionsList_V : tuple{listViewControl::itemId,string,string,string,string}* := [].
    localExtOptionsList_F : string := "[]".
    prevSelectExt : string := "".

constants
    appEdit_id = 3.
    listButton_id = 4.
    edit_id = 5.

clauses
    display(Parent, FrontEnd) = Form :-
        Form = new(Parent, FrontEnd),
        Form:show().

clauses
    display(WS_FrontEnd):-
        wsFE_Command_P:=wsFE_Command::new(This,WS_FrontEnd),
        show().

clauses
    new(WS_FrontEnd):-
        wsFE_Connector::new(WS_FrontEnd),
        formWindow::new(This),
        formSplittedLayout{string}::new(This),
        generatedInitialize(),
        createStatusBar(),
        buildLayout(This,layout_C,notSet),
        setControls(),
        wsFE_Command_P:=wsFE_Command::new(This,WS_FrontEnd),
        setTitle("",false).

clauses
    new(Parent,WS_FrontEnd):-
        wsFE_Connector::new(WS_FrontEnd),
        formWindow::new(Parent),
        formSplittedLayout{string}::new(This),
        generatedInitialize(),
        createStatusBar(),
        buildLayout(This,layout_C,notSet),
        setControls(),
        wsFE_Command_P:=wsFE_Command::new(This,WS_FrontEnd),
        setTitle("",false).

clauses
    ribbonControl_P() = ribbonControl_ctl.

constants
    groupTree_C:string="R1C1".
    vertSplitter_C:string="VS1".
    projectList_C:string="R1C2".
    horizSplitter_C:string="HS1".
    contentRow_C:string="R1".
    messageControl_C:string="R2".
    panelHeight_C:integer=0.

constants
    layout_C:content_D*=
        [
        row(contentRow_C,250,formSplittedLayout::noBorder,control::dockTop,
            [
            column(groupTree_C,160,formSplittedLayout::noBorder,control::dockLeft,[]),
            splitter(vertSplitter_C,control::dockLeft),
            column(projectList_C,200,formSplittedLayout::noBorder,control::dockFill,[])
            ]),
        splitter(horizSplitter_C,control::dockTop),
        row(messageControl_C,150,formSplittedLayout::noBorder,control::dockfill,[])
        ].

clauses
    layoutControl_nd(groupTree_C,"treeControl",56).
    layoutControl_nd(projectList_C,"listViewControl",70).
    layoutControl_nd(messageControl_C,"messageControl",100).

clauses
    createControl(Container,"treeControl") = Control :-
        !,
        Control = treecontrol{treeNode_std}::new(Container),
        wsFE_P:entityRegistry_P:register(wsFE_SourceTree_C,wsFE_SourceTree::new(wsFE_P,Control)).
    createControl(Container,"messageControl")=Control:-
        !,
        Control = messageControl::new(Container),
        Control:dockStyle := control::dockfill,
        stdio::outputStream := Control:outputStream,
        succeed().
    createControl(Container,"listViewControl")= SourceListContainer :-
        SourceListContainer = containerControl::new(Container),
        SourceListContainer:dockStyle := control::dockFill,
        Control = listviewcontrol::new(SourceListContainer),
        wsFE_P:entityRegistry_P:register(wsFE_SourceList_C,wsFE_SourceList::new(wsFE_P,Control)),
        localOptionsPanel_P := groupBox::new(SourceListContainer),
        localOptionsPanel_P:setText(" Local Options "),
        localOptionsPanel_P:dockStyle := control::dockBottom,
        localOptionsPanel_P:setHeight(panelHeight_C),
        createLocalPanelControls(localOptionsPanel_P).
%        localOptionsList_V :=
%            [
%            tuple(cmdArgID_1, "1", "", "", argument_C),
%            tuple(cmdArgID_2, "2", "", "", argument_C),
%            tuple(cmdArgID_3, "3", "", "", argument_C),
%            tuple(cmdArgID_4, "4", "", "", argument_C)
%            ].

constants
    cmdArgID_1 : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,101).
    cmdArgID_2 : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,102).
    cmdArgID_3 : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,103).
    cmdArgID_4 : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,104).

clauses
    getSourceFilterList() =
            [
            menuCommand::cmd(ClearFilterCmd),menuCommand::separator|
            SourceTypeFilterList
            ] :-
        if filterMenuCommand("$All", ClearFilterCmd) then
        else
            ClearFilterCmd = checkCommand::new(This, "clearFilter"),
            ClearFilterCmd:menuLabel := ws_Events():getString(cmdClearFilter_C),
            ClearFilterCmd:ribbonLabel := ws_Events():getString(cmdClearFilter_C),
            ClearFilterCmd:tipTitle := toolTip::tip(ws_Events():getString(tipClearFilter_C)),
            ClearFilterCmd:checked := true,
            ClearFilterCmd:checkChangeEvent:addListener({:- showAll(ClearFilterCmd:checked)}),
            assert(filterMenuCommand("$All", ClearFilterCmd))
        end if,
        ws_Events():changeLanguageEvent:addListener({:- retractAll(filterMenuCommand("$All", _)) }),
        SourceTypeFilterList = getSourceTypeFilterList(ClearFilterCmd:checked).

predicates
    getSourceTypeFilterList : (boolean AllChecked) -> menuCommand::menuItem*.
clauses
    getSourceTypeFilterList(AllChecked) =
        [ menuCommand::cmd(ExtFilterCmd) ||
        xml_ExtOptions(ExtName, ExtList, _),
            if filterMenuCommand(ExtName, ExtFilterCmd) then
            else
                ExtFilterCmd = checkCommand::new(This, ExtName),
                ExtFilterCmd:menuLabel := ExtName,
                ExtFilterCmd:ribbonLabel := ExtName,
                ExtFilterCmd:tipTitle := toolTip::tip(ExtName),
                ExtFilterCmd:checked := false,
                ExtFilterCmd:enabled := boolean::logicalNot(AllChecked),
                ExtFilterCmd:checkChangeEvent:addListener({:- setFilter(ExtFilterCmd, ExtList)}),
                assert(filterMenuCommand(ExtName, ExtFilterCmd))
            end if
        ].

clauses
    setCheckedFilter(FilterList):-
        _ = getSourceFilterList(),
        foreach
            FilterName in FilterList,
            filterMenuCommand(FilterName, FilterCmd)
        do
            FilterCmd:checked := true
        end foreach,
        if
            not("$All" in FilterList),
            filterMenuCommand("$All", AllFilterCmd)
        then
            AllFilterCmd:checked := false
        end if.

predicates
    showAll : (boolean IsShowAll).
    setFilter : (checkCommand ExtFilterCmd,string* ExtList).
clauses
    showAll(IsShowAll):-
        foreach filterMenuCommand(ExtName, ExtFilterCmd), ExtName <> "$All" do
            ExtFilterCmd:enabled := boolean::logicalNot(IsShowAll)
        end foreach,
        wsFE_Tasks():showAll(IsShowAll).

    setFilter(ExtFilterCmd, ExtList):-
        wsFE_Tasks():setFilter(ExtFilterCmd, ExtList).

clauses
    setLocalExtOptionsList([namedValue(_SourceID, string(ExtOptionsStr))]):-
        editSource_V = listViewControl::itemId_zero,
        newSourceLocalOptions_V := [],
        !,
        localExtOptionsList_F := ExtOptionsStr,
        LocalExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        foreach tuple(ItemID, Index, CmdName, _Application, _ArgName) in localOptionsList_V do
            if
                tuple(_, AttributesList) = list::tryGetMemberEq({(N, tuple(N, _))}, Index, LocalExtOptionsList),
                AddModeA = toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, addModeA_C)),
                ArgValue = namedValue::tryGetNamed_string(AttributesList, argument_C),
                AddModeS = toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, addModeS_C)),
                SufValue = namedValue::tryGetNamed_string(AttributesList, suffix_C),
                AppStr = namedValue::getNamedDefault_string(AttributesList, fileName_C, "")
            then
            else
                AddModeA = true,
                ArgValue = "",
                AddModeS = true,
                SufValue = "",
                AppStr = ""
            end if,
            panelControl_P:updateItem(
                listViewControl::item(ItemID, Index, 0, [],
                    [
                    CmdName,
                    AppStr,
                    if AddModeA = true then ws_Events():getString(pmnAdd) else ws_Events():getString(pmnReplace) end if,
                    ArgValue,
                    if AddModeS = true then ws_Events():getString(pmnAdd) else ws_Events():getString(pmnReplace) end if,
                    SufValue,
                    ""])
                )
        end foreach,
        showSourceLocalOptions().
    setLocalExtOptionsList(ExtOptionsList):-
        newSourceLocalOptions_V := ExtOptionsList.

clauses
    setExtOptionsList(ExtOptionsList):-
        foreach namedValue(ExtName, string(ExtOptionsStr)) in ExtOptionsList do
            retractall(xml_ExtOptions(ExtName, _, _)),
            _Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
        end foreach,
        showSourceLocalOptions().

domains
    extOptions = tuple{string, namedValue*}*.
predicates
    extOptionsStr2xml_Options : (string ExtName,string ExtOptionsStr) -> xmlDocument Xml_Options.
clauses
    extOptionsStr2xml_Options(ExtName, ExtOptionsStr) = Xml_Options :-
        Xml_Options=xmlDocument::new(ExtName),
        Xml_Options:codePage_P:=utf8,
        Xml_Options:indent_P:=true,
        Xml_Options:xmlStandalone_P:=xmlLite::yes,
        ExtGroup = xmlElement::new("", groupNode_C,Xml_Options:root_P),
        ExtGroup:addAttribute(title_C, "ExtOptions"),
        Xml_Options:root_P:addNode(ExtGroup),
        ExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        foreach tuple(NodeName, AttributesList) in ExtOptionsList do
            CmdNode = xmlElement::new("", NodeName,ExtGroup),
            ExtGroup:addNode(CmdNode),
            foreach namedValue(AttrName, string(AttrValue)) in AttributesList do
                CmdNode:addAttribute(AttrName, AttrValue),
                if NodeName = "common" and AttrName = extName_C then
                    ExtList = splitExtListStr(string::trim(string::toLowerCase(AttrValue))),
                    assert(xml_ExtOptions(ExtName, ExtList, Xml_Options))
                end if
            end foreach
        end foreach,
        !.

class predicates
    splitExtListStr : (string ExtListStr) -> string* ExtList.
clauses
    splitExtListStr(ExtListStr) = [string::trim(Ext)|RestList] :-
        string::splitStringBySeparators(ExtListStr, ",;", Ext, _, RestStr),
        !,
        RestList = splitExtListStr(RestStr).
    splitExtListStr(ExtListStr) = Rest :-
        if "" = string::trim(ExtListStr) then
            Rest = []
        else
            Rest = [string::trim(ExtListStr)]
        end if.

predicates
    createLocalPanelControls : (groupBox GroupBox).
clauses
    createLocalPanelControls(GroupBox):-
        edit_ctl := editControl::new(GroupBox),
        edit_ctl:setText(""),
        edit_ctl:setPosition(0, 0),
        edit_ctl:setVisible(false),
        edit_ctl:setBorder(false),
        edit_ctl:setKeyDownResponder(onEditKeyDown),
        edit_ctl:addNativeMessageHandler(onNative),
        edit_ctl:addLoseFocusListener(onEditLoseFocus),
        edit_ctl:setCtrlId(edit_id),
        listButton_ctl := listButton::new(GroupBox),
        listButton_ctl:setPosition(0, 0),
        listButton_ctl:setMaxDropDownRows(5),
        listButton_ctl:setSort(false),
        listButton_ctl:setVisible(false),
        listButton_ctl:setKeyDownResponder(onEditKeyDown),
        listButton_ctl:addNativeMessageHandler(onNative),
        listButton_ctl:addLoseFocusListener(onEditLoseFocus),
        listButton_ctl:setCtrlId(listButton_id),
        panelControl_P := listviewcontrol::new(GroupBox),
        panelControl_P:setPosition(0, 0),
        panelControl_P:setSize(50, 50),
        panelControl_P:dockStyle := control::dockFill,
        columnList_V :=
                [
            listViewControl::column("#", 30, alignleft),
            listViewControl::column(ws_Events():getString(colOpName), 120, alignleft),
            listViewControl::column(ws_Events():getString(txtApplicationFile), 120, alignleft),
            listViewControl::column(ws_Events():getString(colAddMode), 80, alignleft),
            listViewControl::column(ws_Events():getString(colCmdArgument), 150, alignleft),
            listViewControl::column(ws_Events():getString(colAddMode), 80, alignleft),
            listViewControl::column(ws_Events():getString(colLocalSuffix), 150, alignleft),
            listViewControl::column(ws_Events():getString(colResultStr), 600, alignleft)
                ],
        panelControl_P:insertColumnList(1, columnList_V),
        ws_Events():changeLanguageEvent:addListener(
            {:-
             if panelControl_P:isShown then
                column(_, W2, A2) = panelControl_P:getColumn(2),
                panelControl_P:setColumn(2, column(ws_Events():getString(colOpName), W2, A2)),
                column(_, W3, A3) = panelControl_P:getColumn(3),
                panelControl_P:setColumn(3, column(ws_Events():getString(txtApplicationFile), W3, A3)),
                column(_, W4, A4) = panelControl_P:getColumn(4),
                panelControl_P:setColumn(4, column(ws_Events():getString(colAddMode), W4, A4)),
                column(_, W5, A5) = panelControl_P:getColumn(5),
                panelControl_P:setColumn(5, column(ws_Events():getString(colCmdArgument), W5, A5)),
                column(_, W6, A6) = panelControl_P:getColumn(6),
                panelControl_P:setColumn(6, column(ws_Events():getString(txtApplicationFile), W6, A6)),
                column(_, W7, A7) = panelControl_P:getColumn(7),
                panelControl_P:setColumn(7, column(ws_Events():getString(colAddMode), W7, A7)),
                column(_, W8, A8) = panelControl_P:getColumn(8),
                panelControl_P:setColumn(8, column(ws_Events():getString(colCmdArgument), W8, A8))
            end if
            }),
        panelControl_P:setLvType(listViewControl::lvs_report),
        panelControl_P:setLvExStyle(listViewControl::lvs_ex_labeltip + listViewControl::lvs_ex_gridLines),
        panelControl_P:setStyle([listViewControl::lvs_showSelAlways, listViewControl::lvs_autoArrange, listViewControl::lvs_singleSel]),
        localOptionsList_V :=
            [
            tuple(cmdArgID_1, "1", "", "", argument_C),
            tuple(cmdArgID_2, "2", "", "", argument_C),
            tuple(cmdArgID_3, "3", "", "", argument_C),
            tuple(cmdArgID_4, "4", "", "", argument_C)
            ],
        ListViewItems =
            [ listViewControl::item(ItemID, Name, 0, [], [CmdName, "", ws_Events():getString(pmnAdd), "", ws_Events():getString(pmnAdd), "", ""])
            ||
                tuple(ItemID, Name, CmdName, _, _) = list::getMember_nd(localOptionsList_V)
            ],
        panelControl_P:insertItemList(ListViewItems),
        addModeListValue_V := [ws_Events():getString(pmnAdd), ws_Events():getString(pmnReplace)],
        listButton_ctl:addList(addModeListValue_V),
        panelControl_P:addMouseClickListener(onListViewControlMouseClick).

clauses
    showSourceLocalOptions():-
        FileName = wSFE_SourceList():tryGetSelectSourceFileName(),
        xml_ExtOptions(_ExtName, ExtList, Xml_Options),
        list::isMemberEq(string::equalIgnoreCase, filename::getExtension(FileName), ExtList),
        !,
        SelectExt = filename::getExtension(FileName),
        if prevSelectExt <> SelectExt then
            updateCommandRibbon(Xml_Options)
        end if,
        prevSelectExt := SelectExt,
        foreach tuple(ItemID, Index, _CmdName, _Application, ArgName) in localOptionsList_V do
            setLocalOptionsRow(ItemID, Xml_Options, Index, ArgName)
        end foreach.
    showSourceLocalOptions().

clauses
    tryGetStatusCheck(Index, FileName, AllPossible) = StatusCheck :-
        xml_ExtOptions(_ExtName, ExtList, Xml_Options),
        list::isMemberEq(string::equalIgnoreCase, filename::getExtension(FileName), ExtList),
        Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, { (CN) :- toString(Index) = CN:attribute(index_C) })]),
        !,
        StatusCheck = toBoolean("true" = Node:attribute(checkStatus_C)),
        AllPossible = toBoolean("true" = Node:attribute(allPossible_C)).

predicates
    updateCommandRibbon : (xmlDocument Xml_Options).
clauses
    updateCommandRibbon(Xml_Options):-
        foreach Index = std::cIterate(commandMax)+1 do
            if Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, { (CN) :- toString(Index) = CN:attribute(index_C) })]),
                CmdName = Node:attribute(name_C)
            then
                wsFE_Command_P:updateCommandRibbon(Index, CmdName, true)
            else
                wsFE_Command_P:updateCommandRibbon(Index, "not assigned", false)
            end if
        end foreach,
        ribbonControl_ctl:invalidate().

predicates
    setLocalOptionsRow: (listViewControl::itemId ItemID,xmlDocument Xml_Options,string Index,string ArgName).
clauses
    setLocalOptionsRow(ItemID, Xml_Options, Index, _ArgName):-
        Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, { (CN) :- Index = CN:attribute(index_C) })]),
        if CmdName = Node:attribute(name_C) then
            panelControl_P:updateTextSubItem(ItemID, 0, CmdName)
        end if,
        FC = Node:attribute(formatCmd_C),
        !,
        ApplicationStr = getApplicationStr(ItemID, Node),
        FC1 = string::replaceAll(FC, "[Application]", ApplicationStr),
        SecondArg = getArgumentStr(ItemID, Node),
        FC2 = string::replaceAll(FC1, "[Arguments]", SecondArg),
        SuffixArg = getSuffixStr(ItemID, Node),
        ResultString = string::replaceAll(FC2, "[Suffix]", SuffixArg),
        panelControl_P:updateTextSubItem(ItemID, 6, ResultString).
    setLocalOptionsRow(_ItemID, _Xml_Options, _CmdName, _ArgName).

predicates
    getApplicationStr : (listViewControl::itemId ItemID,xmlElement Node) -> string ApplicationStr.
clauses
    getApplicationStr(ItemID, Node) = ApplicationStr :-
        listViewControl::item(ItemID, _, _, _, [_, AppStr, _, _, _, _,  _]) == panelControl_P:getItem(ItemID),
        ApplicationStr = if AppStr <> "" then AppStr elseif FN = Node:attribute(fileName_C) then FN else "" end if.

predicates
    getArgumentStr : (listViewControl::itemId ItemID,xmlElement Node) -> string ArgumentStr.
clauses
    getArgumentStr(ItemID, Node) = ArgumentStr :-
        listViewControl::item(ItemID, _, _, _, [_, _, AddModeA, LocalOptions, _, _,  _]) == panelControl_P:getItem(ItemID),
        if AddModeA = ws_Events():getString(pmnAdd) then
            ArgumentStr = if SN = Node:attribute(argument_C) then string::concat(SN," ",LocalOptions) else LocalOptions end if
        else
            ArgumentStr = LocalOptions
        end if.

predicates
    getSuffixStr : (listViewControl::itemId ItemID,xmlElement Node) -> string SuffixStr.
clauses
    getSuffixStr(ItemID, Node) = SuffixStr :-
        listViewControl::item(ItemID, _, _, _, [_, _, _, _, AddModeS, LocalSuffix, _]) == panelControl_P:getItem(ItemID),
        if AddModeS = ws_Events():getString(pmnAdd) then
            SuffixStr = if SN = Node:attribute(suffix_C) then string::concat(SN," ",LocalSuffix) else LocalSuffix end if
        else
            SuffixStr = LocalSuffix
        end if.

predicates
    onListViewControlMouseClick : listViewControl::mouseClickListener.
clauses
    onListViewControlMouseClick(Source, PNT):-
        ItemID = list::getMember_nd(panelControl_P:getAll()),
        RCT = Source:getItemRect(ItemID, listViewControl::bounds),
        listViewControl::column(_,  Width1, _) = Source:getColumn(1),
        listViewControl::column(_,  Width2, _) = Source:getColumn(2),
        listViewControl::column(_,  Width3, _) = Source:getColumn(3),
        listViewControl::column(_,  Width4, _) = Source:getColumn(4),
        listViewControl::column(_,  Width5, _) = Source:getColumn(5),
        listViewControl::column(_,  Width6, _) = Source:getColumn(6),
        listViewControl::column(_,  Width7, _) = Source:getColumn(7),
        RCT = rct(L,T,_,B),
        RCTColumn3 = rct(L+Width1+Width2,T,L+Width1+Width2+Width3,B),
        RCTColumn4 = rct(L+Width1+Width2+Width3,T,L+Width1+Width2+Width3+Width4,B),
        RCTColumn5 = rct(L+Width1+Width2+Width3+Width4,T,L+Width1+Width2+Width3+Width4+Width5,B),
        RCTColumn6 = rct(L+Width1+Width2+Width3+Width4+Width5,T,L+Width1+Width2+Width3+Width4+Width5+Width6,B),
        RCTColumn7 = rct(L+Width1+Width2+Width3+Width4+Width5+Width6,T,L+Width1+Width2+Width3+Width4+Width5+Width6+Width7,B),
        if gui::rectPntInside(RCTColumn3, PNT) then
            ColNumber = appEdit_id,
            EditRCT = RCTColumn3,
            EditCtrl = convert(control,edit_ctl)
        elseif gui::rectPntInside(RCTColumn4, PNT) then
            ColNumber = listbutton_id,
            EditRCT = RCTColumn4,
            EditCtrl = convert(control,listbutton_ctl)
        elseif gui::rectPntInside(RCTColumn5, PNT) then
            ColNumber = edit_id,
            EditRCT = RCTColumn5,
            EditCtrl = convert(control,edit_ctl)
        elseif gui::rectPntInside(RCTColumn6, PNT) then
            ColNumber = listbutton_id + 2,
            EditRCT = RCTColumn6,
            EditCtrl = convert(control,listbutton_ctl)
        elseif gui::rectPntInside(RCTColumn7, PNT) then
            ColNumber = edit_id + 2,
            EditRCT = RCTColumn7,
            EditCtrl = convert(control,edit_ctl)
        else
            fail
        end if,
        Source:tryGetItemIndex(ItemId) = ItemIndex,
        Source:getItemColumnText(ItemIndex, ColNumber) = Text,
        Source:select([ItemId], true),
        EditCtrl:setOuterRect(EditRCT),
        if ColNumber in [appEdit_id, edit_id, edit_id + 2] then
            edit_ctl:setText(Text)
        elseif ColNumber in [listbutton_id, listbutton_id + 2] then
            listbutton_ctl:selectAt(list::tryGetIndex(Text, addModeListValue_V), true)
        end if,
        [SourceID] = wSFE_SourceList():sourceList_P:getSel(),
        !,
        editSource_V := SourceID,
        currentEditColumn_V := ColNumber,
        EditCtrl:setVisible(true),
        EditCtrl:bringToTop(),
        EditCtrl:setFocus().
    onListViewControlMouseClick(_Source, _PNT).

class predicates
    dlgc_translate : (gui_native::lResult) -> gui_native::lResult.
clauses
    dlgc_translate(Code) = gui_api::mkR(bit::bitOr(gui_api::getUnsigned(Code), gui_native::dlgc_wantallkeys)).

class predicates
    onNative : window::nativeMessageHandler.
clauses
    onNative(Source, gui_native::wm_getdlgcode, WParam, LParam) = window::nativeResult(dlgc_translate(Ret)) :-
        !,
        Ret = Source:sendEvent(vpiDomains::e_Native(gui_native::wm_getdlgcode, WParam, LParam), false).
    onNative(_, _, _, _) = window::defaultNativeHandling.

predicates
    onEditKeyDown : window::keyDownResponder.
clauses
    onEditKeyDown(Source, Key, _ShiftControlAlt) = window::keyDownHandled :-
        Key in [vpiDomains::k_esc,vpiDomains::k_enter],
        Source:setVisible(false),
        if Key = vpiDomains::k_enter then
            [ItemID] = panelControl_P:getSel(),
            panelControl_P:updateTextSubItem(ItemID, currentEditColumn_V - 2, Source:getText()),
            updateLocalOptions(),
            if newSourceLocalOptions_V <> [] then
                setLocalExtOptionsList(newSourceLocalOptions_V)
            else
                showSourceLocalOptions(),
                panelControl_P:setFocus()
            end if
        end if,
        !.
    onEditKeyDown(_Source, _Key, _ShiftControlAlt) = window::defaultKeyDownHandling.

predicates
    onEditLoseFocus : window::loseFocusListener.
clauses
    onEditLoseFocus(Source):-
        if Source:getVisible() = true then
            try
                _ = onEditKeyDown(Source, vpiDomains::k_enter, vpiDomains::c_Nothing)
            catch _ do
                succeed
            end try
        end if.

predicates
    updateLocalOptions : ().
clauses
    updateLocalOptions():-
        LocalOptionsList = [tuple(Index,
                [namedValue(addModeA_C, string(toString(AddModeA))), namedValue(argument_C, string(Arguments)),
                 namedValue(addModeS_C, string(toString(AddModeS))), namedValue(suffix_C, string(Suffix)),
                 namedValue(fileName_C, string(AppStr))]) ||
            tuple(ItemID, Index, _CmdName, _Application, _ArgName) in localOptionsList_V,
            panelControl_P:tryGetItemIndex(ItemId) = ItemIndex,
            AppStr = panelControl_P:getItemColumnText(ItemIndex, appEdit_id),
            AddModeA = toBoolean(ws_Events():getString(pmnAdd) = panelControl_P:getItemColumnText(ItemIndex, listButton_id)),
            Arguments = panelControl_P:getItemColumnText(ItemIndex, edit_id),
            AddModeS = toBoolean(ws_Events():getString(pmnAdd) = panelControl_P:getItemColumnText(ItemIndex, listButton_id+2)),
            Suffix = panelControl_P:getItemColumnText(ItemIndex, edit_id+2)
            ],
        wsFE_Tasks():saveLocalOptions([namedValue(toString(editSource_V), string(toString(LocalOptionsList)))]),
        editSource_V := listViewControl::itemId_zero.

clauses
    showLocalOptionsDialog():-
        wsFE_Tasks():showLocalOptions().

    showLocalOptionsDialog(VirtualDirList):-
        FileName = wSFE_SourceList():tryGetSelectSourceFileName(),
        xml_ExtOptions(_ExtName, ExtList, Xml_Options),
        list::isMemberEq(string::equalIgnoreCase, filename::getExtension(FileName), ExtList),
        !,
        _ = localOptions::display(This, wsFE_P, localExtOptionsList_F, FileName, Xml_Options, VirtualDirList).
    showLocalOptionsDialog(_).

    showLocalOptionsPanel(IsShown):-
        if IsShown = true then
            localOptionsPanel_P:setHeight(panelHeight_C)
        else
            localOptionsPanel_P:setHeight(0)
        end if.

clauses
    setTitle(WS_FileName,_TrueIfReadOnly):-
        "wsm" <> filename::getExtension(WS_FileName),
        !,
        setText(string::concat("WorkSpaceManager - ",ws_Events():getString(txtNewWS))).
    setTitle(WS_FileName,TrueIfReadOnly):-
        fileName::getPathAndName(WS_FileName, WS_Path, WS_Name),
        if TrueIfReadOnly=true then
                ReadOnly = ws_Events():getString(txtReadOnly)
        else
                ReadOnly = ""
        end if,
        WS_TitleStr = string::format("WorkSpaceManager - %s (%s) %s ", WS_Name, WS_Path, ReadOnly),
        setText(WS_TitleStr).

clauses
    onSelectedProject().

facts
    statusBarControl : statusBarControl := erroneous.
    progressBar : progressBarControl := erroneous.

predicates
    createStatusBar : ().
clauses
    createStatusBar() :-
        statusBarControl := statusBarControl::new(This),
        Cell1 = statusBarCell::new(statusBarControl, 300),
        CellPB = statusBarCell::new(statusBarControl, 200),
        Cell2 = statusBarCell::new(statusBarControl, 300),
        Cell3 = statusBarCell::new(statusBarControl, 200),
        statusBarControl:cells := [Cell1, CellPB, Cell2, Cell3],
        setStatusBar().

clauses
    setStatusBar():-
        progress := statusBar_set.

predicates
    statusBar_set : (integer NumberCell,string Text).
clauses
    statusBar_set(NumberCell, Text) :-
        if Cell = list::tryGetNth(NumberCell-1, statusBarControl:cells) then
            Cell:text := Text
        end if.

clauses
    progressBar_activate(Count) :-
        progressBar := progressBarControl::new(statusBarControl),
        if Count <= 1 then
            progressBar:marquee_style := true
        else
            progressBar:setRange(0, Count)
        end if,
        progressBar:show(),
        progressBar:useTaskBarList := true,
        if Count <= 1 then
            progressBar:autoMarqueeMode := true,
            progressBar:autoMarqueeMs := 100
        end if,
        [_, CellPB, _, _] == statusBarControl:cells,
        CellPB:control := some(progressBar).

clauses
    progressBar_progress(Count) :-
        if not(isErroneous(progressBar)) then
            progressBar:progress := Count
        end if.

clauses
    progressBar_remove() :-
        [_, CellPB, _, _] == statusBarControl:cells,
        CellPB:control := core::none(),
        progressBar := erroneous.

predicates
    onClose : frameDecoration::closeResponder.
clauses
    onClose(_Source) = frameDecoration::acceptClose :-
        wsFE_Tasks():trySaveNewWorkSpace(),
        !,
        if tuple(NodePath,_) == wsFE_SourceTree():tryGetSelectedNodePath() then
            selectNode_V := NodePath
        end if,
        wsFE_Tasks():save_OptionsFE(),
        forgetObjects(),
        ws_Events():appEvent_P:notify(0),
        succeed().
    onClose(_Source) = frameDecoration::denyClose.

predicates
    forgetObjects:().
clauses
    forgetObjects():-
        wsFE_Command_P := erroneous,
        progress := erroneous,
        localOptionsPanel_P := erroneous,
        panelControl_P := erroneous,
        edit_ctl := erroneous,
        listButton_ctl := erroneous,
        editSource_V := listViewControl::itemId_zero,
        newSourceLocalOptions_V := erroneous,
        addModeListValue_V := erroneous,
        columnList_V := erroneous,
        localOptionsList_V := erroneous,
        ribbonControl_ctl := erroneous,
        statusBarControl := erroneous,
        progressBar := erroneous,
        selectNode_V := erroneous,
        succeed().

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data):-
        _ = wsFE_SourceList():sourceList_P:sendNative(listViewControl::lvm_setextendedlistviewstyle,
                gui_api::mkW(listViewControl::lvs_ex_labelTip), gui_api::mkL(listViewControl::lvs_ex_labelTip)).


% This code is maintained automatically, do not update it manually.
%  12:26:08-19.10.2018

facts
    ribbonControl_ctl : ribboncontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("WorkSpaceForm"),
        setRect(rct(100, 100, 800, 500)),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings, wsf_ClipChildren]),
        menuSet(noMenu),
        addShowListener(onShow),
        setCloseResponder(onClose),
        ribbonControl_ctl := ribboncontrol::new(This),
        ribbonControl_ctl:setPosition(12, 4),
        ribbonControl_ctl:setSize(232, 16),
        ribbonControl_ctl:dockStyle := control::dockTop.
% end of automatic code
end implement wsFE_Form
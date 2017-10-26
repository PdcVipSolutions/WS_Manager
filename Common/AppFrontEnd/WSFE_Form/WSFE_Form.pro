/*****************************************************************************
Copyright (c) Prolog Development Center SPB

******************************************************************************/
implement wsFE_Form
    inherits formWindow
    inherits formSplittedLayout{string}
    inherits wsFE_Connector
    open core, vpiDomains, ribbonControl,treeModel_std, ws_eventManager, xmlNavigate, pfc\log

facts - options_FB
    selectNode_V:namedValue* := [].

facts
    wsFE_Command_P:wsFE_Command:=erroneous.
    progress : core::predicate{integer,string} := {(_,_):-succeed}.

    localOptionsPanel_P :groupBox :=erroneous.
    panelControl_ctl : listviewcontrol :=erroneous.
    edit_ctl : editControl :=erroneous.
    listButton_ctl : listButton :=erroneous.
    feListButton_ctl : listButton :=erroneous.
    editSource_V : listViewControl::itemId := listViewControl::itemId_zero.
    newSourceLocalOptions_V : namedValue_list := [].

    xml_ExtOptions : (string ExtName, string* ExtList, xmlDocument).
    filterMenuCommand : (string ExtName, checkCommand).
    feModeByType : (string CmdName,string ExtName, boolean Value).

    addModeListValue_V : string_list := [].
    feModeListValue_V : string_list := [].
    columnList_V : listViewControl::column* := [].
    localOptionsList_V : tuple{listViewControl::itemId,string,string,string}* := [].

constants
    edit_id = 2.
    listButton_id = 3.
    feListButton_id = 4.

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

constants
    groupTree_C:string="R1C1".
    vertSplitter_C:string="VS1".
    projectList_C:string="R1C2".
    horizSplitter_C:string="HS1".
    contentRow_C:string="R1".
    messageControl_C:string="R2".
    panelHeight_C:integer=90.

constants
    layout_C:content_D*=
        [
        row(contentRow_C,300,formSplittedLayout::border,control::dockTop,
            [
            column(groupTree_C,160,formSplittedLayout::border,control::dockLeft,[]),
            splitter(vertSplitter_C,control::dockLeft),
            column(projectList_C,200,formSplittedLayout::border,control::dockFill,[])
            ]),
        splitter(horizSplitter_C,control::dockTop),
        row(messageControl_C,50,formSplittedLayout::border,control::dockfill,[])
        ].

clauses
    layoutControl_nd(groupTree_C,"treeControl",56).
    layoutControl_nd(projectList_C,"listViewControl",70).
    layoutControl_nd(messageControl_C,"messageControl",70).

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

constants
    openItemID : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,100).
    runItemID : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,101).
    rerunItemID : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,102).
    suffixItemID : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,103).
    execItemID : listViewControl::itemId = uncheckedConvert(listViewControl::itemId,104).

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
        LocalExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        foreach tuple(ItemID, Name, CmdName, ArgName) in localOptionsList_V do
            if
                tuple(_, AttributesList) = list::tryGetMemberEq({(N, tuple(N, _))}, ArgName, LocalExtOptionsList),
                Value = namedValue::tryGetNamed_string(AttributesList, value_C),
                AddMode = toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, addMode_C)),
                FeMode = toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, feMode_C))
            then
            else
                Value = "",
                AddMode = true,
                if
                    Ext = filename::getExtension(wSFE_SourceList():tryGetSelectSourceFileName()),
                    feModeByType(CmdName, ExtName, FEValue),
                    xml_ExtOptions(ExtName, ExtList, _),
                    %Ext in ExtList
                    list::isMemberEq(string::equalIgnoreCase, Ext, ExtList)
                then
                    FeMode = FEValue
                else
                    FeMode = false
                end if
            end if,
            panelControl_ctl:updateItem(
                listViewControl::item(ItemID, Name, 0, [],
                    [Value,
                    if AddMode = true then ws_Events():getString(pmnAdd) else ws_Events():getString(pmnReplace) end if,
                    if FeMode = true then ws_Events():getString(pmnFrontEnd) else ws_Events():getString(pmnBackEnd) end if,
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
            retractall(feModeByType(_, ExtName, _)),
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
%        ExtGroup:name_P := groupNode_C,
        ExtGroup:addAttribute(title_C, "ExtOptions"),
        Xml_Options:root_P:addNode(ExtGroup),
        ExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        foreach tuple(NodeName, AttributesList) in ExtOptionsList do
            CmdNode = xmlElement::new("", NodeName,ExtGroup),
%            CmdNode:name_P := NodeName,
            ExtGroup:addNode(CmdNode),
            foreach namedValue(AttrName, string(AttrValue)) in AttributesList do
                CmdNode:addAttribute(AttrName, AttrValue),
                if NodeName = "common" and AttrName = extName_C then
                    ExtList = splitExtListStr(string::trim(string::toLowerCase(AttrValue))),
                    assert(xml_ExtOptions(ExtName, ExtList, Xml_Options))
                end if,
                if AttrName = feMode_C then
                    assert(feModeByType(NodeName, ExtName, toBoolean("true" = CmdNode:attribute(feMode_C))))
                end if
            end foreach
        end foreach.

predicates
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

        feListButton_ctl := listButton::new(GroupBox),
        feListButton_ctl:setPosition(0, 0),
        feListButton_ctl:setMaxDropDownRows(5),
        feListButton_ctl:setSort(false),
        feListButton_ctl:setVisible(false),
        feListButton_ctl:setKeyDownResponder(onEditKeyDown),
        feListButton_ctl:addNativeMessageHandler(onNative),
        feListButton_ctl:addLoseFocusListener(onEditLoseFocus),
        feListButton_ctl:setCtrlId(feListButton_id),

        panelControl_ctl := listviewcontrol::new(GroupBox),
        panelControl_ctl:setPosition(4, 4),
        panelControl_ctl:setSize(50, 50),
        panelControl_ctl:dockStyle := control::dockFill,
        columnList_V :=
            [
            listViewControl::column(ws_Events():getString(colCmdArgument), 120, alignleft),
            listViewControl::column(ws_Events():getString(colLocalValue), 150, alignleft),
            listViewControl::column(ws_Events():getString(colAddMode), 80, alignleft),
            listViewControl::column(ws_Events():getString(colWhereMode), 80, alignleft),
            listViewControl::column(ws_Events():getString(colResultStr), 600, alignleft)
            ],
        panelControl_ctl:insertColumnList(1, columnList_V),
        panelControl_ctl:setLvType(listViewControl::lvs_report),
        panelControl_ctl:setLvExStyle(listViewControl::lvs_ex_labeltip),
        panelControl_ctl:setStyle([listViewControl::lvs_showSelAlways, listViewControl::lvs_autoArrange, listViewControl::lvs_singleSel]),
        localOptionsList_V :=
            [
            tuple(openItemID, ws_Events():getString(rowOpen), sourceEditor_C, argEditor_C),
            tuple(runItemID, ws_Events():getString(rowRunMode), sourcePerformer_C, argPerformer_C),
            tuple(rerunItemID, ws_Events():getString(rowReRunMode), sourcePerformer_C, reargPerformer_C),
            tuple(suffixItemID, ws_Events():getString(rowSuffix), sourcePerformer_C, suffixPerformer_C),
            tuple(execItemID, ws_Events():getString(rowExecute), sourceExecute_C, argExecute_C)
            ],
        addModeListValue_V := [ws_Events():getString(pmnAdd), ws_Events():getString(pmnReplace)],
        listButton_ctl:addList(addModeListValue_V),
        feModeListValue_V := [ws_Events():getString(pmnBackEnd), ws_Events():getString(pmnFrontEnd)],
        feListButton_ctl:addList(feModeListValue_V),
        ws_Events():changeLanguageEvent:addListener(
            {:-
            listViewControl::column(_, W1, A1) = panelControl_ctl:getColumn(1),
            panelControl_ctl:setColumn(1, listViewControl::column(ws_Events():getString(colCmdArgument), W1, A1)),
            listViewControl::column(_, W2, A2) = panelControl_ctl:getColumn(2),
            panelControl_ctl:setColumn(2, listViewControl::column(ws_Events():getString(colLocalValue), W2, A2)),
            listViewControl::column(_, W3, A3) = panelControl_ctl:getColumn(3),
            panelControl_ctl:setColumn(3, listViewControl::column(ws_Events():getString(colAddMode), W3, A3)),
            listViewControl::column(_, W4, A4) = panelControl_ctl:getColumn(4),
            panelControl_ctl:setColumn(4, listViewControl::column(ws_Events():getString(colWhereMode), W4, A4)),
            listViewControl::column(_, W5, A5) = panelControl_ctl:getColumn(5),
            panelControl_ctl:setColumn(5, listViewControl::column(ws_Events():getString(colResultStr), W5, A5)),
            addModeListValue_V := [ws_Events():getString(pmnAdd), ws_Events():getString(pmnReplace)],
            listButton_ctl:clearAll(),
            listButton_ctl:addList(addModeListValue_V),
            feModeListValue_V := [ws_Events():getString(pmnBackEnd), ws_Events():getString(pmnFrontEnd)],
            feListButton_ctl:clearAll(),
            feListButton_ctl:addList(feModeListValue_V),
            localOptionsList_V :=
                [
                tuple(openItemID, ws_Events():getString(rowOpen), sourceEditor_C, argEditor_C),
                tuple(runItemID, ws_Events():getString(rowRunMode), sourcePerformer_C, argPerformer_C),
                tuple(rerunItemID, ws_Events():getString(rowReRunMode), sourcePerformer_C, reargPerformer_C),
                tuple(suffixItemID, ws_Events():getString(rowSuffix), sourcePerformer_C, suffixPerformer_C),
                tuple(execItemID, ws_Events():getString(rowExecute), sourceExecute_C, argExecute_C)
                ]
            }),
        ListViewItems =
            [ listViewControl::item(ItemID, Name, 0, [], ["", ws_Events():getString(pmnAdd), ws_Events():getString(pmnBackEnd), ""])
            ||
                tuple(ItemID, Name, _CmdName, _) = list::getMember_nd(localOptionsList_V)
            ],
       panelControl_ctl:insertItemList(ListViewItems),
       panelControl_ctl:addMouseClickListener(onListViewControlMouseClick).

clauses
    showSourceLocalOptions():-
        FileName = wSFE_SourceList():tryGetSelectSourceFileName(),
        xml_ExtOptions(_ExtName, ExtList, Xml_Options),
        list::isMemberEq(string::equalIgnoreCase, filename::getExtension(FileName), ExtList),
%        filename::getExtension(FileName) in ExtList,
        !,
        foreach tuple(ItemID, _Name, CmdName, ArgName) in localOptionsList_V do
            setLocalOptionsRow(ItemID, Xml_Options, CmdName, ArgName)
        end foreach.
    showSourceLocalOptions().

predicates
    setLocalOptionsRow: (listViewControl::itemId ItemID,xmlDocument Xml_Options,string CmdName,string ArgName).
clauses
    setLocalOptionsRow(ItemID, Xml_Options, CmdName, _ArgName):-
        Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child(CmdName, { (_) })]),
        !,
        if CmdName = sourceEditor_C then
            FirstArg = if FN = Node:attribute(fileName_C) then FN else "" end if,
            SecondArg = getArgumentStr(ItemID, Node, argEditor_C),
            if FirstArg = "" then
                ResultString = ws_Events():getString(noRuleStatus_C)
            else
                ResultString = string::format(@{% % "$(SourceFile)"}, FirstArg, SecondArg)
            end if
        elseif CmdName = sourcePerformer_C then
            FirstArg = if FN = Node:attribute(fileName_C) then FN else "" end if,
            if ItemID = suffixItemID then % It's "Run suffix"
                SecondArg = getArgumentStr(runItemID, Node, argPerformer_C),
                ThreeArg = getArgumentStr(ItemID, Node, suffixPerformer_C)
            else
                SecondArg = getArgumentStr(ItemID, Node, if ItemID = runItemID then argPerformer_C else reargPerformer_C end if),
                ThreeArg = getArgumentStr(suffixItemID, Node, suffixPerformer_C)
            end if,
            if FirstArg = "" then
                ResultString = ws_Events():getString(noRuleStatus_C)
            else
                ResultString = string::format(@{% % "$(SourceFile)" %}, FirstArg, SecondArg, ThreeArg)
            end if
        elseif CmdName = sourceExecute_C then
            FirstArg = if FN = Node:attribute(cmdExecute_C) then FN else "" end if,
            SecondArg = getArgumentStr(ItemID, Node, argExecute_C),
            if "true" = Node:attribute(execOn_C) then
                ResultString = string::format(@{% %}, FirstArg, SecondArg)
            else
                ResultString = ws_Events():getString(txtExecCmdDisabled)
            end if
        else
            ResultString = ""
        end if,
        panelControl_ctl:updateTextSubItem(ItemID, 3, ResultString).
    setLocalOptionsRow(_ItemID, _Xml_Options, _CmdName, _ArgName).

predicates
    getArgumentStr : (listViewControl::itemId ItemID,xmlElement Node,string ArgName) -> string ArgumentStr.
clauses
    getArgumentStr(ItemID, Node, ArgName) = ArgumentStr :-
        listViewControl::item(ItemID, _, _, _, [LocalOptions, AddMode, _, _]) == panelControl_ctl:getItem(ItemID),
        if AddMode = ws_Events():getString(pmnAdd) then
            ArgumentStr = if SN = Node:attribute(ArgName) then string::concat(SN," ",LocalOptions) else LocalOptions end if
        else
            ArgumentStr = LocalOptions
        end if.

predicates
    onListViewControlMouseClick : listViewControl::mouseClickListener.
clauses
    onListViewControlMouseClick(Source, PNT):-
        ItemID = list::getMember_nd(panelControl_ctl:getAll()),
        RCT = Source:getItemRect(ItemID, listViewControl::bounds),
        listViewControl::column(_,  Width1, _) = Source:getColumn(1),
        listViewControl::column(_,  Width2, _) = Source:getColumn(2),
        listViewControl::column(_,  Width3, _) = Source:getColumn(3),
        listViewControl::column(_,  Width4, _) = Source:getColumn(4),
        RCT = rct(L,T,_,B),
        RCTColumn2 = rct(L+Width1,T,L+Width1+Width2,B),
        RCTColumn3 = rct(L+Width1+Width2,T,L+Width1+Width2+Width3,B),
        RCTColumn4 = rct(L+Width1+Width2+Width3,T,L+Width1+Width2+Width3+Width4,B),
        if gui::rectPntInside(RCTColumn2, PNT) then
            ColNumber = edit_id,
            EditRCT = RCTColumn2,
            EditCtrl = convert(control,edit_ctl)
        elseif gui::rectPntInside(RCTColumn3, PNT) then
            ColNumber = listbutton_id,
            EditRCT = RCTColumn3,
            EditCtrl = convert(control,listbutton_ctl)
        elseif gui::rectPntInside(RCTColumn4, PNT) then
            ColNumber = feListButton_id,
            EditRCT = RCTColumn4,
            EditCtrl = convert(control,feListButton_ctl)
        else
            fail
        end if,
        Source:tryGetItemIndex(ItemId) = ItemIndex,
        Source:getItemColumnText(ItemIndex, ColNumber) = Text,
        Source:select([ItemId], true),
        EditCtrl:setRect(This:rectPixel2Unit(EditRCT)),
        if ColNumber = edit_id then
            edit_ctl:setText(Text)
        elseif ColNumber = listbutton_id then
            listbutton_ctl:selectAt(list::tryGetIndex(Text, addModeListValue_V), true)
        else
            feListButton_ctl:selectAt(list::tryGetIndex(Text, feModeListValue_V), true)
        end if,
        [SourceID] = wSFE_SourceList():sourceList_P:getSel(),
        !,
        editSource_V := SourceID,
        EditCtrl:setVisible(true),
        EditCtrl:bringToTop(),
        EditCtrl:setFocus().
    onListViewControlMouseClick(_Source, _PNT).

class predicates
    dlgc_translate : (gui_native::lResult) -> gui_native::lResult.
clauses
    dlgc_translate(Code) = gui_api::mkR(bit::bitOr(gui_api::getUnsigned(Code), gui_native::dlgc_wantallkeys)).

predicates
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
            [ItemID] = panelControl_ctl:getSel(),
            if convert(control,Source):getCtrlId() = feListButton_id and ItemID in [runItemID, rerunItemID, suffixItemID] then
                panelControl_ctl:updateTextSubItem(runItemID, convert(control,Source):getCtrlId()-2, Source:getText()),
                panelControl_ctl:updateTextSubItem(rerunItemID, convert(control,Source):getCtrlId()-2, Source:getText()),
                panelControl_ctl:updateTextSubItem(suffixItemID, convert(control,Source):getCtrlId()-2, Source:getText())
            else
                panelControl_ctl:updateTextSubItem(ItemID, convert(control,Source):getCtrlId()-2, Source:getText())
            end if,
            updateLocalOptions(),
            if newSourceLocalOptions_V <> [] then
                setLocalExtOptionsList(newSourceLocalOptions_V)
            else
                showSourceLocalOptions(),
                panelControl_ctl:setFocus()
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
        LocalOptionsList = [tuple(ArgName, [namedValue(value_C, string(Value)), namedValue(addMode_C, string(toString(AddMode))), namedValue(feMode_C, string(toString(FeMode)))]) ||
            tuple(ItemID, _Name, _CmdName, ArgName) in localOptionsList_V,
            panelControl_ctl:tryGetItemIndex(ItemId) = ItemIndex,
            Value = panelControl_ctl:getItemColumnText(ItemIndex, edit_id),
            AddMode = toBoolean(ws_Events():getString(pmnAdd) = panelControl_ctl:getItemColumnText(ItemIndex, listButton_id)),
            FeMode = toBoolean(ws_Events():getString(pmnFrontEnd) = panelControl_ctl:getItemColumnText(ItemIndex, feListButton_id))
            ],
        wsFE_Tasks():saveLocalOptions([namedValue(toString(editSource_V), string(toString(LocalOptionsList)))]),
        editSource_V := listViewControl::itemId_zero.

clauses
    showLocalOptionsPanel():-
        showLocalOptionsPanel(toBoolean(0 = localOptionsPanel_P:getHeight())).

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
        wsFE_Command_P :=erroneous,
        progress :=erroneous,

        localOptionsPanel_P :=erroneous,
        panelControl_ctl :=erroneous,
        edit_ctl :=erroneous,
        listButton_ctl:=erroneous,
        feListButton_ctl:=erroneous,
        editSource_V:=listViewControl::itemId_zero,
        newSourceLocalOptions_V:=erroneous,

%        xml_ExtOptions:=erroneous,
%        filterMenuCommand:=erroneous,
%        feModeByType :=erroneous,

        addModeListValue_V :=erroneous,
        feModeListValue_V :=erroneous,
        columnList_V :=erroneous,
        localOptionsList_V :=erroneous,
        ribbonControl_ctl :=erroneous,
        statusBarControl :=erroneous,
        progressBar :=erroneous,
        selectNode_V :=erroneous,
        succeed().

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data):-
        _ = wsFE_SourceList():sourceList_P:sendNative(listViewControl::lvm_setextendedlistviewstyle,
                gui_api::mkW(listViewControl::lvs_ex_labelTip), gui_api::mkL(listViewControl::lvs_ex_labelTip)).


% This code is maintained automatically, do not update it manually. 00:17:58-20.1.2017

facts
    ribbonControl_ctl : ribboncontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setFont(vpi::fontCreateByName("Tahoma", 8)),
        setText("WorkSpaceForm"),
        setRect(rct(100, 100, 592, 341)),
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
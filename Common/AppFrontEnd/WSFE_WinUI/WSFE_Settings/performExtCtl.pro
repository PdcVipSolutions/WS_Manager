%

implement performExtCtl
    inherits userControlSupport
    open core, vpiDomains, ws_EventManager, xmlNavigate

facts
    extOptionsList_P : namedValue*.

%    openTab_P : openTab.
%    runTab_P : runTab.
%    execTab_P : execTab.

    cmdTab : (integer Index, tabPage TabPage, commandTab CommandTab).

    xml_ExtOptions : (string Ext, xmlDocument).
    codePageList_F : namedValue* := [].
    formatResultStr_f : string := "".

    optionsDialog : wsfe_Settings.

clauses
    new(Wsfe_Settings, Parent, SelectSourceType, ExtOptionsList):-
        userControlSupport::new(),
        generatedInitialize(),
        setContainer(Parent),
        optionsDialog := Wsfe_Settings,
        TitleDialogList = optionsDialog:getDialogTitleList("performExtCtl"),
        setupCommandTabs(TitleDialogList),
        addExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbAdd")),
        delExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbDelete")),
        staticText1_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtValue")),
        staticText5_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtCmpName")),
        staticText_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtName")),
        formatResultStr_f := namedValue::getNamed_string(TitleDialogList, "txtResultStr"),
        extOptionsList_P := ExtOptionsList,
        ExtList = [Name || namedValue(Name,_) in ExtOptionsList],
        extLBox_ctl:addList(ExtList),
        if ExtList <> [] then
            if Index = list::tryGetIndex(SelectSourceType, ExtList) then
                extLBox_ctl:selectAt(Index, true)
            else
                extLBox_ctl:selectAt(0, true)
            end if
        end if,
        !.

predicates
    setupCommandTabs : (namedValue* TitleDialogList).
clauses
    setupCommandTabs(TitleDialogList):-
        retractAll(cmdTab(_,_,_)),
        foreach Index = std::cIterate(commandMax)+1 do
            Page = tabPage::new(),
            CommandTab = commandTab::new(Page:getContainerControl()),
            Page:setText(string::format("Command # %", Index)),
            assert(cmdTab(Index, Page, CommandTab)),
            tabControl_ctl:addPage(Page),
            setStreamModeValues(CommandTab),
            setControlsID(CommandTab),
            addControlsText(CommandTab, TitleDialogList),
            addControlsListners(CommandTab)
        end foreach,
        editComponentName_ctl:setCtrlId(editComponentName_id),
        editComponentName_ctl:addModifiedListener(onComponentEditorModified).

predicates
    setStreamModeValues : (commandTab CommandTab).
clauses
    setStreamModeValues(CommandTab):-
        CommandTab:lbSteramMode_P:addList(["unicode","ansi(codePage)","binary"]),
        CommandTab:lbSteramMode_P:selectAt(0, true),
        SubKeys = registry::getSubkeys(registry::classesRoot, @"MIME\Database\CodePage"),
        codePageList_F := [namedValue(SubKey, string(CodePage)) ||
            SubKey in SubKeys,
            Key = string::format(@"MIME\Database\CodePage\%", SubKey),
            string(CodePage) = registry::tryGetValue(registry::classesRoot, Key, "BodyCharset")],
        CommandTab:lbCodePage_P:addList([CP||namedValue(_, string(CP)) in codePageList_F]),
        CommandTab:lbCodePage_P:selectAt(0, true).

predicates
    addControlsText: (commandTab CommandTab, namedValue* TitleDialogList).
clauses
    addControlsText(CommandTab, TitleDialogList):-
        CommandTab:browseEditor_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        ApplicationFileStr = namedValue::getNamed_string(TitleDialogList, "txtApplicationFile"),
        CommandTab:txtApplicationFile_P:setText(ApplicationFileStr),
        ArgumentsStr = namedValue::getNamed_string(TitleDialogList, "txtArguments"),
        CommandTab:txtArguments_P:setText(ArgumentsStr),
        FormatCmdStr = namedValue::getNamed_string(TitleDialogList, "txtFormatCmd"),
        CommandTab:txtFormatCommand_P:setText(FormatCmdStr),
        PerformAtFrontEnd = namedValue::getNamed_string(TitleDialogList, "txtPerformFE"),
        CommandTab:cbFEMode_P:setText(PerformAtFrontEnd),
        DefaultCommand = namedValue::getNamed_string(TitleDialogList, "txtDefCommand"),
        CommandTab:cbDefCommand_P:setText(DefaultCommand),
        CommandTab:browseSuffix_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        CommandTab:txtCodePage_P:setText(namedValue::getNamed_string(TitleDialogList, "txtCodePage")),
%        CommandTab:txtStreamMode_P:setText(namedValue::getNamed_string(TitleDialogList, "txtStreamMode")),
        CommandTab:cbStreamMode_P:setText(namedValue::getNamed_string(TitleDialogList, "cbStreamMode")),
        SuffixStr = namedValue::getNamed_string(TitleDialogList, "txtSuffix"),
        CommandTab:txtSuffix_P:setText(SuffixStr),
        CommandTab:cbCallAssociations_P:setText(namedValue::getNamed_string(TitleDialogList, "cbInvokeWinAss")),
        !.

predicates
    setControlsID : (commandTab CommandTab).
clauses
    setControlsID(CommandTab):-
        CommandTab:applicationFileName_P:setCtrlId(editorFileName_id),
        CommandTab:editArguments_P:setCtrlId(editArgString_id),
        CommandTab:browseEditor_P:setCtrlId(browseEditor_id),
        CommandTab:cbFEMode_P:setCtrlId(performOpen_id),
        CommandTab:cbDefCommand_P:setCtrlId(defCommand_id),
        CommandTab:editSuffix_P:setCtrlId(editSuffix_id),
        CommandTab:lbSteramMode_P:setCtrlId(lbSteramMode_id),
        CommandTab:lbCodePage_P:setCtrlId(lbCodePage_id),
        CommandTab:browseSuffix_P:setCtrlId(browseSuffix_id),
        CommandTab:getMacroSymbols_P:setCtrlId(getMacroSymbols_id),
        CommandTab:cbCallAssociations_P:setCtrlId(cbCallAssociations_id),
        CommandTab:editFormatCommand_P:setCtrlId(editFormatCommand_id).
%        CommandTab:editCommandName_P:setCtrlId(editCommandName_id).

predicates
    addControlsListners : (commandTab CommandTab).
clauses
    addControlsListners(CommandTab):-
        CommandTab:applicationFileName_P:addModifiedListener(onEditorModified),
        CommandTab:editArguments_P:addModifiedListener(onEditorModified),
        CommandTab:editFormatCommand_P:addModifiedListener(onEditorModified),
%        CommandTab:editCommandName_P:addModifiedListener(onEditorModified),
        CommandTab:editSuffix_P:addModifiedListener(onEditorModified),
        CommandTab:lbSteramMode_P:addSelectionChangedListener(onStreamModeSelectionChanged),
        CommandTab:lbCodePage_P:addSelectionChangedListener(onCodePageSelectionChanged),
        CommandTab:cbFEMode_P:addStateChangedListener(onCbPerformStateChanged),
        CommandTab:getMacroSymbols_P:setClickResponder(optionsDialog:onGetMacroSymbolsClick),
        CommandTab:cbCallAssociations_P:addStateChangedListener(onCbCallAssociationsStateChanged),
        CommandTab:browseEditor_P:setClickResponder(optionsDialog:onBrowseEditorClick),
        CommandTab:browseSuffix_P:setClickResponder(optionsDialog:onBrowseEditorClick).

predicates
    onCbCallAssociationsStateChanged : checkButton::stateChangedListener.
clauses
    onCbCallAssociationsStateChanged(_Source, _OldState, _NewState):-
        !.

%predicates
%    onCbExecutePossibleStateChanged : checkButton::stateChangedListener.
%clauses
%    onCbExecutePossibleStateChanged(Source, _OldState, _NewState):-
%        NewValue = toString(Source:getChecked()),
%        if
%            Index = extLBox_ctl:tryGetSelectedIndex(),
%            ExtName = extLBox_ctl:getAt(Index),
%            xml_ExtOptions(ExtName, Xml_Options),
%            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
%        then
%            if Node = Xml_Options:getNode_nd([current(GroupNode), child(sourceExecute_C, { (_) })]) then
%            else
%                Node = xmlElement::new("", sourceExecute_C,GroupNode),
%%                Node:name_P := sourceExecute_C,
%                GroupNode:addNode(Node)
%            end if,
%            if _ = Node:attribute(execOn_C) then
%                Node:modifyAttribute(execOn_C, NewValue)
%            else
%                Node:addAttribute(execOn_C, NewValue)
%            end if
%        end if.

predicates
    onCbPerformStateChanged : checkButton::stateChangedListener.
clauses
    onCbPerformStateChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        CommandTab = convert(commandTab, Source:getParent()),
        if
            cmdTab(TabIndex, _, CommandTab),
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
            Node = Xml_Options:getNode_nd([current(GroupNode), child("*", { (CN) :- CN:attribute(index_C) = toString(TabIndex)})])
        then
            if _ = Node:attribute(feMode_C) then
                Node:modifyAttribute(feMode_C, NewValue)
            else
                Node:addAttribute(feMode_C, NewValue)
            end if
        end if.

predicates
    onEditorModified : editControl::modifiedListener.
clauses
    onEditorModified(Source):-
        CommandTab = convert(commandTab, Source:getParent()),
        updateAttribute(Source),
        updateResultString(CommandTab).

predicates
    onComponentEditorModified : editControl::modifiedListener.
clauses
    onComponentEditorModified(Source):-
        NewValue = string::trim(Source:getText()),
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
            Node = Xml_Options:getNode_nd([current(GroupNode), child("common", { (_) })])
        then
            if _ = Node:attribute(componentName_C) then
                Node:modifyAttribute(componentName_C, NewValue)
            elseif NewValue <> "" then
                Node:addAttribute(componentName_C, NewValue)
            end if
        end if.

clauses
    updateResultString(CommandTab):-
        FC = CommandTab:editFormatCommand_P:getText(),
        FC1 = string::replaceAll(FC, "[Application]", CommandTab:applicationFileName_P:getText()),
        FC2 = string::replaceAll(FC1, "[Arguments]", CommandTab:editArguments_P:getText()),
        FC3 = string::replaceAll(FC2, "[Suffix]", CommandTab:editSuffix_P:getText()),
        CommandTab:resultCommandLine_P:setText(FC3),
        !.

clauses
    updateAttribute(Source):-
        ID = Source:getCtrlId(),
        NewValue = string::trim(Source:getText()),
        CommandTab = convert(commandTab, Source:getParent()),
        if
            cmdTab(TabIndex, PageTab, CommandTab),
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
            Node = Xml_Options:getNode_nd([current(GroupNode), child("*", { (CN) :- CN:attribute(index_C) = toString(TabIndex)})])
        then
            if editorFileName_id = ID then AttributeName = fileName_C
            elseif editFormatCommand_id = ID then AttributeName = formatCmd_C
            elseif editCommandName_id = ID then
                AttributeName = name_C,
                CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, NewValue)),
                PageText =  if NewValue = "" then string::format("Command # %", TabIndex) else NewValue end if,
                PageTab:setText(PageText)
            elseif editArgString_id = ID then AttributeName = argument_C
            elseif editSuffix_id = ID then AttributeName = suffix_C
            else AttributeName = ""
            end if,
            if AttributeName <> "" then
                if _ = Node:attribute(AttributeName) then
                    Node:modifyAttribute(AttributeName, NewValue)
                elseif NewValue <> "" then
                    Node:addAttribute(AttributeName, NewValue)
                end if
            end if
        end if.

predicates
    onStreamModeSelectionChanged : listControl::selectionChangedListener.
clauses
    onStreamModeSelectionChanged(Source):-
        CommandTab = convert(commandTab, Source:getParent()),
        if
            cmdTab(TabIndex, _PageTab, CommandTab),
            Index = Source:tryGetSelectedIndex(),
            CommandTab:lbCodePage_P:setEnabled(toBoolean(1 /*ansi(codePage)*/ = Index)),
            IndexLB = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(IndexLB),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
            Node = Xml_Options:getNode_nd([current(GroupNode), child("*", { (CN) :- CN:attribute(index_C) = toString(TabIndex)})])
        then
            if _ = Node:attribute(streamMode_C) then
                Node:modifyAttribute(streamMode_C, toString(Index))
            else
                Node:addAttribute(streamMode_C, toString(Index))
            end if
        end if.

predicates
    onCodePageSelectionChanged : listControl::selectionChangedListener.
clauses
    onCodePageSelectionChanged(Source):-
        CommandTab = convert(commandTab, Source:getParent()),
        if
            cmdTab(TabIndex, _PageTab, CommandTab),
            Index = Source:tryGetSelectedIndex(),
            BodyCharset = Source:getAt(Index),
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(codePageList_F),
            IndexLB = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(IndexLB),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
            Node = Xml_Options:getNode_nd([current(GroupNode), child("*", { (CN) :- CN:attribute(index_C) = toString(TabIndex)})])
        then
            if _ = Node:attribute(codePage_C) then
                Node:modifyAttribute(codePage_C, CodePage)
            else
                Node:addAttribute(codePage_C, CodePage)
            end if
        end if.

predicates
    onExtLBoxSelectionChanged : listControl::selectionChangedListener.
clauses
    onExtLBoxSelectionChanged(_Source):-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            namedValue(ExtName, string(ExtOptionsStr)) in extOptionsList_P
        then
            if xml_ExtOptions(ExtName, Xml_Options) then
            else
                Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
            end if,
            foreach Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("*", { (_) })]) do
                if Node:name_P = "common" then
                    ExtListStr = if EL = Node:attribute(extName_C) then EL else "" end if,
                    editExtList_ctl:setText(ExtListStr),
                    ComponentName = if C = Node:attribute(componentName_C) then C else "" end if,
                    editComponentName_ctl:setText(ComponentName)
                elseif
                    TabIndex = tryToTerm(integer, Node:attribute(index_C)),
                    cmdTab(TabIndex, Page, CommandTab)
                then
                    if
                        CommandName = Node:attribute(name_C),
                        CommandName <> ""
                    then
                        Page:setText(CommandName),
                        CommandTab:editCommandName_P:setText(CommandName),
                        CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, CommandName)),
                        FormatCommand = if FC = Node:attribute(formatCmd_C) then FC else "" end if,
                        CommandTab:editFormatCommand_P:setText(FormatCommand),
                        ApplicationName = if S = Node:attribute(fileName_C) then S else "" end if,
                        CommandTab:applicationFileName_P:setText(ApplicationName),
                        Arguments = if A = Node:attribute(argument_C) then A else "" end if,
                        CommandTab:editArguments_P:setText(Arguments),
                        SuffixPerformer = if SP = Node:attribute(suffix_C) then SP else "" end if,
                        CommandTab:editSuffix_P:setText(SuffixPerformer),
                        StreamModeIndex = if SMI = toTerm(positive, Node:attribute(streamMode_C)) then SMI else 0 end if,
                        CommandTab:lbSteramMode_P:selectAt(StreamModeIndex, true),
                        PerformOn = toBoolean("true" = Node:attribute(feMode_C)),
                        CommandTab:cbFEMode_P:setChecked(PerformOn),
                        if CP = Node:attribute(codePage_C),
                            list::memberIndex_nd(NV, CodePageIndex, codePageList_F),
                            NV = namedValue(CP, _)
                        then
                            CommandTab:lbCodePage_P:selectAt(CodePageIndex, true)
                        else
                            CommandTab:lbCodePage_P:selectAt(0, true)
                        end if
                    else
                        Page:setText(string::format("Command # %", TabIndex)),
                        CommandTab:editCommandName_P:setText(""),
                        CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, ""))
                    end if
                end if
            end foreach
%            updateResultString()
        end if.

predicates
    extOptionsStr2xml_Options : (string ExtName,string ExtOptionsStr) -> xmlDocument Xml_Options.
clauses
    extOptionsStr2xml_Options(ExtName, ExtOptionsStr) = Xml_Options :-
        Xml_Options=xmlDocument::new(ExtName),
        Xml_Options:codePage_P:=utf8,
        Xml_Options:indent_P:=true,
        Xml_Options:xmlStandalone_P:=xmlLite::yes,
        assert(xml_ExtOptions(ExtName, Xml_Options)),
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
                CmdNode:addAttribute(AttrName, AttrValue)
            end foreach
        end foreach.

predicates
    onAddExtClick : button::clickResponder.
clauses
    onAddExtClick(_Source) = button::defaultAction :-
        TitleDialogList = optionsDialog:getDialogTitleList("addNewSourceType"),
            ReturnValue = addNewSourceType::display(This, existsExtValue, TitleDialogList),
        if tuple(NewName, NewExtStr) = isSome(ReturnValue) then
            addNewSourceType(NewName, string::toLowerCase(string::trim(NewExtStr)))
        end if.

predicates
    existsExtValue : topLevelContainerWindow::validateResponder.
clauses
    existsExtValue(Dialog) = ValidResult :-
        EditName = convert(addNewSourceType, Dialog):editName_ctl,
        NewName = EditName:getText(),
        EditValue = convert(addNewSourceType, Dialog):editValue_ctl,
        NewValue = string::trim(EditValue:getText()),
        ErrorMsgList = optionsDialog:getDialogTitleList("existsExtValueMsg"),
        if list::isMemberEq(existsName, NewName, extOptionsList_P) then
            FormatStr = namedValue::getNamed_string(ErrorMsgList, "txtTypeExists"),
            ErrorMessage = string::format(FormatStr, NewName),
            ValidResult = control::contentsInvalid(EditName, EditName, ErrorMessage)
        elseif "" <> NewValue and
            ExtList = splitExtListStr(NewValue),
            tuple(EN, EE) = checkExtList(NewName, ExtList) then
            FormatStr = namedValue::getNamed_string(ErrorMsgList, "txtExtAdded"),
            ErrorMessage = string::format(FormatStr, EE, EN),
            ValidResult = control::contentsInvalid(EditValue, EditValue, ErrorMessage)
        elseif NewName <> "" and "" = NewValue then
            ErrorMessage = namedValue::getNamed_string(ErrorMsgList, "txtExtRequired"),
            ValidResult = control::contentsInvalid(EditValue, EditValue, ErrorMessage)
        else
            ValidResult = control::contentsOk
        end if.

predicates
    existsName : predicate_dt{string, namedValue}.
clauses
    existsName(NewName, namedValue(ExtName, _)):-
        string::equalIgnoreCase(NewName, ExtName),
        !.

predicates
    splitExtListStr : (string ExtListStr) -> string* ExtList.
clauses
    splitExtListStr(ExtListStr) =
        list::filteredMap(string::split(ExtListStr, separatorsExt), {(S) = R :- R = string::trim(S), R <> ""}).

predicates
    checkExtList : (string NameExt,string* NewExtList) -> tuple{string, string} determ.
clauses
    checkExtList(NameExt, NewExtList) = tuple(ExtName, IntExtStr) :-
        namedValue(ExtName, string(ExtOptionsStr)) in extOptionsList_P,
        not(string::equalIgnoreCase(NameExt, ExtName)),
            if xml_ExtOptions(ExtName, Xml_Options) then
            else
                Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
            end if,
            Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]),
            ExtListStr = Node:attribute(extName_C),
            OldExtList = splitExtListStr(string::trim(string::toLowerCase(ExtListStr))),
            IntExtList = list::intersection(NewExtList, OldExtList),
        IntExtList <> [],
        IntExtStr = string::concatWithDelimiter(IntExtList, ","),
        !.

constants
    newTemplateExtOptions_C : extOptions =
        [
        tuple(command_C,[namedValue(index_C, string("1"))]),
        tuple(command_C,[namedValue(index_C, string("2"))]),
        tuple(command_C,[namedValue(index_C, string("3"))]),
        tuple(command_C,[namedValue(index_C, string("4"))])
        ].
predicates
    addNewSourceType : (string NewName, string NewExtStr).
clauses
    addNewSourceType(NewName, NewExtStr):-
        extOptionsList_P := list::append(extOptionsList_P,
            [namedValue(NewName, string(toString([tuple("common",[namedValue(extName_C, string(NewExtStr))])|newTemplateExtOptions_C])))]),
        extLBox_ctl:add(NewName),
        extLBox_ctl:selectAt(list::length(extOptionsList_P)-1, true).

predicates
    onDelExtClick : button::clickResponder.
clauses
    onDelExtClick(_Source) = button::defaultAction :-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            Ext = extLBox_ctl:getAt(Index),
            namedValue(Ext, string(ExtOptionsStr)) in extOptionsList_P
        then
            extOptionsList_P := list::remove(extOptionsList_P, namedValue(Ext, string(ExtOptionsStr))),
            extLBox_ctl:delete(Index),
            if extOptionsList_P <> [] then
                extLBox_ctl:selectAt(math::min(Index, list::length(extOptionsList_P)-1), true)
%            else
            end if
        end if.

predicates
    onExtLBoxMouseDbl : window::mouseDblListener.
clauses
    onExtLBoxMouseDbl(_Source, _Point, _ShiftControlAlt, _Button):-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            NewName = commonDialogs::tryGet_string(This, "Edit Source type name", "", "", ExtName, existsExtName),
            not(string::equalIgnoreCase(NewName, ExtName))
        then
            extOptionsList_P := [namedValue(Name, Value)||namedValue(N, Value) in extOptionsList_P, Name = if N = ExtName then NewName else N end if],
            extLBox_ctl:delete(Index),
            extLBox_ctl:addAt(Index, NewName),
            extLBox_ctl:selectAt(Index, true)
       end if.

predicates
    existsExtName : function_dt{string Result, string ErrorMessage}.
clauses
    existsExtName(NewName) = ErrorMessage :-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            not(string::equalIgnoreCase(NewName, extLBox_ctl:getAt(Index))),
            list::isMemberEq(existsName, NewName, extOptionsList_P)
        then
            ErrorMessage = string::format("The Source type name <%s> exists!", NewName)
        else
            fail
        end if.

predicates
    onEditExtListLoseFocus : window::loseFocusListener.
clauses
    onEditExtListLoseFocus(_Source):-
        ExtListStr = editExtList_ctl:getText(),
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            ExtList = splitExtListStr(string::trim(string::toLowerCase(ExtListStr))),
            tuple(EN, EE) = checkExtList(ExtName, ExtList)
        then
            ErrorMessage = string::format("Extension(s) <%s> already added into the source type name <%s>!", EE, EN),
            messageBox::displayError(This, ErrorMessage),
            This:postUserEvent(0,1)
        else
            updateExtList(ExtListStr)
        end if.

predicates
    updateExtList : (string ExtListStr).
clauses
    updateExtList(ExtListStr):-
        Index = extLBox_ctl:tryGetSelectedIndex(),
        ExtName = extLBox_ctl:getAt(Index),
        xml_ExtOptions(ExtName, Xml_Options),
        Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]),
        !,
        Node:modifyAttribute(extName_C, ExtListStr).
    updateExtList(_).

facts
    newExtOptionsList : namedValue* := [].
clauses
    createNewExtOptionsList():-
        newExtOptionsList := [],
        foreach namedValue(ExtName, string(ExtOptionsStr)) in extOptionsList_P do
            if xml_ExtOptions(ExtName, Xml_Options) then
            else
                Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
            end if,
            if
                Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]),
                ExtListStr = Node:attribute(extName_C)
            then
                newExtOptionsList := list::append(newExtOptionsList, [namedValue(ExtName, string(ExtListStr))])
            end if
        end foreach.

clauses
    getExtOptionsValues(ExtName, ExtOptionsStr) = OptionsValues :-
        if xml_ExtOptions(ExtName, Xml_Options) then
        else
            Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
        end if,
        OptionsValues = [tuple(Node:name_P, AttributesList)||
            Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("*", { (_) })]),
%            Node:name_P <> "common",
            AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]
            ].

% This code is maintained automatically, do not update it manually. 14:21:38-6.10.2017

facts
    extLBox_ctl : listBox.
    addExt_ctl : button.
    delExt_ctl : button.
    editComponentName_ctl : editControl.
    tabControl_ctl : tabcontrol.
    staticText_ctl : textControl.
    staticText5_ctl : textControl.
    staticText1_ctl : textControl.
    editExtList_ctl : editControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Source Type"),
        This:setSize(380, 230),
        extLBox_ctl := listBox::new(This),
        extLBox_ctl:setPosition(4, 20),
        extLBox_ctl:setSize(80, 188),
        extLBox_ctl:setSort(false),
        extLBox_ctl:setAnchors([control::left, control::top, control::bottom]),
        extLBox_ctl:addSelectionChangedListener(onExtLBoxSelectionChanged),
        extLBox_ctl:addMouseDblListener(onExtLBoxMouseDbl),
        addExt_ctl := button::new(This),
        addExt_ctl:setText("Add"),
        addExt_ctl:setPosition(4, 212),
        addExt_ctl:setSize(36, 12),
        addExt_ctl:defaultHeight := false,
        addExt_ctl:setAnchors([control::left, control::bottom]),
        addExt_ctl:setClickResponder(onAddExtClick),
        delExt_ctl := button::new(This),
        delExt_ctl:setText("Delete"),
        delExt_ctl:setPosition(48, 212),
        delExt_ctl:setSize(36, 12),
        delExt_ctl:defaultHeight := false,
        delExt_ctl:setAnchors([control::left, control::bottom]),
        delExt_ctl:setClickResponder(onDelExtClick),
        editComponentName_ctl := editControl::new(This),
        editComponentName_ctl:setText("Edit"),
        editComponentName_ctl:setPosition(156, 20),
        editComponentName_ctl:setWidth(216),
        editComponentName_ctl:setAnchors([control::left, control::top, control::right]),
        tabControl_ctl := tabcontrol::new(This),
        tabControl_ctl:setPosition(88, 34),
        tabControl_ctl:setSize(284, 190),
        tabControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        editExtList_ctl := editControl::new(This),
        editExtList_ctl:setText("Edit"),
        editExtList_ctl:setPosition(156, 6),
        editExtList_ctl:setWidth(216),
        editExtList_ctl:setAnchors([control::left, control::top, control::right]),
        editExtList_ctl:setLowerCase(),
        editExtList_ctl:addLoseFocusListener(onEditExtListLoseFocus),
        staticText1_ctl := textControl::new(This),
        staticText1_ctl:setText("Extension List: "),
        staticText1_ctl:setPosition(88, 7),
        staticText1_ctl:setSize(64, 10),
        staticText5_ctl := textControl::new(This),
        staticText5_ctl:setText("Component Name:"),
        staticText5_ctl:setPosition(88, 21),
        staticText5_ctl:setSize(64, 10),
        staticText_ctl := textControl::new(This),
        staticText_ctl:setText("Source Type Name:"),
        staticText_ctl:setPosition(4, 7),
        staticText_ctl:setSize(68, 10).
% end of automatic code
end implement performExtCtl
%

implement performExtCtl
    inherits userControlSupport
    open core, vpiDomains, ws_EventManager, xmlNavigate

facts
    extOptionsList_P : namedValue*.

    openTab_P : openTab.
    runTab_P : runTab.
    execTab_P : execTab.

    xml_ExtOptions : (string Ext, xmlDocument).
    codePageList_F : namedValue* := [].

    optionsDialog : wsfe_Settings.

clauses
    new(Wsfe_Settings, Parent, SelectSourceType, ExtOptionsList):-
        userControlSupport::new(),
        generatedInitialize(),
        setContainer(Parent),
        optionsDialog := Wsfe_Settings,
        TitleDialogList = optionsDialog:getDialogTitleList("performExtCtl"),
        Page1 = tabPage::new(),
        openTab_P := openTab::new(Page1:getContainerControl()),
        Page1:setText(namedValue::getNamed_string(TitleDialogList, "tabOpen")),
        tabControl_ctl:addPage(Page1),
        Page2 = tabPage::new(),
        runTab_P := runTab::new(Page2:getContainerControl()),
        Page2:setText(namedValue::getNamed_string(TitleDialogList, "tabRun")),
        tabControl_ctl:addPage(Page2),
        Page3 = tabPage::new(),
        execTab_P := execTab::new(Page3:getContainerControl()),
        Page3:setText(namedValue::getNamed_string(TitleDialogList, "tabExecute")),
        tabControl_ctl:addPage(Page3),
        addExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbAdd")),
        delExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbDelete")),
        staticText1_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtValue")),
        staticText5_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtCmpName")),
        staticText_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtName")),
        setStreamModeValues(),
        setControlsID(),
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
        addControlsText(TitleDialogList),
        addControlsListners().

predicates
    setStreamModeValues : ().
clauses
    setStreamModeValues():-
        runTab_P:lbSteramMode_P:addList(["unicode","ansi(codePage)","binary"]),
        runTab_P:lbSteramMode_P:selectAt(0, true),
        SubKeys = registry::getSubkeys(registry::classesRoot, @"MIME\Database\CodePage"),
        codePageList_F := [namedValue(SubKey, string(CodePage)) ||
            SubKey in SubKeys,
            Key = string::format(@"MIME\Database\CodePage\%", SubKey),
            string(CodePage) = registry::tryGetValue(registry::classesRoot, Key, "BodyCharset")],
        runTab_P:lbCodePage_P:addList([CP||namedValue(_, string(CP)) in codePageList_F]),
        runTab_P:lbCodePage_P:selectAt(0, true).

predicates
    addControlsText: (namedValue* TitleDialogList).
clauses
    addControlsText(TitleDialogList):-
        openTab_P:browseEditor_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        OpenFormatStr = namedValue::getNamed_string(TitleDialogList, "txtOpenFormat"),
        EditorFileStr = namedValue::getNamed_string(TitleDialogList, "txtEditorFile"),
        openTab_P:txtEditorFile_P:setText(EditorFileStr),
        ArgumentsStr = namedValue::getNamed_string(TitleDialogList, "txtArguments"),
        openTab_P:txtArguments_P:setText(ArgumentsStr),
        FormatCmdStr = namedValue::getNamed_string(TitleDialogList, "txtFormatCmd"),
        OpenFormatText = string::concat(FormatCmdStr,": ",string::format(OpenFormatStr, EditorFileStr, ArgumentsStr)),
        openTab_P:txtFormatCommand_P:setText(OpenFormatText),
        FormatResultStr = namedValue::getNamed_string(TitleDialogList, "txtResultStr"),
        openTab_P:gbResultStr_P:setText(string::format(FormatResultStr, namedValue::getNamed_string(TitleDialogList, "tabOpen"))),
        runTab_P:gbResultStr_P:setText(string::format(FormatResultStr, namedValue::getNamed_string(TitleDialogList, "tabRun"))),
        execTab_P:gbResultStr_P:setText(string::format(FormatResultStr, namedValue::getNamed_string(TitleDialogList, "tabExecute"))),
        PerformAtFrontEnd = namedValue::getNamed_string(TitleDialogList, "txtPerformFE"),
        openTab_P:cbFEMode_P:setText(PerformAtFrontEnd),
        runTab_P:cbFEMode_P:setText(PerformAtFrontEnd),
        execTab_P:cbFEMode_P:setText(PerformAtFrontEnd),
        runTab_P:browseEditor_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        runTab_P:browseSuffix_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        runTab_P:txtCodePage_P:setText(namedValue::getNamed_string(TitleDialogList, "txtCodePage")),
        runTab_P:txtStreamMode_P:setText(namedValue::getNamed_string(TitleDialogList, "txtStreamMode")),
        RunFileStr = namedValue::getNamed_string(TitleDialogList, "txtRunFile"),
        runTab_P:txtRunFile_P:setText(RunFileStr),
        runTab_P:gbArguments_P:setText(namedValue::getNamed_string(TitleDialogList, "gbArgForRun")),
        runTab_P:txtRunMode_P:setText(namedValue::getNamed_string(TitleDialogList, "txtRunMode")),
        runTab_P:txtReRunMode_P:setText(namedValue::getNamed_string(TitleDialogList, "txtReRunMode")),
        SuffixStr = namedValue::getNamed_string(TitleDialogList, "txtSuffix"),
        runTab_P:txtSuffix_P:setText(SuffixStr),
        RunFormatStr = namedValue::getNamed_string(TitleDialogList, "txtRunFormat"),
        RunFormatText = string::concat(FormatCmdStr,": ",string::format(RunFormatStr, RunFileStr, ArgumentsStr, SuffixStr)),
        runTab_P:txtFormatCommand_P:setText(RunFormatText),
        CommandLineStr = namedValue::getNamed_string(TitleDialogList, "txtCommandLine"),
        execTab_P:txtCommandLine_P:setText(CommandLineStr),
        execTab_P:txtArguments_P:setText(ArgumentsStr),
        execTab_P:cbExecutePossible_P:setText(namedValue::getNamed_string(TitleDialogList, "cbCmdEnabled")),
        ExecFormatStr = namedValue::getNamed_string(TitleDialogList, "txtExecFormat"),
        ExecFormatText = string::concat(FormatCmdStr,": ",string::format(ExecFormatStr, CommandLineStr, ArgumentsStr)),
        execTab_P:txtFormatCommand_P:setText(ExecFormatText).

predicates
    setControlsID : ().
clauses
    setControlsID():-
        openTab_P:editorFileName_P:setCtrlId(editorFileName_id),
        openTab_P:editArgString_P:setCtrlId(editArgString_id),
        openTab_P:browseEditor_P:setCtrlId(browseEditor_id),
        openTab_P:cbFEMode_P:setCtrlId(performOpen_id),
        runTab_P:editorFileName_P:setCtrlId(runFileName_id),
        runTab_P:editArgStringForRun_P:setCtrlId(editArgStringForRun_id),
        runTab_P:editArgStringForReRun_P:setCtrlId(editArgStringForReRun_id),
        runTab_P:editSuffix_P:setCtrlId(editSuffix_id),
        runTab_P:browseEditor_P:setCtrlId(browseRun_id),
        editComponentName_ctl:setCtrlId(editComponentName_id),
        runTab_P:lbSteramMode_P:setCtrlId(lbSteramMode_id),
        runTab_P:lbCodePage_P:setCtrlId(lbCodePage_id),
        runTab_P:browseSuffix_P:setCtrlId(browseSuffix_id),
        runTab_P:cbFEMode_P:setCtrlId(performRun_id),
        execTab_P:editCmdString_P:setCtrlId(execCmdString_id),
        execTab_P:editArgString_P:setCtrlId(execArgString_id),
        execTab_P:getMacroSymbols_P:setCtrlId(getMacroSymbols_id),
        execTab_P:cbFEMode_P:setCtrlId(performExec_id),
        execTab_P:cbExecutePossible_P:setCtrlId(cbExecutePossible_id).

predicates
    addControlsListners : ().
clauses
    addControlsListners():-
        openTab_P:editorFileName_P:addModifiedListener(onEditorModified),
        openTab_P:editArgString_P:addModifiedListener(onEditorModified),
        runTab_P:editorFileName_P:addModifiedListener(onEditorModified),
        runTab_P:editArgStringForRun_P:addModifiedListener(onEditorModified),
        runTab_P:editArgStringForReRun_P:addModifiedListener(onEditorModified),
        runTab_P:editSuffix_P:addModifiedListener(onEditorModified),
        editComponentName_ctl:addModifiedListener(onEditorModified),
        runTab_P:lbSteramMode_P:addSelectionChangedListener(onStreamModeSelectionChanged),
        runTab_P:lbCodePage_P:addSelectionChangedListener(onCodePageSelectionChanged),
        execTab_P:editCmdString_P:addModifiedListener(onEditorModified),
        execTab_P:editArgString_P:addModifiedListener(onEditorModified),
%        execTab_P:getMacroSymbols_P:setClickResponder(onGetMacroSymbolsClick),
        openTab_P:cbFEMode_P:addStateChangedListener(onCbPerformStateChanged),
        runTab_P:cbFEMode_P:addStateChangedListener(onCbPerformStateChanged),
        execTab_P:cbFEMode_P:addStateChangedListener(onCbPerformStateChanged),
        execTab_P:cbExecutePossible_P:addStateChangedListener(onCbExecutePossibleStateChanged).

predicates
    onCbExecutePossibleStateChanged : checkButton::stateChangedListener.
clauses
    onCbExecutePossibleStateChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(sourceExecute_C, { (_) })]) then
            else
                Node = xmlElement::new("", sourceExecute_C,GroupNode),
%                Node:name_P := sourceExecute_C,
                GroupNode:addNode(Node)
            end if,
            if _ = Node:attribute(execOn_C) then
                Node:modifyAttribute(execOn_C, NewValue)
            else
                Node:addAttribute(execOn_C, NewValue)
            end if
        end if.

predicates
    onCbPerformStateChanged : checkButton::stateChangedListener.
clauses
    onCbPerformStateChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        if Source:getCtrlId() = performOpen_id then NodeName = sourceEditor_C
        elseif Source:getCtrlId() = performRun_id then  NodeName = sourcePerformer_C
        elseif Source:getCtrlId() =  performExec_id then NodeName = sourceExecute_C
        else NodeName = ""
        end if,
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(NodeName, { (_) })]) then
            else
                Node = xmlElement::new("", NodeName,GroupNode),
%                Node:name_P := NodeName,
                GroupNode:addNode(Node)
            end if,
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
        updateAttribute(Source),
        updateResultString().

clauses
    updateResultString():-
        OpenResultString = string::format(@{% % "$(SourceFile)"}, openTab_P:editorFileName_P:getText(), openTab_P:editArgString_P:getText()),
        openTab_P:resultCommandLine_P:setText(OpenResultString),
        RunResultString = string::format(@{% % "$(SourceFile)" %},
                                                            runTab_P:editorFileName_P:getText(),
                                                            runTab_P:editArgStringForRun_P:getText(),
                                                            runTab_P:editSuffix_P:getText()),
        runTab_P:resultCommandLine_P:setText(RunResultString),
        ExecResultString = string::format(@{% %},
                                                            execTab_P:editCmdString_P:getText(),
                                                            execTab_P:editArgString_P:getText()),
        execTab_P:resultCommandLine_P:setText(ExecResultString).

clauses
    updateAttribute(Source):-
        ID = Source:getCtrlId(),
        NewValue = string::trim(Source:getText()),
        if ID in [editorFileName_id, editArgString_id] then NodeName = sourceEditor_C
        elseif ID in [runFileName_id, editArgStringForRun_id, editArgStringForReRun_id, editSuffix_id, editComponentName_id] then NodeName = sourcePerformer_C
        elseif ID in [execCmdString_id,execArgString_id] then NodeName = sourceExecute_C
        else NodeName = ""
        end if,
        if editorFileName_id = ID then AttributeName = fileName_C
        elseif editArgString_id = ID then AttributeName = argEditor_C
        elseif runFileName_id = ID then AttributeName = fileName_C
        elseif editArgStringForRun_id = ID then AttributeName = argPerformer_C
        elseif editArgStringForReRun_id = ID then AttributeName = reargPerformer_C
        elseif editSuffix_id = ID then AttributeName = suffixPerformer_C
        elseif editComponentName_id = ID then AttributeName = componentName_C
        elseif execCmdString_id = ID then AttributeName = cmdExecute_C
        elseif execArgString_id = ID then AttributeName = argExecute_C
        else AttributeName = ""
        end if,
        if
            NodeName <> "" and AttributeName <> "",
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(NodeName, { (_) })]) then
            else
                Node = xmlElement::new("", NodeName,GroupNode),
%                Node:name_P := NodeName,
                GroupNode:addNode(Node)
            end if,
            if _ = Node:attribute(AttributeName) then
                Node:modifyAttribute(AttributeName, NewValue)
            elseif NewValue <> "" then
                Node:addAttribute(AttributeName, NewValue)
            end if
        end if.

predicates
    onStreamModeSelectionChanged : listControl::selectionChangedListener.
clauses
    onStreamModeSelectionChanged(Source):-
        if
            Index = Source:tryGetSelectedIndex(),
            runTab_P:lbCodePage_P:setEnabled(toBoolean(1 /*ansi(codePage)*/ = Index)),
            IndexLB = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(IndexLB),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(sourcePerformer_C, { (_) })]) then
            else
                Node = xmlElement::new("",sourcePerformer_C, GroupNode),
%                Node:name_P := sourcePerformer_C,
                GroupNode:addNode(Node)
            end if,
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
        if
            Index = Source:tryGetSelectedIndex(),
            BodyCharset = Source:getAt(Index),
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(codePageList_F),
            IndexLB = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(IndexLB),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(sourcePerformer_C, { (_) })]) then
            else
                Node = xmlElement::new("", sourcePerformer_C,GroupNode),
%                Node:name_P := sourcePerformer_C,
                GroupNode:addNode(Node)
            end if,
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
                if Node:name_P = sourceEditor_C then
                    SourceEditor = if E = Node:attribute(fileName_C) then E else "" end if,
                    openTab_P:editorFileName_P:setText(SourceEditor),
                    PerformOn = toBoolean("true" = Node:attribute(feMode_C)),
                    openTab_P:cbFEMode_P:setChecked(PerformOn),
                    ArgEditor = if AE = Node:attribute(argEditor_C) then AE else "" end if,
                    openTab_P:editArgString_P:setText(ArgEditor)
                elseif Node:name_P = sourcePerformer_C then
                    ComponentName = if C = Node:attribute(componentName_C) then C else "" end if,
                    editComponentName_ctl:setText(ComponentName),
                    SourcePerformer = if S = Node:attribute(fileName_C) then S else "" end if,
                    runTab_P:editorFileName_P:setText(SourcePerformer),
                    ArgPerformer = if A = Node:attribute(argPerformer_C) then A else "" end if,
                    runTab_P:editArgStringForRun_P:setText(ArgPerformer),
                    ReArgPerformer = if RA = Node:attribute(reargPerformer_C) then RA else "" end if,
                    runTab_P:editArgStringForReRun_P:setText(ReArgPerformer),
                    SuffixPerformer = if SP = Node:attribute(suffixPerformer_C) then SP else "" end if,
                    runTab_P:editSuffix_P:setText(SuffixPerformer),
                    StreamModeIndex = if SMI = toTerm(positive, Node:attribute(streamMode_C)) then SMI else 0 end if,
                    runTab_P:lbSteramMode_P:selectAt(StreamModeIndex, true),
                    PerformOn = toBoolean("true" = Node:attribute(feMode_C)),
                    runTab_P:cbFEMode_P:setChecked(PerformOn),
                    if CP = Node:attribute(codePage_C),
                        list::memberIndex_nd(NV, CodePageIndex, codePageList_F),
                        NV = namedValue(CP, _)
                    then
                        runTab_P:lbCodePage_P:selectAt(CodePageIndex, true)
                    else
                        runTab_P:lbCodePage_P:selectAt(0, true)
                    end if
                elseif Node:name_P = sourceExecute_C then
                    CmdExec = if CEx = Node:attribute(cmdExecute_C) then CEx else "" end if,
                    execTab_P:editCmdString_P:setText(CmdExec),
                    ArgExec = if AEx = Node:attribute(argExecute_C) then AEx else "" end if,
                    execTab_P:editArgString_P:setText(ArgExec),
                    PerformOn = toBoolean("true" = Node:attribute(feMode_C)),
                    execTab_P:cbFEMode_P:setChecked(PerformOn),
                    ExecOn = toBoolean("true" = Node:attribute(execOn_C)),
                    execTab_P:cbExecutePossible_P:setChecked(ExecOn)
                elseif Node:name_P = "common" then
                    ExtListStr = if EL = Node:attribute(extName_C) then EL else "" end if,
                    editExtList_ctl:setText(ExtListStr)
                end if
            end foreach,
            updateResultString()
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
    newTemplateExtOptions_C : extOptions = [tuple(sourceEditor_C,[]),tuple(sourcePerformer_C,[]),tuple(sourceExecute_C,[])].
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
            else
                runTab_P:editorFileName_P:setText(""),
                runTab_P:editArgStringForRun_P:setText(""),
                runTab_P:editArgStringForReRun_P:setText(""),
                runTab_P:editSuffix_P:setText(""),
                openTab_P:editorFileName_P:setText(""),
                openTab_P:editArgString_P:setText(""),
                updateResultString()
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
            Node:name_P <> "common",
            AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]
            ].

% This code is maintained automatically, do not update it manually. 15:15:09-30.9.2017

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
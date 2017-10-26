%

implement performExt
    inherits dialog
    inherits wsFE_Connector
    open core, vpiDomains, ws_EventManager, xmlNavigate

constants
    editorFileName_id = 1000.
    browseEditor_id = editorFileName_id + 1.
    editArgString_id = browseEditor_id + 1.
    runFileName_id = editArgString_id + 1.
    browseRun_id = runFileName_id + 1.
    editSuffix_id = browseRun_id + 1.
    browseSuffix_id = editSuffix_id + 1.
    editArgStringForRun_id = browseSuffix_id + 1.
    editArgStringForReRun_id = editArgStringForRun_id + 1.
    execCmdString_id = editArgStringForReRun_id + 1.
    execArgString_id = execCmdString_id + 1.
    editComponentName_id = execArgString_id + 1.
    lbSteramMode_id = editComponentName_id + 1.
    lbCodePage_id = lbSteramMode_id + 1.
    getMacroSymbols_id = lbCodePage_id + 1.
    cbExecutePossible_id = getMacroSymbols_id + 1.

domains
    extOptions = tuple{string, namedValue*}*.

facts
    extOptionsList_P : namedValue*.

    openTab_P : openTab.
    runTab_P : runTab.
    execTab_P : execTab.

    xml_ExtOptions : (string Ext, xmlDocument).
    codePageList_F : namedValue* := [].
    addText_F : string := "".

clauses
    display(Parent, WS_FrontEnd, ExtOptionsList) = Dialog :-
        Dialog = new(Parent, WS_FrontEnd, ExtOptionsList),
        Dialog:show().

constructors
    new : (window Parent,ws_FrontEnd,namedValue* ExtOptionsList).
clauses
    new(Parent, WS_FrontEnd, ExtOptionsList) :-
        wsFE_Connector::new(WS_FrontEnd),
        dialog::new(Parent),
        generatedInitialize(),
        Page1 = tabPage::new(),
        openTab_P := openTab::new(Page1:getContainerControl()),
        Page1:setText(openTab_P:getText()),
        tabControl_ctl:addPage(Page1),
        Page2 = tabPage::new(),
        runTab_P := runTab::new(Page2:getContainerControl()),
        Page2:setText(runTab_P:getText()),
        tabControl_ctl:addPage(Page2),
        Page3 = tabPage::new(),
        execTab_P := execTab::new(Page3:getContainerControl()),
        Page3:setText(execTab_P:getText()),
        tabControl_ctl:addPage(Page3),
        setStreamModeValues(),
        setControlsID(),
        extOptionsList_P := ExtOptionsList,
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
    setControlsID : ().
clauses
    setControlsID():-
        openTab_P:editorFileName_P:setCtrlId(editorFileName_id),
        openTab_P:editArgString_P:setCtrlId(editArgString_id),
        openTab_P:browseEditor_P:setCtrlId(browseEditor_id),
        runTab_P:editorFileName_P:setCtrlId(runFileName_id),
        runTab_P:editArgStringForRun_P:setCtrlId(editArgStringForRun_id),
        runTab_P:editArgStringForReRun_P:setCtrlId(editArgStringForReRun_id),
        runTab_P:editSuffix_P:setCtrlId(editSuffix_id),
        runTab_P:browseEditor_P:setCtrlId(browseRun_id),
        editComponentName_ctl:setCtrlId(editComponentName_id),
        runTab_P:lbSteramMode_P:setCtrlId(lbSteramMode_id),
        runTab_P:lbCodePage_P:setCtrlId(lbCodePage_id),
        runTab_P:browseSuffix_P:setCtrlId(browseSuffix_id),
        execTab_P:editCmdString_P:setCtrlId(execCmdString_id),
        execTab_P:editArgString_P:setCtrlId(execArgString_id),
        execTab_P:getMacroSymbols_P:setCtrlId(getMacroSymbols_id),
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
        openTab_P:browseEditor_P:setClickResponder(onBrowseEditorClick),
        runTab_P:browseEditor_P:setClickResponder(onBrowseEditorClick),
        runTab_P:browseSuffix_P:setClickResponder(onBrowseEditorClick),
        runTab_P:lbSteramMode_P:addSelectionChangedListener(onStreamModeSelectionChanged),
        runTab_P:lbCodePage_P:addSelectionChangedListener(onCodePageSelectionChanged),
        execTab_P:editCmdString_P:addModifiedListener(onEditorModified),
        execTab_P:editArgString_P:addModifiedListener(onEditorModified),
        execTab_P:getMacroSymbols_P:setClickResponder(onGetMacroSymbolsClick),
        execTab_P:cbExecutePossible_P:addStateChangedListener(onCbExecutePossibleStateChanged).

predicates
    onCbExecutePossibleStateChanged : checkButton::stateChangedListener.
clauses
    onCbExecutePossibleStateChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        if
            xml_ExtOptions(_ExtName, Xml_Options),
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

constants
    menuTag0_id = getMacroSymbols_id + 1.
    menuTag1_id = menuTag0_id + 1.
    menuTag2_id = menuTag1_id + 1.
    menuTag3_id = menuTag2_id + 1.
    menuTag4_id = menuTag3_id + 1.
predicates
    onGetMacroSymbolsClick : button::clickResponder.
clauses
    onGetMacroSymbolsClick(_Source) = button::defaultAction :-
        rct(_X, Y, _, _) = execTab_P:mapRectangleTo(This, execTab_P:getRect()),
        rct(XC, _, _, YC) = execTab_P:getMacroSymbols_P:mapRectangleTo(execTab_P, execTab_P:getMacroSymbols_P:getRect()),
        menuPopUp(dynMenu(
            [
            txt(menuTag0_id, "Browse...", noAccelerator, 1, mis_none, []),
            txt(menuTag1_id, "$(SourceFile)", noAccelerator, 1, mis_none, []),
            txt(menuTag2_id, "$(SourceName)", noAccelerator, 1, mis_none, []),
            txt(menuTag4_id, "$(ExeName)", noAccelerator, 1, mis_none, []),
            txt(menuTag3_id, "$(SourceExeDir)", noAccelerator, 1, mis_none, [])
            ]), pnt(XC, Y+YC), align_Right).

predicates
    onMenuItem : window::menuItemListener.
clauses
    onMenuItem(_Source, Tag):-
        CurText = execTab_P:editCmdString_P:getText(),
        if Tag = menuTag0_id then
            CurrentDirectory = directory::getCurrentDirectory(),
            addText_F := "",
            if FileName = vpiCommonDialogs::getFileName
                ("", ["Executable File","*.exe","All files","*.*"], "File", [vpiDomains::dlgfn_filemustexist], CurrentDirectory, _SelectedFiles) then
                directory::setCurrentDirectory(CurrentDirectory),
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- addText_F := ShortFileName})
            end if,
            AddText = addText_F
        elseif Tag = menuTag1_id then AddText = "$(SourceFile)"
        elseif Tag = menuTag2_id then AddText = "$(SourceName)"
        elseif Tag = menuTag3_id then AddText = "$(SourceExeDir)"
        elseif Tag = menuTag4_id then AddText = "$(ExeName)"
        else AddText = ""
        end if,
        execTab_P:editCmdString_P:setText(string::concat(CurText, AddText)),
        updateAttribute(execTab_P:editCmdString_P),
        updateResultString().

predicates
    onBrowseEditorClick : button::clickResponder.
clauses
    onBrowseEditorClick(Source) = button::defaultAction :-
        ID = Source:getCtrlId(),
        CurrentDirectory = directory::getCurrentDirectory(),
        if browseSuffix_id = ID then
            StartExt = "*.*"
        else
            StartExt = "*.exe"
        end if,
        if FileName = vpiCommonDialogs::getFileName
            (StartExt, ["Executable File","*.exe","All files","*.*"], "File", [vpiDomains::dlgfn_filemustexist], CurrentDirectory, _SelectedFiles) then
            directory::setCurrentDirectory(CurrentDirectory),
            if browseEditor_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(openTab_P:editorFileName_P, ShortFileName)})
            elseif browseRun_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(runTab_P:editorFileName_P, ShortFileName)})
            elseif browseSuffix_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(runTab_P:editSuffix_P, ShortFileName)})
            end if
       end if.

predicates
    updateEditorControl : (editControl, string).
clauses
    updateEditorControl(EditControl, ShortFileName):-
        EditControl:setText(ShortFileName),
        updateAttribute(EditControl),
        updateResultString().

predicates
    onEditorModified : editControl::modifiedListener.
clauses
    onEditorModified(Source):-
        updateAttribute(Source),
        updateResultString().

predicates
    updateResultString : ().
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

predicates
    updateAttribute : (editControl Source).
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
            xml_ExtOptions(_ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(NodeName, { (_) })]) then
            else
                Node = xmlElement::new("",NodeName, GroupNode),
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
            xml_ExtOptions(_ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if Node = Xml_Options:getNode_nd([current(GroupNode), child(sourcePerformer_C, { (_) })]) then
            else
                Node = xmlElement::new("", sourcePerformer_C,GroupNode),
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
            xml_ExtOptions(_ExtName, Xml_Options),
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

facts
    newExtOptionsList : namedValue* := [].
predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::defaultAction :-
        foreach namedValue(ExtName, string(ExtOptionsStr)) in extOptionsList_P do
            if xml_ExtOptions(ExtName, Xml_Options) then
            else
                Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
            end if,
            OptionsValues = [tuple(Node:name_P, AttributesList)||
                Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("*", { (_) })]),
                if Node:name_P = "common", ExtListStr = Node:attribute(extName_C) then
                    newExtOptionsList := list::append(newExtOptionsList, [namedValue(ExtName, string(ExtListStr))]),
                    fail
                else
                    AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]
                end if
                ],
            notify(methodRequest,ws_EventManager::updateExtOptions_C, [namedValue(ExtName, string(toString(OptionsValues)))])
        end foreach,
        notify(methodRequest,ws_EventManager::updateExtOptionsList_C, newExtOptionsList).

% This code is maintained automatically, do not update it manually. 15:51:57-22.8.2016

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    tabControl_ctl : tabcontrol.
    editComponentName_ctl : editControl.
    staticText5_ctl : textControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Source Type Settings"),
        setRect(rct(50, 40, 314, 243)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        addMenuItemListener(onMenuItem),
        editComponentName_ctl := editControl::new(This),
        editComponentName_ctl:setText("Edit"),
        editComponentName_ctl:setPosition(72, 6),
        editComponentName_ctl:setWidth(188),
        editComponentName_ctl:setReadOnly(),
        tabControl_ctl := tabcontrol::new(This),
        tabControl_ctl:setPosition(4, 20),
        tabControl_ctl:setSize(257, 164),
        tabControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(108, 188),
        ok_ctl:setSize(48, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(161, 188),
        cancel_ctl:setSize(48, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(213, 188),
        help_ctl:setSize(48, 12),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        staticText5_ctl := textControl::new(This),
        staticText5_ctl:setText("Component Name:"),
        staticText5_ctl:setPosition(4, 7),
        staticText5_ctl:setSize(64, 10).
% end of automatic code
end implement performExt
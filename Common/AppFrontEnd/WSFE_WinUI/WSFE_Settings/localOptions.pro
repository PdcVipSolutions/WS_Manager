%

implement localOptions
    inherits dialog
    inherits wsFE_Connector
    open core, vpiDomains, ws_eventManager, xmlNavigate

domains
    extOptions = tuple{string, namedValue*}*.

facts
    localOptions_F : extOptions := [].
    xmlOptions_F : xmlDocument := erroneous.

    xml_LocalOptions : (string ExtName,xmlDocument Xml_Options).

    addText_F : string := "".
    macroSymbols : (string MacroName, string Value).

clauses
    display(Parent, WS_FrontEnd, LocalOptions, FileName, XmlOptions, VirtualDirList) = Dialog :-
        Dialog = new(Parent, WS_FrontEnd, LocalOptions, FileName, XmlOptions, VirtualDirList),
        Dialog:show().

clauses
    new(Parent, WS_FrontEnd, LocalOptions, FileName, XmlOptions, VirtualDirList) :-
        wsFE_Connector::new(WS_FrontEnd),
        dialog::new(Parent),
        generatedInitialize(),
        commandTab_ctl:gbResultStr_P:setPosition(0, 128),
        commandTab_ctl:gbResultStr_P:setHeight(90),

        commandTab_ctl:cbFEMode_P:setVisible(false),
        commandTab_ctl:cbDefCommand_P:setVisible(false),
        commandTab_ctl:cbCheckStatus_P:setVisible(false),
        commandTab_ctl:cbCallAssociations_P:setVisible(false),
        commandTab_ctl:cbPossibleAll_P:setVisible(false),

        commandTab_ctl:setStreamModeValues(),
        commandTab_ctl:setControlsID(),
        pbClearOptions_ctl:setText(ws_Events():getString(ttlClear_pb)),
        TitleDialogList = getDialogTitleList("localOptions"),
        commandTab_ctl:addControlsText(TitleDialogList),
        setText(string::concat("Local options <", FileName, ">")),
        retractAll(macroSymbols(_, _)),
        foreach
            namedValue(Name, string(ValueStr)) in VirtualDirList,
            tuple(VirtualDir, _) = tryToTerm(tuple{string, boolean}, ValueStr)
        do
            assert(macroSymbols(Name, VirtualDir))
        end foreach,
        FullFileName = parseCommandLine(FileName),
        assert(macroSymbols("$(SourceFile)", FullFileName)),
        initLocalOptions(LocalOptions, XmlOptions).

clauses
    initLocalOptions(LocalOptions, XmlOptions):-
        LocalOptionsList = toTerm(extOptions, LocalOptions),
        localOptions_F := LocalOptionsList,
        xmlOptions_F := XmlOptions,
        NameList = [Name ||
            Node = XmlOptions:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, {(_)})]),
            Name = Node:attribute(name_C),
            initLocalOptionsXml(Name, Node:attribute(index_C), Node)
            ],
        lbCommandName_ctl:addList(NameList),
        lbCommandName_ctl:selectAt(0, true).

predicates
    getDialogTitleList : (string) -> namedValue*.
clauses
    getDialogTitleList("addErrorWarning") =
        [
        namedValue("pbAdd", string(ws_Events():getString(ttlAdd_pb))),
        namedValue("pbDelete", string(ws_Events():getString(ttlDelete_pb))),
        namedValue("pbNew", string(ws_Events():getString(ttlNew_pb))),
        namedValue("pbEdit", string(ws_Events():getString(ttlEdit_pb))),
        namedValue("txtDeleteGroup", string(ws_Events():getString(delKWGroupDir))),
        namedValue("stGroupName", string(ws_Events():getString(ttlGroupName_st))),
        namedValue("txtFormatAddWord", string(ws_Events():getString(txtFormatAddWord))),
        namedValue("txtFormatEditWord", string(ws_Events():getString(txtFormatEditWord))),
        namedValue("txtPromptWord", string(ws_Events():getString(txtPromptWord))),
        namedValue("ttlWarning", string(ws_Events():getString(ttlWarning)))
        ] :-!.
    getDialogTitleList(_) =
        [
        namedValue("pbBrowse", string(ws_Events():getString(ttlBrowse_pb))),
        namedValue("txtArguments", string(ws_Events():getString(txtArguments))),
        namedValue("txtFormatCmd", string(ws_Events():getString(txtFormatCmd))),
        namedValue("txtPerformFE", string(ws_Events():getString(txtPerformFE))),
        namedValue("txtDefCommand", string(ws_Events():getString(txtDefCommandFE))),
        namedValue("txtCommandName", string(ws_Events():getString(txtCommandName))),

        namedValue("txtApplicationFile", string(ws_Events():getString(txtApplicationFile))),

        namedValue("txtCodePage", string(ws_Events():getString(txtCodePage))),
        namedValue("txtStreamMode", string(ws_Events():getString(txtStreamMode))),
        namedValue("cbStreamMode", string(ws_Events():getString(cbStreamMode))),
        namedValue("txtSuffix", string(ws_Events():getString(txtSuffix))),
        namedValue("cbPossibleAll", string(ws_Events():getString(cbPossibleAll))),
        namedValue("cbCheckStatus", string(ws_Events():getString(cbCheckStatus))),
        namedValue("cbInvokeWinAss", string(ws_Events():getString(cbInvokeWinAss))),
        namedValue("stInputStream", string(ws_Events():getString(stInputStream))),
        namedValue("gbInputStream", string(ws_Events():getString(gbInputStream))),
        namedValue("gbOutputStream", string(ws_Events():getString(gbOutputStream))),
        namedValue("pbKeywords", string(ws_Events():getString(pbKeywords)))
        ].

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data):-
        addControlsListners(commandTab_ctl).

clauses
    addControlsListners(CommandTab):-
        CommandTab:applicationFileName_P:addModifiedListener(onEditorModified),
        CommandTab:editArguments_P:addModifiedListener(onEditorModified),
        CommandTab:editFormatCommand_P:addModifiedListener(onEditorModified),
        CommandTab:editSuffix_P:addModifiedListener(onEditorModified),
        CommandTab:edInputStreamFile_P:addModifiedListener(onEditorModified),
        CommandTab:cbStreamMode_P:addStateChangedListener(onStreamModeChanged),
        CommandTab:lbCodePage_P:addSelectionChangedListener(onCodePageSelectionChanged),
        CommandTab:lbInputEncoding_P:addSelectionChangedListener(onCodePageSelectionChanged),
        CommandTab:getMacroSymbols_P:setClickResponder(onGetMacroSymbolsClick),
        CommandTab:browseEditor_P:setClickResponder(onBrowseEditorClick),
        CommandTab:browseSuffix_P:setClickResponder(onBrowseEditorClick),
        CommandTab:browseInputFile_P:setClickResponder(onBrowseEditorClick),
        CommandTab:pbSetWords_P:setClickResponder(onSetWordsClick),
        !.

predicates
    initLocalOptionsXml : (string ExtName,string CmdIndex,xmlElement Node).
clauses
    initLocalOptionsXml(ExtName, CmdIndex, Node):-
        Xml_Options=xmlDocument::new(ExtName),
        Xml_Options:codePage_P:=utf8,
        Xml_Options:indent_P:=true,
        Xml_Options:xmlStandalone_P:=xmlLite::yes,
        assert(xml_LocalOptions(ExtName, Xml_Options)),
        CmdNode = xmlElement::new("", command_C, Xml_Options:root_P),
        Xml_Options:root_P:addNode(CmdNode),
        if tuple(_, AttributesList) = list::tryGetMemberEq({(N, tuple(N, _))}, CmdIndex, localOptions_F) then
            foreach
                namedValue(AttrName, string(AttrValue)) in AttributesList,
                AttrValue <> Node:attribute(AttrName) otherwise ""
            do
                CmdNode:addAttribute(AttrName, AttrValue)
            end foreach
        end if.
predicates
    onCodePageSelectionChanged : listControl::selectionChangedListener.
clauses
    onCodePageSelectionChanged(Source):-
        ID = Source:getCtrlId(),
        Index = Source:tryGetSelectedIndex(),
        AttrName = if ID = commandTab::lbCodePage_id then codePage_C else inputCP_C end if,
        _Node = tryGetSelectNode(Xml_LocalOptions),
        !,
        if CmdNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]) then
        else
            CmdNode = xmlElement::new("", command_C, Xml_LocalOptions:root_P),
            Xml_LocalOptions:root_P:addNode(CmdNode)
        end if,
        BodyCharset = Source:getAt(Index),
        if
            equal = string::compareIgnoreCase(BodyCharset, "unicode"),
            AttrName = codePage_C
        then
            if _ = CmdNode:attribute(streamMode_C) then
                CmdNode:modifyAttribute(streamMode_C, toString(0)) % set Unicode Stream Mode
            else
                CmdNode:addAttribute(streamMode_C, toString(0))
            end if
        elseif
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(commandTab_ctl:codePageList_P),
            AttrName = codePage_C
        then
            if _ = CmdNode:attribute(streamMode_C) then
                CmdNode:modifyAttribute(streamMode_C, toString(1)) % set Ansi Stream Mode
            else
                CmdNode:addAttribute(streamMode_C, toString(1))
            end if,
            if _ = CmdNode:attribute(AttrName) then
                CmdNode:modifyAttribute(AttrName, CodePage)
            else
                CmdNode:addAttribute(AttrName, CodePage)
            end if
        elseif
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(commandTab_ctl:codePageList_P)
        then
            if _ = CmdNode:attribute(AttrName) then
                CmdNode:modifyAttribute(AttrName, CodePage)
            else
                CmdNode:addAttribute(AttrName, CodePage)
            end if
        end if.
    onCodePageSelectionChanged(_Source).

predicates
    onLbCommandNameSelectionChanged : listControl::selectionChangedListener.
clauses
    onLbCommandNameSelectionChanged(_Source):-
        if Node = tryGetSelectNode(Xml_LocalOptions) then
            if LocalNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]) then
                StreamModeOn = toBoolean("true" = (LocalNode:attribute(streamModeOn_C) otherwise (Node:attribute(streamModeOn_C) otherwise "false"))),
                CP = LocalNode:attribute(codePage_C) otherwise (Node:attribute(codePage_C) otherwise "1200"),
                ICP = LocalNode:attribute(inputCP_C) otherwise (Node:attribute(inputCP_C) otherwise "1200"),
                FormatCommand = LocalNode:attribute(formatCmd_C) otherwise (Node:attribute(formatCmd_C) otherwise ""),
                ApplicationFN = LocalNode:attribute(fileName_C) otherwise (Node:attribute(fileName_C) otherwise ""),
                ArgumentStr = LocalNode:attribute(argument_C) otherwise (Node:attribute(argument_C) otherwise ""),
                SuffixStr = LocalNode:attribute(suffix_C) otherwise (Node:attribute(suffix_C) otherwise ""),
                InputFile = LocalNode:attribute(inputFile_C) otherwise (Node:attribute(inputFile_C) otherwise "")
            else
                StreamModeOn = toBoolean("true" = Node:attribute(streamModeOn_C)),
                CP = Node:attribute(codePage_C) otherwise "1200",
                ICP = Node:attribute(inputCP_C) otherwise "1200",
                FormatCommand = Node:attribute(formatCmd_C) otherwise "",
                ApplicationFN = Node:attribute(fileName_C) otherwise "",
                ArgumentStr = Node:attribute(argument_C) otherwise "",
                SuffixStr = Node:attribute(suffix_C) otherwise "",
                InputFile = Node:attribute(inputFile_C) otherwise ""
            end if,
            commandTab_ctl:cbStreamMode_P:setChecked(StreamModeOn),
            commandTab_ctl:lbCodePage_P:setEnabled(StreamModeOn),
            if
                list::memberIndex_nd(NV, CodePageIndex, commandTab_ctl:codePageList_P),
                NV = namedValue(CP, _)
            then
                commandTab_ctl:lbCodePage_P:selectAt(CodePageIndex, true)
            else
                commandTab_ctl:lbCodePage_P:selectAt(0, true)
            end if,
            if
                list::memberIndex_nd(INV, ICodePageIndex, commandTab_ctl:codePageList_P),
                INV = namedValue(ICP, _)
            then
                commandTab_ctl:lbInputEncoding_P:selectAt(ICodePageIndex, true)
            else
                commandTab_ctl:lbInputEncoding_P:selectAt(0, true)
            end if,
            commandTab_ctl:editFormatCommand_P:setText(FormatCommand),
            commandTab_ctl:applicationFileName_P:setText(ApplicationFN),
            commandTab_ctl:editArguments_P:setText(ArgumentStr),
            commandTab_ctl:editSuffix_P:setText(SuffixStr),
            commandTab_ctl:edInputStreamFile_P:setText(InputFile)
        end if,
        updateResultString().

predicates
    onEditorModified : editControl::modifiedListener.
clauses
    onEditorModified(Source):-
        updateAttribute(Source),
        updateResultString().

predicates
    updateAttribute : (editControl Source).
clauses
    updateAttribute(Source):-
        ID = Source:getCtrlId(),
        NewValue = string::trim(Source:getText()),
        Node = tryGetSelectNode(Xml_LocalOptions),
        !,
        if CmdNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]) then
        else
            CmdNode = xmlElement::new("", command_C, Xml_LocalOptions:root_P),
            Xml_LocalOptions:root_P:addNode(CmdNode)
        end if,
        if commandTab::editorFileName_id = ID then AttributeName = fileName_C
        elseif commandTab::editFormatCommand_id = ID then AttributeName = formatCmd_C
        elseif commandTab::editArgString_id = ID then AttributeName = argument_C
        elseif commandTab::editSuffix_id = ID then AttributeName = suffix_C
        elseif commandTab::edInputStreamFile_id = ID then AttributeName = inputFile_C
        else AttributeName = ""
        end if,
        if AttributeName <> "" then
            if _ = CmdNode:attribute(AttributeName) then
                if NewValue <> (Node:attribute(AttributeName) otherwise "") then
                    CmdNode:modifyAttribute(AttributeName, NewValue)
                else
                    CmdNode:removeAttribute(AttributeName)
                end if
            elseif NewValue <> (Node:attribute(AttributeName) otherwise "") then
                CmdNode:addAttribute(AttributeName, NewValue)
            end if
        end if.
    updateAttribute(_Source).

predicates
    onStreamModeChanged : checkButton::stateChangedListener.
clauses
    onStreamModeChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        CommandTab = commandTab_ctl,
        CommandTab:lbCodePage_P:setEnabled(Source:getChecked()),
        if Node = tryGetSelectNode(Xml_LocalOptions) then
            if CmdNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]) then
            else
                CmdNode = xmlElement::new("", command_C, Xml_LocalOptions:root_P),
                Xml_LocalOptions:root_P:addNode(CmdNode)
            end if,
            if _ = CmdNode:attribute(streamModeOn_C) then
                if NewValue <> (Node:attribute(streamModeOn_C) otherwise "false") then
                    CmdNode:modifyAttribute(streamModeOn_C, NewValue)
                else
                    CmdNode:removeAttribute(streamModeOn_C)
                end if
            elseif NewValue <> (Node:attribute(streamModeOn_C) otherwise "false") then
                CmdNode:addAttribute(streamModeOn_C, NewValue)
            end if
        end if.

predicates
    tryGetSelectNode : (xmlDocument Xml_LocalOptions [out]) -> xmlElement Node determ.
clauses
    tryGetSelectNode(Xml_LocalOptions) = Node :-
        Index = lbCommandName_ctl:tryGetSelectedIndex(),
        Name = lbCommandName_ctl:getAt(Index),
        Node = xmlOptions_F:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, {(CN) :- Name = CN:attribute(name_C)})]),
        xml_LocalOptions(Name, Xml_LocalOptions),
        !.

predicates
    onSetWordsClick : button::clickResponder.
clauses
    onSetWordsClick(_Source) = button::defaultAction :-
        if
            Node = tryGetSelectNode(Xml_LocalOptions),
            LocalNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]),
            KeyWords = LocalNode:attribute(keyWords_C) otherwise Node:attribute(keyWords_C),
            WordsList = toTerm(KeyWords),
            TitleList = getDialogTitleList("addErrorWarning"),
            some(NewWordsList) = addErrorWarningWords::display(This, WordsList, TitleList)
        then
            if _ = LocalNode:attribute(keyWords_C) then
                LocalNode:modifyAttribute(keyWords_C, toString(NewWordsList))
            else
                LocalNode:addAttribute(keyWords_C, toString(NewWordsList))
            end if
        end if.

constants
    menuTag0_id = commandTab::cbPossibleAll_id + 1.
    menuTag1_id = menuTag0_id + 1.
    menuTag2_id = menuTag1_id + 1.
    menuTag3_id = menuTag2_id + 1.
    menuTag4_id = menuTag3_id + 1.
    menuTag5_id = menuTag4_id + 1.
    menuTag6_id = menuTag5_id + 1.
    menuTag7_id = menuTag6_id + 1.

predicates
    onGetMacroSymbolsClick : button::clickResponder.
clauses
    onGetMacroSymbolsClick(_Source) = button::defaultAction :-
        rct(XC, _, _, YC) = commandTab_ctl:getMacroSymbols_P:getOuterRect(),
        menuPopUp(dynMenu(
            [
            txt(menuTag0_id, ws_Events():getString(ttlBrowse_pb), noAccelerator, 1, mis_none, []),
            separator,
            txt(menuTag5_id, "[Application]", noAccelerator, 1, mis_none, []),
            txt(menuTag6_id, "[Arguments]", noAccelerator, 1, mis_none, []),
            txt(menuTag7_id, "[Suffix]", noAccelerator, 1, mis_none, []),
            separator,
            txt(menuTag1_id, "$(SourceFile)", noAccelerator, 1, mis_none, []),
            txt(menuTag2_id, "$(SourceName)", noAccelerator, 1, mis_none, []),
            txt(menuTag4_id, "$(ExeName)", noAccelerator, 1, mis_none, []),
            txt(menuTag3_id, "$(SourceExeDir)", noAccelerator, 1, mis_none, [])
            ]), pnt(XC+40, YC+44), align_Right). % BB!!!

predicates
    onMenuItem : window::menuItemListener.
clauses
    onMenuItem(_Source, Tag):-
        CurText = commandTab_ctl:editFormatCommand_P:getText(),
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
        elseif Tag = menuTag5_id then AddText = "[Application]"
        elseif Tag = menuTag6_id then AddText = "[Arguments]"
        elseif Tag = menuTag7_id then AddText = "[Suffix]"
        else AddText = ""
        end if,
        commandTab_ctl:editFormatCommand_P:setText(string::concat(CurText, AddText)),
        updateAttribute(commandTab_ctl:editFormatCommand_P),
        updateResultString().

predicates
    onBrowseEditorClick : button::clickResponder.
clauses
    onBrowseEditorClick(Source) = button::defaultAction :-
        ID = Source:getCtrlId(),
        CurrentDirectory = directory::getCurrentDirectory(),
        if commandTab::browseSuffix_id = ID then
            StartExt = "*.*"
        elseif commandTab::browseInputFile_id = ID then
            StartExt = "*.txt"
        else
            StartExt = "*.exe"
        end if,
        CommandTab = commandTab_ctl,
        if FileName = vpiCommonDialogs::getFileName
            (StartExt, ["Executable File","*.exe","Text File","*.txt","All files","*.*"], "File", [vpiDomains::dlgfn_filemustexist], CurrentDirectory, _SelectedFiles) then
            directory::setCurrentDirectory(CurrentDirectory),
            if commandTab::browseSuffix_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab:editSuffix_P, ShortFileName)})
            elseif commandTab::browseInputFile_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab:edInputStreamFile_P, ShortFileName)})
            else
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab:applicationFileName_P, ShortFileName)})
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
    updateResultString : ().
clauses
    updateResultString():-
        FC = commandTab_ctl:editFormatCommand_P:getText(),
        FC1 = string::replaceAll(FC, "[Application]", commandTab_ctl:applicationFileName_P:getText()),
        FC2 = string::replaceAll(FC1, "[Arguments]", commandTab_ctl:editArguments_P:getText()),
        FC3 = string::replaceAll(FC2, "[Suffix]", commandTab_ctl:editSuffix_P:getText()),
        FullCommandLine = parseCommandLine(FC3),
        WrapList = wrapString(FullCommandLine, ""),
        CommandLine = string::concatWithDelimiter(WrapList, "\n"),
        commandTab_ctl:resultCommandLine_P:setText(CommandLine),
        !.

predicates
    wrapString : (string Input,string Start) -> string* WrapList.
clauses
    wrapString(Input, Start) = Result :-
        string::frontToken(Input, Token, RestStr),
        !,
        First = string::frontChar(RestStr),
        if 74 > string::length(string::concat(Start, Token)) then
            Result = if ' ' = First then wrapString(RestStr, string::concat(Start, Token, " ")) else wrapString(RestStr, string::concat(Start, Token)) end if
        else
            Result = [Start | if ' ' = First then wrapString(RestStr, string::concat(Token, " ")) else wrapString(RestStr, Token) end if]
        end if.
    wrapString(_, Rest) = [Rest].

predicates
    parseCommandLine : (string InputStr) -> string.
clauses
    parseCommandLine(InputStr) = string::concat(HeadStr, ParseStr, OutStr) :-
        string::splitStringBySeparators(InputStr, "$", HeadStr, _, RestStr),
        string::splitStringBySeparators(RestStr, ")", VirtName, _, Rest),
        ParseStr = changeMacroSymbol(string::concat("$", VirtName, ")")),
        !,
        OutStr = parseCommandLine(Rest).
    parseCommandLine(Rest) = Rest.

predicates
    changeMacroSymbol : (string MacroSymbol) -> string ParseStr.
clauses
    changeMacroSymbol(MacroName) = MacroValue :-
        macroSymbols(MacroName, MacroValue),
        !.
    changeMacroSymbol("$(SourceExeDir)") = FullExeDir :-
        macroSymbols("$(SourceFile)", SourceFile),
        FullExeDir = filename::getPath(SourceFile),
        assert(macroSymbols("$(SourceExeDir)", FullExeDir)),
        !.
    changeMacroSymbol("$(SourceName)") = FileName :-
        macroSymbols("$(SourceFile)", SourceFile),
        FileName = filename::getName(SourceFile),
        assert(macroSymbols("$(SourceName)", FileName)),
        !.
    changeMacroSymbol("$(ExeName)") = ExeName :-
        macroSymbols("$(SourceFile)", SourceFile),
        FileName = filename::getName(SourceFile),
        ExeName = filename::setExtension(FileName, "exe"),
        assert(macroSymbols("$(ExeName)", ExeName)),
        !.
%    changeMacroSymbol(WSBE_Options, MacroSymbol) = ParseStr :-
%        ParseStr = WSBE_Options:getFullFileName(MacroSymbol),
%        not(string::equalIgnoreCase(ParseStr, MacroSymbol)),
%        !.
    changeMacroSymbol(String) = String.

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::defaultAction :-
        updateLocalOptions().

predicates
    updateLocalOptions : ().
clauses
    updateLocalOptions():-
        if [SourceID] = wsFE_SourceList():sourceList_P:getSel() then
            LocalOptionsList = [tuple(Node:attribute(index_C), NameValueList) ||
                xml_LocalOptions(Name, Xml_LocalOptions),
                Node = xmlOptions_F:getNode_nd([root(), child(groupNode_C, { (_) }), child(command_C, {(CN) :- Name = CN:attribute(name_C)})]),
                NameValueList = [ namedValue(AttrName, string(AttrValue)) ||
                    CmdNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})]),
                    tuple(_, AttrName, AttrValue) = CmdNode:getAttribute_nd()]
                ],
            wsFE_Tasks():saveLocalOptions([namedValue(toString(SourceID), string(toString(LocalOptionsList)))]),
            wsFE_Form():setLocalExtOptionsList([namedValue(toString(SourceID), string(toString(LocalOptionsList)))])
        end if.

predicates
    onPbClearOptionsClick : button::clickResponder.
clauses
    onPbClearOptionsClick(_Source) = button::defaultAction :-
        if
            _Node = tryGetSelectNode(Xml_LocalOptions),
            CmdNode = Xml_LocalOptions:getNode_nd([root(), child(command_C, {(_)})])
        then
            foreach tuple(_, AttrName, _) = CmdNode:getAttribute_nd() and AttrName <> index_C do
                CmdNode:removeAttribute(AttrName)
            end foreach,
            onLbCommandNameSelectionChanged(lbCommandName_ctl)
        end if.

% This code is maintained automatically, do not update it manually.
%  14:20:23-24.12.2018

facts
    ok_ctl : button.
    cancel_ctl : button.
    lbCommandName_ctl : listButton.
    stCommand_ctl : textControl.
    commandTab_ctl : commandtab.
    pbClearOptions_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("localOptions"),
        setRect(rct(50, 40, 346, 300)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        addMenuItemListener(onMenuItem),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(169, 244),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(233, 244),
        cancel_ctl:setSize(56, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        lbCommandName_ctl := listButton::new(This),
        lbCommandName_ctl:setPosition(72, 6),
        lbCommandName_ctl:setWidth(216),
        lbCommandName_ctl:setSort(false),
        lbCommandName_ctl:addSelectionChangedListener(onLbCommandNameSelectionChanged),
        commandTab_ctl := commandtab::new(This),
        commandTab_ctl:setPosition(8, 22),
        commandTab_ctl:setSize(280, 218),
        stCommand_ctl := textControl::new(This),
        stCommand_ctl:setText("Operation name: "),
        stCommand_ctl:setPosition(8, 7),
        stCommand_ctl:setSize(60, 10),
        pbClearOptions_ctl := button::new(This),
        pbClearOptions_ctl:setText("Clear"),
        pbClearOptions_ctl:setPosition(8, 244),
        pbClearOptions_ctl:setSize(56, 12),
        pbClearOptions_ctl:defaultHeight := false,
        pbClearOptions_ctl:setClickResponder(onPbClearOptionsClick).
% end of automatic code
end implement localOptions
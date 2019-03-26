%

implement performExt2
    inherits userControlSupport
    open core, vpiDomains, ws_EventManager, xmlNavigate

facts
    extOptionsList_P : namedValue*.
    performGroupList_P : namedValue*.

    xml_ExtOptions : (string Ext, xmlDocument).
    formatResultStr_f : string := "".
    reOrder_F : (menuTag, string From, string To).

    optionsDialog : wsfe_Settings.

constants
    indexList_C : string* = ["1", "2", "3", "4"].

clauses
    new(Wsfe_Settings, Parent, SelectSourceType, ExtOptionsList, PerformGroupList):-
        userControlSupport::new(),
        generatedInitialize(),
        setContainer(Parent),
        optionsDialog := Wsfe_Settings,
        TitleDialogList = optionsDialog:getDialogTitleList("performExtCtl"),
%
        addExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbAdd")),
        delExt_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbDelete")),
        staticText1_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtValue")),
        staticText5_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtCmpName")),
        staticText_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtName")),
        pbReOrder_ctl:setText(namedValue::getNamed_string(TitleDialogList, "pbReOrder")),
        stCommand_ctl:setText(string::concat(namedValue::getNamed_string(TitleDialogList, "stCommand"), " #:")),
        txtCommandName_ctl:setText(namedValue::getNamed_string(TitleDialogList, "txtCommandName")),
        formatResultStr_f := namedValue::getNamed_string(TitleDialogList, "txtResultStr"),
        ExpertTypeList = [Synname || namedValue(Synname, _) in PerformGroupList],
        lbExpertType_ctl:addList([""|ExpertTypeList]),
%
        commandTab_ctl:setStreamModeValues(),
        commandTab_ctl:setControlsID(),
        editCommandName_ctl:setCtrlId(commandTab::editCommandName_id),
        commandTab_ctl:addControlsText(TitleDialogList),
        addControlsListners(commandTab_ctl),
%
        lbCommandIndex_ctl:addList(indexList_C),
        lbCommandIndex_ctl:selectAt(0, true),
        extOptionsList_P := ExtOptionsList,
        performGroupList_P := PerformGroupList,
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

clauses
    extLBox_P() = extLBox_ctl.
    commandTab_P() = commandTab_ctl.
    pbReOrder_P() = pbReOrder_ctl.
    lbCommandIndex_P() = lbCommandIndex_ctl.
    stCommand_P() = stCommand_ctl.

predicates
    addControlsListners : (commandTab CommandTab).
clauses
    addControlsListners(CommandTab):-
        CommandTab:applicationFileName_P:addModifiedListener(onEditorModified),
        CommandTab:editArguments_P:addModifiedListener(onEditorModified),
        CommandTab:editFormatCommand_P:addModifiedListener(onEditorModified),
        CommandTab:edInputStreamFile_P:addModifiedListener(onEditorModified),
        editCommandName_ctl:addModifiedListener(onCmdNameEditorModified),
        editCommandName_ctl:addLoseFocusListener(onCmdNameEditorLoseFocus),
        CommandTab:editSuffix_P:addModifiedListener(onEditorModified),
        CommandTab:cbStreamMode_P:addStateChangedListener(onStreamModeChanged),
        CommandTab:lbCodePage_P:addSelectionChangedListener(onCodePageSelectionChanged),
        CommandTab:lbInputEncoding_P:addSelectionChangedListener(onCodePageSelectionChanged),
        CommandTab:cbFEMode_P:addStateChangedListener(onCbPerformStateChanged),
        CommandTab:getMacroSymbols_P:setClickResponder(optionsDialog:onGetMacroSymbolsClick),
        CommandTab:cbCallAssociations_P:addStateChangedListener(onCbCallAssociationsStateChanged),
        CommandTab:cbDefCommand_P:addStateChangedListener(onCbDefaultCommandChanged),
        CommandTab:cbCheckStatus_P:addStateChangedListener(onCbCheckStatusChanged),
        CommandTab:cbPossibleAll_P:addStateChangedListener(onCbPossibleAllChanged),
        CommandTab:browseEditor_P:setClickResponder(optionsDialog:onBrowseEditorClick),
        CommandTab:browseSuffix_P:setClickResponder(optionsDialog:onBrowseEditorClick),
        CommandTab:browseInputFile_P:setClickResponder(optionsDialog:onBrowseEditorClick),
        CommandTab:pbSetWords_P:setClickResponder(onSetWordsClick),
        pbReOrder_ctl:setClickResponder(optionsDialog:onPbReOrderClick).

predicates
    setEnabledControls : (boolean Enable).
clauses
    setEnabledControls(Enable):-
        commandTab_ctl:applicationFileName_P:setEnabled(Enable),
        commandTab_ctl:editArguments_P:setEnabled(Enable),
        commandTab_ctl:browseEditor_P:setEnabled(Enable),
        commandTab_ctl:editSuffix_P:setEnabled(Enable),
        commandTab_ctl:cbStreamMode_P:setEnabled(Enable),
        EnableSM = boolean::logicalAnd(Enable, commandTab_ctl:cbStreamMode_P:getChecked()),
        commandTab_ctl:lbCodePage_P:setEnabled(EnableSM),
        commandTab_ctl:lbInputEncoding_P:setEnabled(Enable),
        commandTab_ctl:edInputStreamFile_P:setEnabled(Enable),
        commandTab_ctl:browseInputFile_P:setEnabled(Enable),
        commandTab_ctl:browseSuffix_P:setEnabled(Enable),
        commandTab_ctl:getMacroSymbols_P:setEnabled(Enable),
        commandTab_ctl:editFormatCommand_P:setEnabled(Enable).

predicates
    onCbCallAssociationsStateChanged : checkButton::stateChangedListener.
clauses
    onCbCallAssociationsStateChanged(Source, _OldState, _NewState):-
        if false = Source:getChecked() then
            updateCheckAttribute(winAss_C, "false")
        else
            checkAndUpdate(Source, winAss_C)
        end if,
        setEnabledControls(boolean::logicalNot(Source:getChecked())).

predicates
    onCbDefaultCommandChanged : checkButton::stateChangedListener.
clauses
    onCbDefaultCommandChanged(Source, _OldState, _NewState):-
        if false = Source:getChecked() then
            updateCheckAttribute(defCommand_C, "false")
        else
            checkAndUpdate(Source, defCommand_C)
        end if.

predicates
    onCbCheckStatusChanged : checkButton::stateChangedListener.
clauses
    onCbCheckStatusChanged(Source, _OldState, _NewState):-
        updateCheckAttribute(checkStatus_C, toString(Source:getChecked())).

predicates
    onCbPossibleAllChanged : checkButton::stateChangedListener.
clauses
    onCbPossibleAllChanged(Source, _OldState, _NewState):-
        updateCheckAttribute(allPossible_C, toString(Source:getChecked())).

clauses
    tryCreatePopUp(MenuTagList) = PopUpItems :-
        Index = extLBox_ctl:tryGetSelectedIndex(),
        ExtName = extLBox_ctl:getAt(Index),
        CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
        CmdID = lbCommandIndex_ctl:getAt(CmdIndex),
        CmdName = editCommandName_ctl:getText(),
        xml_ExtOptions(ExtName, Xml_Options),
        GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
        !,
        RestList = list::remove(indexList_C, CmdID),
        retractAll(reOrder_F(_, _, _)),
        PopUpItems = [txt(MenuTag, ItemTitle, noAccelerator, 1, mis_none, [])||
            tuple(MenuTag, CIndex) = list::zipHead_nd(MenuTagList, RestList),
            CName =
            if
                Node = Xml_Options:getNode_nd([current(GroupNode), child("command", { (CN) :- CN:attribute(index_C) = CIndex})]),
                Name = Node:attribute(name_C),
                Name <> ""
            then
                Name
            else
                "Empty"
            end if,
            ItemTitle = string::format("#% [%] <-> #% [%]", CmdID, CmdName, CIndex, CName),
            assert(reOrder_F(MenuTag, CmdID, CIndex))
            ].

clauses
    reOrderCommands(MenuTag):-
        if
            reOrder_F(MenuTag, FromID, ToID),
            GroupNode = tryGetGroupNode(Xml_Options),
            FromNode = Xml_Options:getNode_nd([current(GroupNode), child("command", { (FN) :- FN:attribute(index_C) = FromID})])
        then
            if ToNode = Xml_Options:getNode_nd([current(GroupNode), child("command", { (TN) :- TN:attribute(index_C) = ToID})]) then
                ToNode:modifyAttribute(index_C, FromID)
            end if,
            FromNode:modifyAttribute(index_C, ToID),
            LBIndex = toTerm(positive, ToID)-1,
            lbCommandIndex_ctl:selectAt(LBIndex, true)
        end if.

predicates
    updateCheckAttribute : (string AttributeName,string NewValue).
clauses
    updateCheckAttribute(AttributeName, NewValue):-
        if Node = tryGetSelectNode() then
            if _ = Node:attribute(AttributeName) then
                Node:modifyAttribute(AttributeName, NewValue)
            elseif NewValue <> "" then
                Node:addAttribute(AttributeName, NewValue)
            end if
        end if,
        !.

predicates
    checkAndUpdate : (checkButton Source,string AttributeName).
clauses
    checkAndUpdate(Source, AttributeName):-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
            CmdID = lbCommandIndex_ctl:getAt(CmdIndex),
            CmdNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" }),
                                                                child("command", { (CN) :- CN:attribute(index_C) = CmdID})]),
            Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" }),
                                                                child("command", { (CN) :- CN:attribute(AttributeName) = "true"})]),
            CmdName = CmdNode:attribute(name_C),
            NodeName = Node:attribute(name_C),
            NodeName <> CmdName
        then
            Ask = string::format("You already have a command (%s) with the flag set. Only one command can have this flag.\n"
                                            "Do you want to change this command (%)?", NodeName, CmdName),
            if 1 = vpiCommonDialogs::askBox("", Ask, mesbox_buttonsYesNo, mesbox_defaultSecond) then
                Node:removeAttribute(AttributeName),
                updateCheckAttribute(AttributeName, "true")
            else
                Source:setChecked(false)
            end if
        else
            updateCheckAttribute(AttributeName, "true")
        end if.

predicates
    onCmdNameEditorModified : editControl::modifiedListener.
clauses
    onCmdNameEditorModified(Source):-
        disabledControlsForEmpty(toBoolean("" <> string::trim(Source:getText()))).

predicates
    disabledControlsForEmpty : (boolean Enabled).
clauses
    disabledControlsForEmpty(Enabled):-
        commandTab_ctl:cbCallAssociations_P:setEnabled(Enabled),
        commandTab_ctl:cbDefCommand_P:setEnabled(Enabled),
        commandTab_ctl:cbCheckStatus_P:setEnabled(Enabled),
        commandTab_ctl:cbFEMode_P:setEnabled(Enabled),
        commandTab_ctl:cbPossibleAll_P:setEnabled(Enabled),
        pbReOrder_ctl:setEnabled(Enabled),
        if false = commandTab_ctl:cbCallAssociations_P:getChecked() then
            setEnabledControls(Enabled)
        end if.

predicates
    onCmdNameEditorLoseFocus : window::loseFocusListener.
clauses
    onCmdNameEditorLoseFocus(Source):-
        NewValue = string::trim(Source:getText()),
        if
            GroupNode = tryGetGroupNode(Xml_Options),
            CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
            CmdID = lbCommandIndex_ctl:getAt(CmdIndex),
            CmdNames = [string::toLowerCase(Node:attribute(name_C))||
                Node = Xml_Options:getNode_nd([current(GroupNode), child("command", { (CN) :- CN:attribute(index_C) <> CmdID})]),
                Node:attribute(name_C) <> ""
            ],
            ErrorMessage = checkCommandName(NewValue, CmdNames)
        then
            messageBox::displayError(This, ErrorMessage),
            This:postUserEvent(0,1)
        else
            updateCommandName(NewValue)
        end if.

predicates
    updateCommandName : (string NewValue).
clauses
    updateCommandName(NewValue):-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
            CmdID = lbCommandIndex_ctl:getAt(CmdIndex),
            xml_ExtOptions(ExtName, Xml_Options),
            GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })])
        then
            if
                Node = Xml_Options:getNode_nd([current(GroupNode), child("command", { (CN) :- CN:attribute(index_C) = CmdID})])
            then
                if _ = Node:attribute(name_C) then
                    Node:modifyAttribute(name_C, NewValue)
                elseif NewValue <> "" then
                    Node:addAttribute(name_C, NewValue)
                end if
            elseif NewValue <> "" then
                CmdNode = xmlElement::new("", command_C, GroupNode),
                GroupNode:addNode(CmdNode),
                foreach namedValue(AttrName, string(AttrValue)) in [namedValue(index_C, string(CmdID)), namedValue(name_C, string(NewValue))] do
                    CmdNode:addAttribute(AttrName, AttrValue)
                end foreach
            end if
        end if.

class predicates
    checkCommandName : (string NewCommand, string* CmdNames) -> string determ.
clauses
    checkCommandName(NewCommand, CmdNames) = ErrorMessage :-
        list::isMemberEq(string::equalIgnoreCase, NewCommand, CmdNames),
        !,
        ErrorMessage = string::format("The Command name <%s> exists!", NewCommand).

predicates
    onEditorModified : editControl::modifiedListener.
clauses
    onEditorModified(Source):-
        CommandTab = commandTab_P,
        updateAttribute(Source),
        CommandTab:updateResultString().

predicates
    tryGetGroupNode : (xmlDocument Xml_Options [out]) -> xmlElement GroupNode determ.
clauses
    tryGetGroupNode(Xml_Options) = GroupNode :-
        Index = extLBox_ctl:tryGetSelectedIndex(),
        ExtName = extLBox_ctl:getAt(Index),
        xml_ExtOptions(ExtName, Xml_Options),
        GroupNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (O) :- O:attribute(title_C) = "ExtOptions" })]),
        !.

predicates
    tryGetSelectNode : (xmlDocument Xml_Options [out]) -> xmlElement Node determ.
clauses
    tryGetSelectNode(Xml_Options) = Node :-
        GroupNode = tryGetGroupNode(Xml_Options),
        CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
        CmdID = lbCommandIndex_ctl:getAt(CmdIndex),
        Node = Xml_Options:getNode_nd([current(GroupNode), child("command", { (CN) :- CN:attribute(index_C) = CmdID})]),
        !.

clauses
    updateAttribute(Source):-
        ID = Source:getCtrlId(),
        NewValue = string::trim(Source:getText()),
        CommandTab = commandTab_P,
        if Node = tryGetSelectNode() then
            if commandTab::editorFileName_id = ID then AttributeName = fileName_C
            elseif commandTab::editFormatCommand_id = ID then AttributeName = formatCmd_C
            elseif commandTab::editCommandName_id = ID then
                AttributeName = name_C,
                CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, NewValue))
            elseif commandTab::editArgString_id = ID then AttributeName = argument_C
            elseif commandTab::editSuffix_id = ID then AttributeName = suffix_C
            elseif commandTab::edInputStreamFile_id = ID then AttributeName = inputFile_C
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
    onStreamModeChanged : checkButton::stateChangedListener.
clauses
    onStreamModeChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        CommandTab = commandTab_P,
        CommandTab:lbCodePage_P:setEnabled(Source:getChecked()),
        if Node = tryGetSelectNode() then
            if _ = Node:attribute(streamModeOn_C) then
                Node:modifyAttribute(streamModeOn_C, NewValue)
            else
                Node:addAttribute(streamModeOn_C, NewValue)
            end if
        end if.

predicates
    onCbPerformStateChanged : checkButton::stateChangedListener.
clauses
    onCbPerformStateChanged(Source, _OldState, _NewState):-
        NewValue = toString(Source:getChecked()),
        if Node = tryGetSelectNode() then
            if _ = Node:attribute(feMode_C) then
                Node:modifyAttribute(feMode_C, NewValue)
            else
                Node:addAttribute(feMode_C, NewValue)
            end if
        end if.

predicates
    onCodePageSelectionChanged : listControl::selectionChangedListener.
clauses
    onCodePageSelectionChanged(Source):-
        ID = Source:getCtrlId(),
        Index = Source:tryGetSelectedIndex(),
        AttrName = if ID = commandTab::lbCodePage_id then codePage_C else inputCP_C end if,
        Node = tryGetSelectNode(),
        !,
        BodyCharset = Source:getAt(Index),
        if
            equal = string::compareIgnoreCase(BodyCharset, "unicode"),
            AttrName = codePage_C
        then
            if _ = Node:attribute(streamMode_C) then
                Node:modifyAttribute(streamMode_C, toString(0)) % set Unicode Stream Mode
            else
                Node:addAttribute(streamMode_C, toString(0))
            end if
        elseif
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(commandTab_ctl:codePageList_P),
            AttrName = codePage_C
        then
            if _ = Node:attribute(streamMode_C) then
                Node:modifyAttribute(streamMode_C, toString(1)) % set Ansi Stream Mode
            else
                Node:addAttribute(streamMode_C, toString(1))
            end if,
            if _ = Node:attribute(AttrName) then
                Node:modifyAttribute(AttrName, CodePage)
            else
                Node:addAttribute(AttrName, CodePage)
            end if
        elseif
            namedValue(CodePage, string(BodyCharset)) = list::getMember_nd(commandTab_ctl:codePageList_P)
        then
            if _ = Node:attribute(AttrName) then
                Node:modifyAttribute(AttrName, CodePage)
            else
                Node:addAttribute(AttrName, CodePage)
            end if
        end if.
    onCodePageSelectionChanged(_Source).

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
            if CommonNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]) then
                ExtListStr = CommonNode:attribute(extName_C) otherwise "",
                editExtList_ctl:setText(ExtListStr),
                if
                    ComponentName = CommonNode:attribute(componentName_C),
                    namedValue(ExpertType,string(ComponentName)) in performGroupList_P,
                    CIndex = list::tryGetIndex(ExpertType, lbExpertType_ctl:getAll())
                then
                    lbExpertType_ctl:selectAt(CIndex, true)
                else
                    lbExpertType_ctl:selectAt(0, true)
                end if
            end if,
            if
                CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
                CmdID = lbCommandIndex_ctl:getAt(CmdIndex)
            then
                setCommandAttributes(Xml_Options, CmdID),
                commandTab_ctl:updateResultString()
            end if
        end if.

predicates
    onSetWordsClick : button::clickResponder.
clauses
    onSetWordsClick(_Source) = button::defaultAction :-
        if
            Node = tryGetSelectNode(_Xml_Options),
            KeyWords = Node:attribute(keyWords_C),
            WordsList = toTerm(KeyWords),
            TitleList = optionsDialog:getDialogTitleList("addErrorWarning"),
            some(NewWordsList) = addErrorWarningWords::display(optionsDialog, WordsList, TitleList)
        then
            if _ = Node:attribute(keyWords_C) then
                Node:modifyAttribute(keyWords_C, toString(NewWordsList))
            else
                Node:addAttribute(keyWords_C, toString(NewWordsList))
            end if
        end if.

predicates
    onLbCommandNameSelectionChanged : listControl::selectionChangedListener.
clauses
    onLbCommandNameSelectionChanged(_Source):-
        if
            Index = extLBox_ctl:tryGetSelectedIndex(),
            ExtName = extLBox_ctl:getAt(Index),
            xml_ExtOptions(ExtName, Xml_Options),
            CmdIndex = lbCommandIndex_ctl:tryGetSelectedIndex(),
            CmdID = lbCommandIndex_ctl:getAt(CmdIndex)
        then
            setCommandAttributes(Xml_Options, CmdID),
            commandTab_ctl:updateResultString()
        end if.

predicates
    setCommandAttributes : (xmlDocument, string).
clauses
    setCommandAttributes(Xml_Options, CommandIndex):-
        CommandTab = commandTab_ctl,
        if
            Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("command", { (C):- C:attribute(index_C) = CommandIndex })]),
            Node:attribute(name_C) = CommandName and CommandName <> ""
        then
            editCommandName_ctl:setText(CommandName),
            CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, CommandName)),
            FormatCommand = Node:attribute(formatCmd_C) otherwise "",
            CommandTab:editFormatCommand_P:setText(FormatCommand),
            ApplicationName = Node:attribute(fileName_C) otherwise "",
            CommandTab:applicationFileName_P:setText(ApplicationName),
            Arguments = Node:attribute(argument_C) otherwise "",
            CommandTab:editArguments_P:setText(Arguments),
            SuffixPerformer = Node:attribute(suffix_C) otherwise "",
            CommandTab:editSuffix_P:setText(SuffixPerformer),
            InputStreamFile = Node:attribute(inputFile_C) otherwise "",
            CommandTab:edInputStreamFile_P:setText(InputStreamFile),
            StreamModeOn = toBoolean("true" = Node:attribute(streamModeOn_C)),
            CommandTab:cbStreamMode_P:setChecked(StreamModeOn),
            StreamModeIndex = toTerm(positive, Node:attribute(streamMode_C)) otherwise 0,
            PerformOn = toBoolean("true" = Node:attribute(feMode_C)),
            CommandTab:cbFEMode_P:setChecked(PerformOn),
            WinAssOn = toBoolean("true" = Node:attribute(winAss_C)),
            CommandTab:cbCallAssociations_P:setChecked(WinAssOn),
            DefCommandOn = toBoolean("true" = Node:attribute(defCommand_C)),
            CommandTab:cbDefCommand_P:setChecked(DefCommandOn),
            CheckStatusOn = toBoolean("true" = Node:attribute(checkStatus_C)),
            CommandTab:cbCheckStatus_P:setChecked(CheckStatusOn),
            AllPossibleOn = toBoolean("true" = Node:attribute(allPossible_C)),
            CommandTab:cbPossibleAll_P:setChecked(AllPossibleOn),
            if StreamModeIndex = 0 then
                CommandTab:lbCodePage_P:selectAt(0, true)
            elseif
                CP = Node:attribute(codePage_C),
                list::memberIndex_nd(NV, CodePageIndex, commandTab_ctl:codePageList_P),
                NV = namedValue(CP, _)
            then
                CommandTab:lbCodePage_P:selectAt(CodePageIndex, true)
            else
                CommandTab:lbCodePage_P:selectAt(0, true)
            end if,
            if
                ICP = Node:attribute(inputCP_C),
                list::memberIndex_nd(INV, ICodePageIndex, commandTab_ctl:codePageList_P),
                INV = namedValue(ICP, _)
            then
                CommandTab:lbInputEncoding_P:selectAt(ICodePageIndex, true)
            else
                CommandTab:lbInputEncoding_P:selectAt(0, true)
            end if,
            disabledControlsForEmpty(true)
        else
            editCommandName_ctl:setText(""),
            CommandTab:gbResultStr_P:setText(string::format(formatResultStr_f, "")),
            CommandTab:editFormatCommand_P:setText(""),
            CommandTab:applicationFileName_P:setText(""),
            CommandTab:editArguments_P:setText(""),
            CommandTab:editSuffix_P:setText(""),
            CommandTab:edInputStreamFile_P:setText(""),
            CommandTab:cbStreamMode_P:setChecked(false),
            CommandTab:cbFEMode_P:setChecked(false),
            CommandTab:cbCallAssociations_P:setChecked(false),
            CommandTab:cbDefCommand_P:setChecked(false),
            CommandTab:cbCheckStatus_P:setChecked(false),
            CommandTab:cbPossibleAll_P:setChecked(false),
            CommandTab:lbCodePage_P:selectAt(0, true),
            disabledControlsForEmpty(false)
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
        ExtGroup:addAttribute(title_C, "ExtOptions"),
        Xml_Options:root_P:addNode(ExtGroup),
        ExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        foreach tuple(NodeName, AttributesList) in ExtOptionsList do
            CmdNode = xmlElement::new("", NodeName, ExtGroup),
            ExtGroup:addNode(CmdNode),
            foreach namedValue(AttrName, string(AttrValue)) in AttributesList do
                CmdNode:addAttribute(AttrName, AttrValue)
            end foreach
        end foreach.

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
            end if
        end if.

predicates
    onAddExtClick : button::clickResponder.
clauses
    onAddExtClick(_Source) = button::defaultAction :-
        TitleDialogList = optionsDialog:getDialogTitleList("addNewSourceType"),
            ReturnValue = addNewSourceType::display(This, existsExtValue, TitleDialogList),
        if tuple(NewName, NewExtStr) = isSome(ReturnValue) then
            addNewSourceType(NewName, string::toLowerCase(string::trim(NewExtStr)))
        end if.

constants
    newTemplateExtOptions_C : extOptions =
        [
        tuple(command_C,[namedValue(index_C, string("1")), namedValue(name_C, string("Open")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])
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
    existsExtValue : topLevelContainerWindow::validateResponder.
clauses
    existsExtValue(Dialog) = ValidResult :-
        EditName = convert(addNewSourceType, Dialog):editName_P,
        NewName = EditName:getText(),
        EditValue = convert(addNewSourceType, Dialog):editValue_P,
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

class predicates
    existsName : predicate_dt{string, namedValue}.
clauses
    existsName(NewName, namedValue(ExtName, _)):-
        string::equalIgnoreCase(NewName, ExtName),
        !.

class predicates
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

facts
    newExtOptionsList_P : namedValue* := [].
clauses
    createNewExtOptionsList():-
        newExtOptionsList_P := [],
        foreach namedValue(ExtName, string(ExtOptionsStr)) in extOptionsList_P do
            if xml_ExtOptions(ExtName, Xml_Options) then
            else
                Xml_Options = extOptionsStr2xml_Options(ExtName, ExtOptionsStr)
            end if,
            if
                Node = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]),
                ExtListStr = Node:attribute(extName_C)
            then
                newExtOptionsList_P := list::append(newExtOptionsList_P, [namedValue(ExtName, string(ExtListStr))])
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
            AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]
            ].

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

predicates
    onLbExpertTypeSelectionChanged : listControl::selectionChangedListener.
clauses
    onLbExpertTypeSelectionChanged(Source):-
        if
            Index = Source:tryGetSelectedIndex(),
            ExpType = Source:getAt(Index),
            _ = tryGetGroupNode(Xml_Options),
            CommonNode = Xml_Options:getNode_nd([root(), child(groupNode_C, { (_) }), child("common", { (_) })]),
            CompName = namedValue::getNamedDefault_string(performGroupList_P, ExpType, "")
        then
            if _ = CommonNode:attribute(componentName_C) then
                CommonNode:modifyAttribute(componentName_C, CompName)
            else
                CommonNode:addAttribute(componentName_C, CompName)
            end if
        end if.

% This code is maintained automatically, do not update it manually.
%  16:01:04-14.12.2018

facts
    delExt_ctl : button.
    addExt_ctl : button.
    extLBox_ctl : listBox.
    editExtList_ctl : editControl.
    staticText1_ctl : textControl.
    staticText5_ctl : textControl.
    staticText_ctl : textControl.
    stCommand_ctl : textControl.
    commandTab_ctl : commandtab.
    lbCommandIndex_ctl : listButton.
    txtCommandName_ctl : textControl.
    editCommandName_ctl : editControl.
    pbReOrder_ctl : button.
    lbExpertType_ctl : listButton.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("performExt2"),
        This:setSize(396, 263),
        extLBox_ctl := listBox::new(This),
        extLBox_ctl:setPosition(8, 20),
        extLBox_ctl:setSize(80, 222),
        extLBox_ctl:setSort(false),
        extLBox_ctl:setAnchors([control::left, control::top, control::bottom]),
        extLBox_ctl:addSelectionChangedListener(onExtLBoxSelectionChanged),
        addExt_ctl := button::new(This),
        addExt_ctl:setText("Add"),
        addExt_ctl:setPosition(8, 246),
        addExt_ctl:setSize(36, 12),
        addExt_ctl:defaultHeight := false,
        addExt_ctl:setAnchors([control::left, control::bottom]),
        addExt_ctl:setClickResponder(onAddExtClick),
        delExt_ctl := button::new(This),
        delExt_ctl:setText("Delete"),
        delExt_ctl:setPosition(52, 246),
        delExt_ctl:setSize(36, 12),
        delExt_ctl:defaultHeight := false,
        delExt_ctl:setAnchors([control::left, control::bottom]),
        delExt_ctl:setClickResponder(onDelExtClick),
        editExtList_ctl := editControl::new(This),
        editExtList_ctl:setText("Edit"),
        editExtList_ctl:setPosition(168, 6),
        editExtList_ctl:setWidth(204),
        editExtList_ctl:setAnchors([control::left, control::top, control::right]),
        editExtList_ctl:setLowerCase(),
        editExtList_ctl:addLoseFocusListener(onEditExtListLoseFocus),
        lbExpertType_ctl := listButton::new(This),
        lbExpertType_ctl:setPosition(168, 20),
        lbExpertType_ctl:setWidth(204),
        lbExpertType_ctl:setSort(false),
        lbExpertType_ctl:addSelectionChangedListener(onLbExpertTypeSelectionChanged),
        lbCommandIndex_ctl := listButton::new(This),
        lbCommandIndex_ctl:setPosition(168, 34),
        lbCommandIndex_ctl:setWidth(32),
        lbCommandIndex_ctl:setSort(false),
        lbCommandIndex_ctl:addSelectionChangedListener(onLbCommandNameSelectionChanged),
        editCommandName_ctl := editControl::new(This),
        editCommandName_ctl:setText(""),
        editCommandName_ctl:setPosition(284, 34),
        editCommandName_ctl:setWidth(88),
        staticText_ctl := textControl::new(This),
        staticText_ctl:setText("Source Type Name:"),
        staticText_ctl:setPosition(8, 7),
        staticText_ctl:setSize(80, 10),
        staticText5_ctl := textControl::new(This),
        staticText5_ctl:setText("Component Name:"),
        staticText5_ctl:setPosition(92, 21),
        staticText5_ctl:setSize(72, 10),
        staticText5_ctl:setAlignment(alignRight),
        staticText1_ctl := textControl::new(This),
        staticText1_ctl:setText("Extension List: "),
        staticText1_ctl:setPosition(92, 7),
        staticText1_ctl:setSize(72, 10),
        staticText1_ctl:setAlignment(alignRight),
        stCommand_ctl := textControl::new(This),
        stCommand_ctl:setText("     Op #: "),
        stCommand_ctl:setPosition(92, 36),
        stCommand_ctl:setSize(72, 10),
        stCommand_ctl:setAlignment(alignRight),
        commandTab_ctl := commandtab::new(This),
        commandTab_ctl:setPosition(92, 50),
        commandTab_ctl:setSize(296, 210),
        txtCommandName_ctl := textControl::new(This),
        txtCommandName_ctl:setText("Name:"),
        txtCommandName_ctl:setPosition(252, 36),
        txtCommandName_ctl:setSize(28, 10),
        txtCommandName_ctl:setAlignment(alignRight),
        pbReOrder_ctl := button::new(This),
        pbReOrder_ctl:setText("ReOrder"),
        pbReOrder_ctl:setPosition(204, 34),
        pbReOrder_ctl:setSize(44, 12),
        pbReOrder_ctl:defaultHeight := false.
% end of automatic code
end implement performExt2
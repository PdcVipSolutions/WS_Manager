%

implement wsfe_Settings
    inherits dialog
    inherits wsFE_Connector
    open core, vpiDomains, ws_EventManager

facts
    generalTab_P : generalTab.
    virtualDirTab_P : virtualDirTab.
    extOptionsTab_P : performExt2.

    addText_F : string := "".
    commandTab_F : commandTab := erroneous.

clauses
    display(Parent, WS_FrontEnd, SettingsList) = Dialog :-
        Dialog = new(Parent, WS_FrontEnd, SettingsList),
        Dialog:show().

constructors
    new : (window Parent,ws_FrontEnd,namedValue* SettingsList).
clauses
    new(Parent, WS_FrontEnd, SettingsList) :-
        wsFE_Connector::new(WS_FrontEnd),
        dialog::new(Parent),
        generatedInitialize(),
        setText(ws_Events():getString(ttlSettings)),
        cancel_ctl:setText(ws_Events():getString(ttlCancel_pb)),
        help_ctl:setText(ws_Events():getString(ttlHelp_pb)),
        getSettingsFromList(SettingsList, SelectSourceType, WSVFile, ExtOptionsList, VirtualDirList, SourceColorsList, PerformGroupList, Languages),
        %---
        Page2 = tabPage::new(),
        extOptionsTab_P := performExt2::new(This, Page2:getContainerControl(), SelectSourceType, ExtOptionsList, PerformGroupList),
        Page2:setText(ws_Events():getString(ttlSourceTypeTab)),
        tabControl_ctl:addPage(Page2),
        %---
        Page3 = tabPage::new(),
        virtualDirTab_P := virtualDirTab::new(Page3:getContainerControl(), VirtualDirList),
        Page3:setText(ws_Events():getString(ttlVirtDirTab)),
        tabControl_ctl:addPage(Page3),
        virtualDirTab_P:new_P:setText(ws_Events():getString(ttlNew_pb)),
        virtualDirTab_P:new_P:setClickResponder(onNewClick),
        virtualDirTab_P:edit_P:setText(ws_Events():getString(ttlEdit_pb)),
        virtualDirTab_P:edit_P:setClickResponder(onEditClick),
        virtualDirTab_P:delete_P:setText(ws_Events():getString(ttlDelete_pb)),
        virtualDirTab_P:delete_P:setClickResponder(onDeleteClick),
        virtualDirTab_P:pbBrowse_P:setClickResponder(onBrowseWSVClick),
        virtualDirTab_P:virtualDirListView_P:addMouseDblClickListener(onVirtualDirListViewMouseDblClick),
        virtualDirTab_P:virtualDirListView_P:addSelectEndListener(onVirtualDirListViewSelectEnd),
        virtualDirTab_P:edWSVariableFile_P:setText(WSVFile),
        wsFE_Tasks():wsvUpdateResponder_P := {(WSVariables) :- virtualDirTab_P:setVirtualDirList(WSVariables)},
        %---
        Page1 = tabPage::new(),
        generalTab_P := generalTab::new(Page1:getContainerControl(), SourceColorsList, Languages),
        generalTab_P:listViewControl_P:insertColumnList(1,
            [listViewControl::column(ws_Events():getString(ttlSourceFile_clm), 200, alignleft), listViewControl::column(ws_Events():getString(ttlColorValue_clm), 102, alignleft)]),
        generalTab_P:pbFontColor_P:setText(ws_Events():getString(ttlFontColor_pb)),
        generalTab_P:staticText_P:setText(ws_Events():getString(ttlSourceFontColors_st)),
        generalTab_P:pbBGColor_P:setText(ws_Events():getString(ttlBGColor_pb)),
        generalTab_P:stCurrentLang_P:setText(ws_Events():getString(ttlLanguage_st)),
        Page1:setText(ws_Events():getString(ttlMiscTab)),
        tabControl_ctl:addPage(Page1).

class predicates
    getSettingsFromList : (namedValue* SettingsList,string SelectSourceType [out],string WSVFile [out],
            namedValue* ExtOptionsList [out],namedValue* VirtualDirList [out],namedValue* SourceColorsList [out],namedValue* PerformGroupList [out],string Languages [out]).
clauses
    getSettingsFromList([namedValue("lang",string(Languages)),namedValue("count", string(Counts))|SettingsList],
                                  SelectSourceType, WSVFile, ExtOptionsList, VirtualDirList, SourceColorsList, PerformGroupList, Languages):-
        !,
        tuple(ExtCount, VDCount, SelectSourceType, WSVFile) == toTerm(tuple{integer,integer,string,string}, Counts),
        list::split(ExtCount, SettingsList, ExtOptionsList, RestList),
        list::split(VDCount, RestList, VirtualDirList, RestList2),
        list::split(3, RestList2, SourceColorsList, PerformGroupList).
    getSettingsFromList(_, "", "", [], [], [], [], "").

predicates
    onNewClick : button::clickResponder.
clauses
    onNewClick(_Source) = button::defaultAction :-
        ReturnValue = editVirtualDir::display(virtualDirTab_P, "", "", getDialogTitleList("editVirtualDir")),
        if tuple(Name, NewDirValue) = isSome(ReturnValue) then
            virtualDirTab_P:virtualDirListView_P:insertItem(listViewControl::item(virtualDirTab_P:getNextId(), string::concat("$(", Name, ")"), virtualDirTab_P:sm_P, [], [NewDirValue])),
            notify(methodRequest,ws_EventManager::insertVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.

predicates
    onVirtualDirListViewMouseDblClick : listViewControl::mouseDblClickListener.
clauses
    onVirtualDirListViewMouseDblClick(Source, _Point):-
        N = Source:getFocus(),
        if listViewControl::item(_, _, virtualDirTab_P:sm_P, _, _) = Source:tryGetItem(N) then
            _ = onEditClick(virtualDirTab_P:edit_P)
        end if.

predicates
    onVirtualDirListViewSelectEnd : listViewControl::selectEndListener.
clauses
    onVirtualDirListViewSelectEnd(_Source, ItemId, _Select) :-
        virtualDirTab_P:virtualDirListView_P:getItem(ItemId) = listViewControl::item(_, _, IconId, _, _),
        virtualDirTab_P:edit_P:setEnabled(toBoolean(virtualDirTab_P:sm_P = IconId)),
        virtualDirTab_P:delete_P:setEnabled(toBoolean(virtualDirTab_P:sm_P = IconId)).

predicates
    onEditClick : button::clickResponder.
clauses
    onEditClick(_Source) = button::defaultAction :-
        ItemId = virtualDirTab_P:virtualDirListView_P:getFocus(),
        listViewControl::item(ItemId, MacroName, BitmapIdx, Flags, [DirValue]) = virtualDirTab_P:virtualDirListView_P:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        ReturnValue = editVirtualDir::display(virtualDirTab_P, Name, DirValue, getDialogTitleList("editVirtualDir")),
        if tuple(_, NewDirValue) = isSome(ReturnValue) and equal <> string::compareIgnoreCase(NewDirValue, DirValue) then
            virtualDirTab_P:virtualDirListView_P:updateItem(listViewControl::item(ItemId, MacroName, BitmapIdx, Flags, [NewDirValue])),
            notify(methodRequest,ws_EventManager::updateVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.
    onEditClick(_Source) = button::defaultAction.

predicates
    onDeleteClick : button::clickResponder.
clauses
    onDeleteClick(_Source) = button::defaultAction :-
        ItemId = virtualDirTab_P:virtualDirListView_P:getFocus(),
        listViewControl::item(_, MacroName, _, _, _) = virtualDirTab_P:virtualDirListView_P:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        Message = string::format(ws_Events():getString(delVirtualDir), MacroName),
        Answer = vpiCommonDialogs::messageBox(ws_Events():getString(ttlWarning), Message, mesbox_iconQuestion,
                    mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
        if idc_ok = Answer then
            notify(methodRequest,ws_EventManager::deleteVirtualDir_C, [namedValue(Name, core::none)]),
            virtualDirTab_P:virtualDirListView_P:deleteItem(ItemId)
        end if.
    onDeleteClick(_Source) = button::defaultAction.

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
        CommandTab = extOptionsTab_P:commandTab_P,
        if FileName = vpiCommonDialogs::getFileName
            (StartExt, ["Executable File","*.exe","Text File","*.txt","All files","*.*"], "File", [vpiDomains::dlgfn_filemustexist], CurrentDirectory, _SelectedFiles) then
            directory::setCurrentDirectory(CurrentDirectory),
            if commandTab::browseSuffix_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab, CommandTab:editSuffix_P, ShortFileName)})
            elseif commandTab::browseInputFile_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab, CommandTab:edInputStreamFile_P, ShortFileName)})
            else
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(CommandTab, CommandTab:applicationFileName_P, ShortFileName)})
            end if
       end if.

predicates
    updateEditorControl : (commandTab, editControl, string).
clauses
    updateEditorControl(CommandTab, EditControl, ShortFileName):-
        EditControl:setText(ShortFileName),
        extOptionsTab_P:updateAttribute(EditControl),
        CommandTab:updateResultString().

constants
    menuTag0_id = commandTab::cbPossibleAll_id + 1.
    menuTag1_id = menuTag0_id + 1.
    menuTag2_id = menuTag1_id + 1.
    menuTag3_id = menuTag2_id + 1.
    menuTag4_id = menuTag3_id + 1.
    menuTag5_id = menuTag4_id + 1.
    menuTag6_id = menuTag5_id + 1.
    menuTag7_id = menuTag6_id + 1.
%---
    menuTag8_id = menuTag7_id + 1.
    menuTag9_id = menuTag8_id + 1.
    menuTag10_id = menuTag9_id + 1.
%---
    menuTag11_id = menuTag10_id + 1.
    menuTag12_id = menuTag11_id + 1.

clauses
    onGetMacroSymbolsClick(Source) = button::defaultAction :-
        rct(L, T, _, _) = tabControl_ctl:getOuterRect(),
        rct(L1, T1, _, _) = extOptionsTab_P:commandTab_P:getOuterRect(),
        commandTab_F := convert(commandTab, Source:getParent()),
        rct(XC, _, _, YC) = commandTab_F:getMacroSymbols_P:getOuterRect(),
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
            ]), pnt(L+L1+XC+24, T+T1+YC+24), align_Right). % BB!!!

clauses
    onPbReOrderClick(_Source) = button::defaultAction :-
        rct(L, T, _, _) = tabControl_ctl:getOuterRect(),
        rct(L1, _, _, B1) = extOptionsTab_P:pbReOrder_P:getOuterRect(),
        if PopupItems = extOptionsTab_P:tryCreatePopUp([menuTag8_id, menuTag9_id, menuTag10_id]) then
            menuPopUp(dynMenu(PopupItems), pnt(L+L1, T+B1+24), align_Left)
        end if.

clauses
    onBrowseWSVClick(_Source) = button::defaultAction :-
        rct(L, T, _, _) = tabControl_ctl:getOuterRect(),
        rct(_, _, R1, B1) = virtualDirTab_P:pbBrowse_P:getOuterRect(),
        menuPopUp(dynMenu(
            [
            txt(menuTag11_id, ws_Events():getString(ttlBrowse_pb), noAccelerator, 1, mis_none, []),
            txt(menuTag12_id, ws_Events():getString(txtClear), noAccelerator, 1, mis_none, [])
            ]), pnt(L+R1, T+B1+24), align_Right). % BB!!!

predicates
    onMenuItem : window::menuItemListener.
clauses
    onMenuItem(_Source, Tag):-
        Tag >= menuTag11_id,
        !,
        if Tag = menuTag11_id then
            CurrentDirectory = directory::getCurrentDirectory(),
            if FileName = vpiCommonDialogs::getFileName
                ("ide.vars", ["IDE Variables File","*.vars"], "File", [], CurrentDirectory, _SelectedFiles) then
                directory::setCurrentDirectory(CurrentDirectory),
                virtualDirTab_P:edWSVariableFile_P:setText(FileName),
                notify(methodRequest,ws_EventManager::setWSVariableFile_C, [namedValue("wsvFile",string(FileName))])
            end if
        else
            virtualDirTab_P:edWSVariableFile_P:setText(""),
            notify(methodRequest,ws_EventManager::setWSVariableFile_C, [namedValue("wsvFile",string(""))])
        end if.
    onMenuItem(_Source, Tag):-
        Tag >= menuTag8_id,
        !,
        extOptionsTab_P:reOrderCommands(Tag).
    onMenuItem(_Source, Tag):-
        CurText = commandTab_F:editFormatCommand_P:getText(),
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
        commandTab_F:editFormatCommand_P:setText(string::concat(CurText, AddText)),
        extOptionsTab_P:updateAttribute(commandTab_F:editFormatCommand_P),
        commandTab_F:updateResultString().

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::noAction :-
        tryCheckVirtualDirectory(),
        !.
    onOkClick(_Source) = button::defaultAction :-
        extOptionsTab_P:createNewExtOptionsList(),
        notify(methodRequest,ws_EventManager::updateExtOptionsList_C, extOptionsTab_P:newExtOptionsList_P),
        foreach namedValue(ExtName, string(ExtOptionsStr)) in extOptionsTab_P:extOptionsList_P do
            OptionsValues = extOptionsTab_P:getExtOptionsValues(ExtName, ExtOptionsStr),
            notify(methodRequest,ws_EventManager::updateExtOptions_C, [namedValue(ExtName, string(toString(OptionsValues)))])
        end foreach,
        if Index = extOptionsTab_P:extLBox_P:tryGetSelectedIndex(),
        ExtName = extOptionsTab_P:extLBox_P:getAt(Index) then
            notify(methodRequest,ws_EventManager::updateSelectSourceType_C, [namedValue("select", string(ExtName))])
        end if,
        wsFE_Form():prevSelectExt := "",
        notify(methodRequest,ws_EventManager::updateSourceColors_C, generalTab_P:getNewSourceColors()),
        if NewUILanguage = generalTab_P:tryGetNewLanguage() then
            notify(methodRequest,ws_EventManager::updateUILanguage_C, [namedValue("lang", string(NewUILanguage))]),
            ws_Events():setCurrentLng(NewUILanguage)
        end if,
        wsFE_Form():delayCall(20, {:-wsFE_Form():ribbonControl_P:layout := wsFE_Form():ribbonControl_P:layout}),
        wsFE_Tasks():getSourceLocalOptions().

facts
    macroList_F : string* := [].
predicates
    tryCheckVirtualDirectory : () determ.
clauses
    tryCheckVirtualDirectory():-
        macroList_F := ["$(SourceFile)", "$(SourceName)", "$(ExeName)", "$(SourceExeDir)"|
            list::map(virtualDirTab_P:virtualDirListView_P:getAll(), {(ItemId) = ItemName :- listViewControl::item(_, ItemName, _, _, _) == virtualDirTab_P:virtualDirListView_P:getItem(ItemId)})],
        namedValue(ExtName, string(ExtOptionsStr)) in extOptionsTab_P:extOptionsList_P,
            OptionsValues = extOptionsTab_P:getExtOptionsValues(ExtName, ExtOptionsStr),
            tuple("command", AttrValues) in OptionsValues,
            AttrName in [fileName_C, formatCmd_C, suffix_C, argument_C, inputFile_C],
            AttrValue = namedValue::tryGetNamed_string(AttrValues, AttrName),
        UndefindedWSV = checkVirtualDirectory2(AttrValue, macroList_F),
        !,
        vpiCommonDialogs::error(string::format(ws_Events():getString(undefinedWSV), UndefindedWSV)).

predicates
    checkVirtualDirectory2 : (string AttrValue,string* MacroList) -> string UndefindedWSV determ.
clauses
    checkVirtualDirectory2(AttrValue, _) = _ :-
        not(_ = string::search(AttrValue, "$(")),
        !,
        fail.
    checkVirtualDirectory2(AttrValue, MacroList) = UndefindedWSV :-
        string::splitStringBySeparators(AttrValue, "$", _, _, RestStr),
        string::splitStringBySeparators(RestStr, ")", VirtName, _, Rest),
        if list::isMemberBy(string::compareIgnoreCase, string::concat("$", VirtName, ")"), MacroList) then
            !,
            UndefindedWSV = checkVirtualDirectory2(Rest, MacroList)
        elseif
            string::frontchar(VirtName, _, NewVirtName),
            ReturnValue = editVirtualDir::display(This, NewVirtName, "", getDialogTitleList("editVirtualDir")),
            tuple(Name, NewDirValue) = isSome(ReturnValue)
        then
            notify(methodRequest,ws_EventManager::insertVirtualDir_C, [namedValue(Name,string(NewDirValue))]),
            !,
            macroList_F := [string::concat("$(", Name, ")")|macroList_F],
            UndefindedWSV = checkVirtualDirectory2(Rest, macroList_F)
        else
            UndefindedWSV = string::concat("$", VirtName, ")")
        end if.

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
    getDialogTitleList("performExtCtl") =
        [
        namedValue("pbAdd", string(ws_Events():getString(ttlAdd_pb))),
        namedValue("pbDelete", string(ws_Events():getString(ttlDelete_pb))),
        namedValue("txtName", string(ws_Events():getString(ttlName_nst))),
        namedValue("txtValue", string(ws_Events():getString(ttlValue_nst))),
        namedValue("txtCmpName", string(ws_Events():getString(ttlCmpName))),
        namedValue("tabOpen", string(ws_Events():getString(cmdOpen_C))),
        namedValue("tabRun", string(ws_Events():getString(cmdRun_C))),
        namedValue("tabExecute", string(ws_Events():getString(cmdExec_C))),
        namedValue("txtResultStr", string(ws_Events():getString(txtResultStr))),
        namedValue("pbBrowse", string(ws_Events():getString(ttlBrowse_pb))),
        namedValue("txtOpenFormat", string(ws_Events():getString(txtOpenFormat))),
        namedValue("txtEditorFile", string(ws_Events():getString(txtEditorFile))),
        namedValue("txtArguments", string(ws_Events():getString(txtArguments))),
        namedValue("txtFormatCmd", string(ws_Events():getString(txtFormatCmd))),
        namedValue("txtPerformFE", string(ws_Events():getString(txtPerformFE))),
        namedValue("txtDefCommand", string(ws_Events():getString(txtDefCommandFE))),
        namedValue("pbReOrder", string(ws_Events():getString(pbReOrder))),
        namedValue("stCommand", string(ws_Events():getString(stCommand))),
        namedValue("txtCommandName", string(ws_Events():getString(txtCommandName))),

        namedValue("txtApplicationFile", string(ws_Events():getString(txtApplicationFile))),

        namedValue("txtCodePage", string(ws_Events():getString(txtCodePage))),
        namedValue("txtStreamMode", string(ws_Events():getString(txtStreamMode))),
        namedValue("cbStreamMode", string(ws_Events():getString(cbStreamMode))),
        namedValue("txtRunFile", string(ws_Events():getString(txtRunFile))),
        namedValue("gbArgForRun", string(ws_Events():getString(gbArgForRun))),
        namedValue("txtRunMode", string(ws_Events():getString(txtRunMode))),
        namedValue("txtReRunMode", string(ws_Events():getString(txtReRunMode))),
        namedValue("txtSuffix", string(ws_Events():getString(txtSuffix))),
        namedValue("txtRunFormat", string(ws_Events():getString(txtRunFormat))),
        namedValue("txtExecFormat", string(ws_Events():getString(txtExecFormat))),
        namedValue("txtCommandLine", string(ws_Events():getString(txtCommandLine))),
        namedValue("cbPossibleAll", string(ws_Events():getString(cbPossibleAll))),
        namedValue("cbCheckStatus", string(ws_Events():getString(cbCheckStatus))),
        namedValue("cbInvokeWinAss", string(ws_Events():getString(cbInvokeWinAss))),
        namedValue("stInputStream", string(ws_Events():getString(stInputStream))),
        namedValue("gbInputStream", string(ws_Events():getString(gbInputStream))),
        namedValue("gbOutputStream", string(ws_Events():getString(gbOutputStream))),
        namedValue("pbKeywords", string(ws_Events():getString(pbKeywords)))
        ] :-!.
    getDialogTitleList("addNewSourceType") =
        [
        namedValue("addNewSourceType", string(ws_Events():getString(ttlAddNewSourceType))),
        namedValue("pbOk", string(ws_Events():getString(ttlOk_pb))),
        namedValue("pbCancel", string(ws_Events():getString(ttlCancel_pb))),
        namedValue("pbHelp", string(ws_Events():getString(ttlHelp_pb))),
        namedValue("txtName", string(ws_Events():getString(ttlName_nst))),
        namedValue("txtValue", string(ws_Events():getString(ttlValue_nst)))
        ] :-!.
    getDialogTitleList("existsExtValueMsg") =
        [
        namedValue("txtTypeExists", string(ws_Events():getString(txtTypeExists))),
        namedValue("txtExtAdded", string(ws_Events():getString(txtExtAdded))),
        namedValue("txtExtRequired", string(ws_Events():getString(txtExtRequired)))
        ] :-!.
    getDialogTitleList("editVirtualDir") =
        [
        namedValue("pbOk", string(ws_Events():getString(ttlOk_pb))),
        namedValue("pbCancel", string(ws_Events():getString(ttlCancel_pb))),
        namedValue("pbHelp", string(ws_Events():getString(ttlHelp_pb))),
        namedValue("pbBrowse", string(ws_Events():getString(ttlBrowse_pb))),
        namedValue("ttlCreateVirtDir", string(ws_Events():getString(ttlCreateVirtDir))),
        namedValue("ttlEditVirtDir", string(ws_Events():getString(ttlEditVirtDit))),
        namedValue("txtName", string(ws_Events():getString(txtName))),
        namedValue("txtDir", string(ws_Events():getString(txtDir))),
        namedValue("txtError", string(ws_Events():getString(txtError))),
        namedValue("msgFormat", string(ws_Events():getString(msgFormat)))
        ] :-!.
    getDialogTitleList(_) = [].

% This code is maintained automatically, do not update it manually.
%  16:27:40-14.12.2018

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    tabControl_ctl : tabcontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Workspace Settings"),
        setRect(rct(50, 40, 446, 346)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        addMenuItemListener(onMenuItem),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(268, 290),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(332, 290),
        cancel_ctl:setSize(56, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(8, 290),
        help_ctl:setSize(56, 12),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        help_ctl:setVisible(false),
        tabControl_ctl := tabcontrol::new(This),
        tabControl_ctl:setPosition(8, 6),
        tabControl_ctl:setSize(380, 278),
        tabControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]).
% end of automatic code
end implement wsfe_Settings
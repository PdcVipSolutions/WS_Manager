%

implement wsfe_Settings
    inherits dialog
    inherits wsFE_Connector
    open core, vpiDomains, ws_EventManager, xmlNavigate

facts
    generalTab_P : generalTab.
    virtualDirTab_P : virtualDirTab.
    extOptionsTab_P : performExtCtl.

    addText_F : string := "".

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
        getSettingsFromList(SettingsList, SelectSourceType, ExtOptionsList, VirtualDirList, SourceColorsList, Languages),
        %---
        Page2 = tabPage::new(),
        extOptionsTab_P := performExtCtl::new(This, Page2:getContainerControl(), SelectSourceType, ExtOptionsList),
        Page2:setText(ws_Events():getString(ttlSourceTypeTab)),
        tabControl_ctl:addPage(Page2),
        extOptionsTab_P:openTab_P:browseEditor_P:setClickResponder(onBrowseEditorClick),
        extOptionsTab_P:runTab_P:browseEditor_P:setClickResponder(onBrowseEditorClick),
        extOptionsTab_P:runTab_P:browseSuffix_P:setClickResponder(onBrowseEditorClick),
        extOptionsTab_P:execTab_P:getMacroSymbols_P:setClickResponder(onGetMacroSymbolsClick),
        %---
        Page3 = tabPage::new(),
        virtualDirTab_P := virtualDirTab::new(Page3:getContainerControl(), VirtualDirList),
        Page3:setText(ws_Events():getString(ttlVirtDirTab)),
        tabControl_ctl:addPage(Page3),
        virtualDirTab_P:new_ctl:setText(ws_Events():getString(ttlNew_pb)),
        virtualDirTab_P:new_ctl:setClickResponder(onNewClick),
        virtualDirTab_P:edit_ctl:setText(ws_Events():getString(ttlEdit_pb)),
        virtualDirTab_P:edit_ctl:setClickResponder(onEditClick),
        virtualDirTab_P:delete_ctl:setText(ws_Events():getString(ttlDelete_pb)),
        virtualDirTab_P:delete_ctl:setClickResponder(onDeleteClick),
        virtualDirTab_P:virtualDirListView_ctl:addMouseDblClickListener(onVirtualDirListViewMouseDblClick),
        virtualDirTab_P:virtualDirListView_ctl:addSelectEndListener(onVirtualDirListViewSelectEnd),
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

predicates
    getSettingsFromList : (namedValue* SettingsList,string SelectSourceType [out],
            namedValue* ExtOptionsList [out],namedValue* VirtualDirList [out],namedValue* SourceColorsList [out],string Languages [out]).
clauses
    getSettingsFromList([namedValue("lang",string(Languages)),namedValue("count", string(Counts))|SettingsList],
                                    SelectSourceType, ExtOptionsList, VirtualDirList, SourceColorsList, Languages):-
        !,
        tuple(ExtCount, VDCount, SelectSourceType) == toTerm(tuple{integer,integer,string}, Counts),
        list::split(ExtCount, SettingsList, ExtOptionsList, RestList),
        list::split(VDCount, RestList, VirtualDirList, SourceColorsList).
    getSettingsFromList(_, "", [], [], [], "").

predicates
    onNewClick : button::clickResponder.
clauses
    onNewClick(_Source) = button::defaultAction :-
        ReturnValue = editVirtualDir::display(virtualDirTab_P, "", "", getDialogTitleList("editVirtualDir")),
        if tuple(Name, NewDirValue) = isSome(ReturnValue) then
            virtualDirTab_P:virtualDirListView_ctl:insertItem(listViewControl::item(virtualDirTab_P:getNextId(), string::concat("$(", Name, ")"), virtualDirTab_P:sm_P, [], [NewDirValue])),
            notify(methodRequest,ws_EventManager::insertVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.

predicates
    onVirtualDirListViewMouseDblClick : listViewControl::mouseDblClickListener.
clauses
    onVirtualDirListViewMouseDblClick(Source, _Point):-
        N = Source:getFocus(),
        if listViewControl::item(_, _, virtualDirTab_P:sm_P, _, _) = Source:tryGetItem(N) then
            _ = onEditClick(virtualDirTab_P:edit_ctl)
        end if.

predicates
    onVirtualDirListViewSelectEnd : listViewControl::selectEndListener.
clauses
    onVirtualDirListViewSelectEnd(_Source, ItemId, _Select) :-
        virtualDirTab_P:virtualDirListView_ctl:getItem(ItemId) = listViewControl::item(_, _, IconId, _, _),
        virtualDirTab_P:edit_ctl:setEnabled(toBoolean(virtualDirTab_P:sm_P = IconId)),
        virtualDirTab_P:delete_ctl:setEnabled(toBoolean(virtualDirTab_P:sm_P = IconId)).

predicates
    onEditClick : button::clickResponder.
clauses
    onEditClick(_Source) = button::defaultAction :-
        ItemId = virtualDirTab_P:virtualDirListView_ctl:getFocus(),
        listViewControl::item(ItemId, MacroName, BitmapIdx, Flags, [DirValue]) = virtualDirTab_P:virtualDirListView_ctl:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        ReturnValue = editVirtualDir::display(virtualDirTab_P, Name, DirValue, getDialogTitleList("editVirtualDir")),
        if tuple(_, NewDirValue) = isSome(ReturnValue) and equal <> string::compareIgnoreCase(NewDirValue, DirValue) then
            virtualDirTab_P:virtualDirListView_ctl:updateItem(listViewControl::item(ItemId, MacroName, BitmapIdx, Flags, [NewDirValue])),
            notify(methodRequest,ws_EventManager::updateVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.
    onEditClick(_Source) = button::defaultAction.

predicates
    onDeleteClick : button::clickResponder.
clauses
    onDeleteClick(_Source) = button::defaultAction :-
        ItemId = virtualDirTab_P:virtualDirListView_ctl:getFocus(),
        listViewControl::item(_, MacroName, _, _, _) = virtualDirTab_P:virtualDirListView_ctl:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        Message = string::format("Are you sure you want to delete virtual directory '%s'?", MacroName),
        Answer = vpiCommonDialogs::messageBox("Warning", Message, mesbox_iconQuestion,
                    mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
        if idc_ok = Answer then
            notify(methodRequest,ws_EventManager::deleteVirtualDir_C, [namedValue(Name, core::none)]),
            virtualDirTab_P:virtualDirListView_ctl:deleteItem(ItemId)
        end if.
    onDeleteClick(_Source) = button::defaultAction.

predicates
    onBrowseEditorClick : button::clickResponder.
clauses
    onBrowseEditorClick(Source) = button::defaultAction :-
        ID = Source:getCtrlId(),
        CurrentDirectory = directory::getCurrentDirectory(),
        if performExtCtl::browseSuffix_id = ID then
            StartExt = "*.*"
        else
            StartExt = "*.exe"
        end if,
        if FileName = vpiCommonDialogs::getFileName
            (StartExt, ["Executable File","*.exe","All files","*.*"], "File", [vpiDomains::dlgfn_filemustexist], CurrentDirectory, _SelectedFiles) then
            directory::setCurrentDirectory(CurrentDirectory),
            if performExtCtl::browseEditor_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(extOptionsTab_P:openTab_P:editorFileName_P, ShortFileName)})
            elseif performExtCtl::browseRun_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(extOptionsTab_P:runTab_P:editorFileName_P, ShortFileName)})
            elseif performExtCtl::browseSuffix_id = ID then
                wsFE_Tasks():getShortFileName(FileName, {(ShortFileName) :- updateEditorControl(extOptionsTab_P:runTab_P:editSuffix_P, ShortFileName)})
            end if
       end if.

predicates
    updateEditorControl : (editControl, string).
clauses
    updateEditorControl(EditControl, ShortFileName):-
        EditControl:setText(ShortFileName),
        extOptionsTab_P:updateAttribute(EditControl),
        extOptionsTab_P:updateResultString().

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data).

constants
    menuTag0_id = performExtCtl::cbExecutePossible_id + 1.
    menuTag1_id = menuTag0_id + 1.
    menuTag2_id = menuTag1_id + 1.
    menuTag3_id = menuTag2_id + 1.
    menuTag4_id = menuTag3_id + 1.
predicates
    onGetMacroSymbolsClick : button::clickResponder.
clauses
    onGetMacroSymbolsClick(_Source) = button::defaultAction :-
        rct(L, T, _, _) = tabControl_ctl:getOuterRect(),
        rct(L1, T1, _, _) = extOptionsTab_P:tabControl_ctl:getOuterRect(),
        rct(XC, _, _, YC) = extOptionsTab_P:execTab_P:getMacroSymbols_P:getOuterRect(),
        menuPopUp(dynMenu(
            [
            txt(menuTag0_id, "Browse...", noAccelerator, 1, mis_none, []),
            txt(menuTag1_id, "$(SourceFile)", noAccelerator, 1, mis_none, []),
            txt(menuTag2_id, "$(SourceName)", noAccelerator, 1, mis_none, []),
            txt(menuTag4_id, "$(ExeName)", noAccelerator, 1, mis_none, []),
            txt(menuTag3_id, "$(SourceExeDir)", noAccelerator, 1, mis_none, [])
            ]), pnt(L+L1+XC+24, T+T1+YC+48), align_Right). % BB!!!

predicates
    onMenuItem : window::menuItemListener.
clauses
    onMenuItem(_Source, Tag):-
        CurText = extOptionsTab_P:execTab_P:editCmdString_P:getText(),
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
        extOptionsTab_P:execTab_P:editCmdString_P:setText(string::concat(CurText, AddText)),
        extOptionsTab_P:updateAttribute(extOptionsTab_P:execTab_P:editCmdString_P),
        extOptionsTab_P:updateResultString().

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::defaultAction :-
        extOptionsTab_P:createNewExtOptionsList(),
        notify(methodRequest,ws_EventManager::updateExtOptionsList_C, extOptionsTab_P:newExtOptionsList),
        foreach namedValue(ExtName, string(ExtOptionsStr)) in extOptionsTab_P:extOptionsList_P do
            OptionsValues = extOptionsTab_P:getExtOptionsValues(ExtName, ExtOptionsStr),
            notify(methodRequest,ws_EventManager::updateExtOptions_C, [namedValue(ExtName, string(toString(OptionsValues)))])
        end foreach,
        if Index = extOptionsTab_P:extLBox_ctl:tryGetSelectedIndex(),
        ExtName = extOptionsTab_P:extLBox_ctl:getAt(Index) then
            notify(methodRequest,ws_EventManager::updateSelectSourceType_C, [namedValue("select", string(ExtName))])
        end if,
        notify(methodRequest,ws_EventManager::updateSourceColors_C, generalTab_P:getNewSourceColors()),
        if NewUILanguage = generalTab_P:tryGetNewLanguage() then
%            _ = vpiCommonDialogs::messageBox(%ws_Events():getString(questionTitle_C), ws_Events():getString(qstSaveNewWSM_C),
%                                                                    "Warning", "The new UI Language will be setup after restart application",
%                                                                    mesbox_iconInformation, mesbox_buttonsOk, mesbox_defaultfirst,
%                                                                    mesbox_suspendapplication),
            notify(methodRequest,ws_EventManager::updateUILanguage_C, [namedValue("lang", string(NewUILanguage))]),
            ws_Events():setCurrentLng(NewUILanguage),
            wsFE_Form():delayCall(20, {:-wsFE_Form():ribbonControl_ctl:layout := wsFE_Form():ribbonControl_ctl:layout})
        end if,
        wsFE_Tasks():getSourceLocalOptions().

clauses
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

        namedValue("txtCodePage", string(ws_Events():getString(txtCodePage))),
        namedValue("txtStreamMode", string(ws_Events():getString(txtStreamMode))),
        namedValue("txtRunFile", string(ws_Events():getString(txtRunFile))),
        namedValue("gbArgForRun", string(ws_Events():getString(gbArgForRun))),
        namedValue("txtRunMode", string(ws_Events():getString(txtRunMode))),
        namedValue("txtReRunMode", string(ws_Events():getString(txtReRunMode))),
        namedValue("txtSuffix", string(ws_Events():getString(txtSuffix))),
        namedValue("txtRunFormat", string(ws_Events():getString(txtRunFormat))),
        namedValue("txtExecFormat", string(ws_Events():getString(txtExecFormat))),
        namedValue("txtCommandLine", string(ws_Events():getString(txtCommandLine))),
        namedValue("cbCmdEnabled", string(ws_Events():getString(cbCmdEnabled)))
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
        namedValue("txtError", string(ws_Events():getString(txtError)))
        ] :-!.
    getDialogTitleList(_) = [].

% This code is maintained automatically, do not update it manually. 13:51:47-14.9.2016

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
        setRect(rct(50, 40, 446, 307)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        addMenuItemListener(onMenuItem),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(206, 252),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(269, 252),
        cancel_ctl:setSize(56, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(332, 252),
        help_ctl:setSize(56, 12),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        tabControl_ctl := tabcontrol::new(This),
        tabControl_ctl:setPosition(8, 6),
        tabControl_ctl:setSize(380, 244),
        tabControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]).
% end of automatic code
end implement wsfe_Settings
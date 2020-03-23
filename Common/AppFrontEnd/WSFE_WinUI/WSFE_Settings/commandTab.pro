%

implement commandTab
    inherits userControlSupport
    open core, vpiDomains

clauses
    new(Parent):-
        new(),
        setContainer(Parent).

clauses
    new():-
        userControlSupport::new(),
        generatedInitialize().

clauses
    txtArguments_P() = txtArguments_ctl.
    editArguments_P() = editArguments_ctl.
    cbCallAssociations_P() = cbCallAssociations_ctl.
    txtFormatCommand_P() = txtFormatCommand_ctl.
    editFormatCommand_P() = editFormatCommand_ctl.
    txtApplicationFile_P() = txtApplicationFile_ctl.
    applicationFileName_P() = applicationFileName_ctl.
    browseEditor_P() = browseEditor_ctl.
    editSuffix_P() = editSuffix_ctl.
    browseSuffix_P() = browseSuffix_ctl.
    txtSuffix_P() = txtSuffix_ctl.
    cbFEMode_P() = cbFEMode_ctl.
    cbDefCommand_P() = cbDefCommand_ctl.
    cbCheckStatus_P() = cbCheckStatus_ctl.
    lbCodePage_P() = lbCodePage_ctl.
    cbStreamMode_P() = cbStreamMode_ctl.
    txtCodePage_P() = txtCodePage_ctl.
    getMacroSymbols_P() = getMacroSymbols_ctl.
    resultCommandLine_P() = resultCommandLine_ctl.
    gbResultStr_P() = gbResultStr_ctl.
    cbPossibleAll_P() = cbPossibleAll_ctl.
    stInputStream_P() = stInputStream_ctl.
    edInputStreamFile_P() = edInputStreamFile_ctl.
    browseInputFile_P() = browseInputFile_ctl.
    gbInputStream_P() = gbInputStream_ctl.
    lbInputEncoding_P() = lbInputEncoding_ctl.
    stInputEncoding_P() = stInputEncoding_ctl.
    gbOutputStream_P() = gbOutputStream_ctl.
    pbSetWords_P() = pbSetWords_ctl.

clauses
    setControlsID():-
        applicationFileName_P:setCtrlId(editorFileName_id),
        editArguments_P:setCtrlId(editArgString_id),
        browseEditor_P:setCtrlId(browseEditor_id),
        cbFEMode_P:setCtrlId(performOpen_id),
        cbDefCommand_P:setCtrlId(defCommand_id),
        editSuffix_P:setCtrlId(editSuffix_id),
        cbStreamMode_P:setCtrlId(cbStreamModeOn_id),
        lbCodePage_P:setCtrlId(lbCodePage_id),
        browseSuffix_P:setCtrlId(browseSuffix_id),
        getMacroSymbols_P:setCtrlId(getMacroSymbols_id),
        cbCallAssociations_P:setCtrlId(cbCallAssociations_id),
        editFormatCommand_P:setCtrlId(editFormatCommand_id),
        cbPossibleAll_P:setCtrlId(cbPossibleAll_id),
        edInputStreamFile_P:setCtrlId(edInputStreamFile_id),
        browseInputFile_P:setCtrlId(browseInputFile_id),
        lbInputEncoding_P:setCtrlId(lbInputEncoding_id).

clauses
    addControlsText(TitleDialogList):-
        browseEditor_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        ApplicationFileStr = namedValue::getNamed_string(TitleDialogList, "txtApplicationFile"),
        txtApplicationFile_P:setText(ApplicationFileStr),
        ArgumentsStr = namedValue::getNamed_string(TitleDialogList, "txtArguments"),
        txtArguments_P:setText(ArgumentsStr),
        FormatCmdStr = namedValue::getNamed_string(TitleDialogList, "txtFormatCmd"),
        txtFormatCommand_P:setText(FormatCmdStr),
        DefaultCommand = namedValue::getNamed_string(TitleDialogList, "txtDefCommand"),
        cbDefCommand_P:setText(DefaultCommand),
        PerformAtFrontEnd = namedValue::getNamed_string(TitleDialogList, "txtPerformFE"),
        cbFEMode_P:setText(PerformAtFrontEnd),
        browseSuffix_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        txtCodePage_P:setText(namedValue::getNamed_string(TitleDialogList, "txtCodePage")),
        cbStreamMode_P:setText(namedValue::getNamed_string(TitleDialogList, "cbStreamMode")),
        cbCheckStatus_P:setText(namedValue::getNamed_string(TitleDialogList, "cbCheckStatus")),
        txtSuffix_P:setText(namedValue::getNamed_string(TitleDialogList, "txtSuffix")),
        cbCallAssociations_P:setText(namedValue::getNamed_string(TitleDialogList, "cbInvokeWinAss")),
        cbPossibleAll_P:setText(namedValue::getNamed_string(TitleDialogList, "cbPossibleAll")),
        browseInputFile_P:setText(namedValue::getNamed_string(TitleDialogList, "pbBrowse")),
        stInputStream_P:setText(namedValue::getNamed_string(TitleDialogList, "stInputStream")),
        gbInputStream_P:setText(namedValue::getNamed_string(TitleDialogList, "gbInputStream")),
        stInputEncoding_P:setText(namedValue::getNamed_string(TitleDialogList, "txtCodePage")),
        gbOutputStream_P:setText(namedValue::getNamed_string(TitleDialogList, "gbOutputStream")),
        pbSetWords_P:setText(namedValue::getNamed_string(TitleDialogList, "pbKeywords")),
        !.

facts
    codePageList_P : namedValue* := [].
clauses
    setStreamModeValues():-
        SubKeys = registry::getSubkeys(registry::classesRoot, @"MIME\Database\CodePage"),
        codePageList_P := [namedValue(SubKey, string(CodePage)) ||
            SubKey in SubKeys,
            Key = string::format(@"MIME\Database\CodePage\%", SubKey),
            string(CodePage) = registry::tryGetValue(registry::classesRoot, Key, "BodyCharset")],
        lbCodePage_P:addList([CP||namedValue(_, string(CP)) in codePageList_P]),
        lbCodePage_P:selectAt(0, true),
        lbInputEncoding_P:addList([CP||namedValue(_, string(CP)) in codePageList_P]),
        lbInputEncoding_P:selectAt(0, true).

clauses
    updateResultString():-
        FC = editFormatCommand_P:getText(),
        FC1 = string::replaceAll(FC, "[Application]", applicationFileName_P:getText()),
        FC2 = string::replaceAll(FC1, "[Arguments]", editArguments_P:getText()),
        FC3 = string::replaceAll(FC2, "[Suffix]", editSuffix_P:getText()),
        resultCommandLine_P:setText(FC3),
        !.

% This code is maintained automatically, do not update it manually.
%  19:01:11-10.1.2019

facts
    txtArguments_ctl : textControl.
    editArguments_ctl : editControl.
    cbCallAssociations_ctl : checkButton.
    txtFormatCommand_ctl : textControl.
    editFormatCommand_ctl : editControl.
    getMacroSymbols_ctl : button.
    txtApplicationFile_ctl : textControl.
    applicationFileName_ctl : editControl.
    browseEditor_ctl : button.
    editSuffix_ctl : editControl.
    browseSuffix_ctl : button.
    txtSuffix_ctl : textControl.
    cbDefCommand_ctl : checkButton.
    gbResultStr_ctl : groupBox.
    resultCommandLine_ctl : editControl.
    cbCheckStatus_ctl : checkButton.
    cbPossibleAll_ctl : checkButton.
    cbFEMode_ctl : checkButton.
    gbInputStream_ctl : groupBox.
    stInputStream_ctl : textControl.
    edInputStreamFile_ctl : editControl.
    browseInputFile_ctl : button.
    lbInputEncoding_ctl : listButton.
    stInputEncoding_ctl : textControl.
    gbOutputStream_ctl : groupBox.
    cbStreamMode_ctl : checkButton.
    txtCodePage_ctl : textControl.
    lbCodePage_ctl : listButton.
    pbSetWords_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("commandTab"),
        This:setSize(284, 218),
        txtFormatCommand_ctl := textControl::new(This),
        txtFormatCommand_ctl:setText("Format Command:"),
        txtFormatCommand_ctl:setPosition(0, 5),
        txtFormatCommand_ctl:setSize(64, 10),
        editFormatCommand_ctl := editControl::new(This),
        editFormatCommand_ctl:setText(""),
        editFormatCommand_ctl:setPosition(64, 4),
        editFormatCommand_ctl:setWidth(200),
        getMacroSymbols_ctl := button::new(This),
        getMacroSymbols_ctl:setText("<<"),
        getMacroSymbols_ctl:setPosition(268, 4),
        getMacroSymbols_ctl:setSize(12, 12),
        getMacroSymbols_ctl:defaultHeight := false,
        txtApplicationFile_ctl := textControl::new(This),
        txtApplicationFile_ctl:setText("Application:"),
        txtApplicationFile_ctl:setPosition(0, 21),
        txtApplicationFile_ctl:setSize(44, 10),
        applicationFileName_ctl := editControl::new(This),
        applicationFileName_ctl:setText(""),
        applicationFileName_ctl:setPosition(48, 20),
        applicationFileName_ctl:setWidth(188),
        browseEditor_ctl := button::new(This),
        browseEditor_ctl:setText("Browse..."),
        browseEditor_ctl:setPosition(240, 20),
        browseEditor_ctl:setSize(40, 12),
        browseEditor_ctl:defaultHeight := false,
        txtArguments_ctl := textControl::new(This),
        txtArguments_ctl:setText("Arguments:"),
        txtArguments_ctl:setPosition(0, 37),
        txtArguments_ctl:setSize(44, 10),
        editArguments_ctl := editControl::new(This),
        editArguments_ctl:setText(""),
        editArguments_ctl:setPosition(48, 36),
        editArguments_ctl:setWidth(232),
        txtSuffix_ctl := textControl::new(This),
        txtSuffix_ctl:setText("Suffix:"),
        txtSuffix_ctl:setPosition(0, 53),
        txtSuffix_ctl:setSize(44, 10),
        editSuffix_ctl := editControl::new(This),
        editSuffix_ctl:setText(""),
        editSuffix_ctl:setPosition(48, 52),
        editSuffix_ctl:setWidth(184),
        browseSuffix_ctl := button::new(This),
        browseSuffix_ctl:setText("Browse..."),
        browseSuffix_ctl:setPosition(240, 52),
        browseSuffix_ctl:setSize(40, 12),
        browseSuffix_ctl:defaultHeight := false,
        gbInputStream_ctl := groupBox::new(This),
        gbInputStream_ctl:setText(" Input Stream "),
        gbInputStream_ctl:setPosition(0, 96),
        gbInputStream_ctl:setSize(280, 28),
        stInputStream_ctl := textControl::new(gbInputStream_ctl),
        stInputStream_ctl:setText("Файл:"),
        stInputStream_ctl:setPosition(3, 3),
        stInputStream_ctl:setSize(20, 10),
        edInputStreamFile_ctl := editControl::new(gbInputStream_ctl),
        edInputStreamFile_ctl:setText(""),
        edInputStreamFile_ctl:setPosition(27, 2),
        edInputStreamFile_ctl:setWidth(92),
        browseInputFile_ctl := button::new(gbInputStream_ctl),
        browseInputFile_ctl:setText("Browse..."),
        browseInputFile_ctl:setPosition(123, 2),
        browseInputFile_ctl:setSize(40, 12),
        browseInputFile_ctl:defaultHeight := false,
        stInputEncoding_ctl := textControl::new(gbInputStream_ctl),
        stInputEncoding_ctl:setText("Encoding:"),
        stInputEncoding_ctl:setPosition(167, 3),
        stInputEncoding_ctl:setSize(44, 10),
        stInputEncoding_ctl:setAlignment(alignRight),
        lbInputEncoding_ctl := listButton::new(gbInputStream_ctl),
        lbInputEncoding_ctl:setPosition(215, 2),
        lbInputEncoding_ctl:setWidth(60),
        lbInputEncoding_ctl:setSort(false),
        cbCallAssociations_ctl := checkButton::new(This),
        cbCallAssociations_ctl:setText("Invoke with the Windows Association"),
        cbCallAssociations_ctl:setPosition(0, 128),
        cbCallAssociations_ctl:setWidth(140),
        cbDefCommand_ctl := checkButton::new(This),
        cbDefCommand_ctl:setText("Default Command (Mouse Double Click)"),
        cbDefCommand_ctl:setPosition(0, 144),
        cbDefCommand_ctl:setWidth(192),
        cbPossibleAll_ctl := checkButton::new(This),
        cbPossibleAll_ctl:setText("Possible \"All\""),
        cbPossibleAll_ctl:setPosition(200, 129),
        cbPossibleAll_ctl:setWidth(80),
        cbCheckStatus_ctl := checkButton::new(This),
        cbCheckStatus_ctl:setText("Check Status"),
        cbCheckStatus_ctl:setPosition(200, 144),
        cbCheckStatus_ctl:setWidth(80),
        cbFEMode_ctl := checkButton::new(This),
        cbFEMode_ctl:setText("Perform at Frontend Side"),
        cbFEMode_ctl:setPosition(0, 160),
        cbFEMode_ctl:setWidth(280),
        gbResultStr_ctl := groupBox::new(This),
        gbResultStr_ctl:setText(" Result Command "),
        gbResultStr_ctl:setPosition(0, 174),
        gbResultStr_ctl:setSize(280, 40),
        resultCommandLine_ctl := editControl::new(gbResultStr_ctl),
        resultCommandLine_ctl:setText(""),
        resultCommandLine_ctl:setPosition(3, 2),
        resultCommandLine_ctl:setWidth(271),
        resultCommandLine_ctl:setHeight(24),
        resultCommandLine_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        resultCommandLine_ctl:setMultiLine(),
        resultCommandLine_ctl:setAutoVScroll(true),
        resultCommandLine_ctl:setReadOnly(),
        gbOutputStream_ctl := groupBox::new(This),
        gbOutputStream_ctl:setText(" Output Stream "),
        gbOutputStream_ctl:setPosition(0, 66),
        gbOutputStream_ctl:setSize(280, 28),
        cbStreamMode_ctl := checkButton::new(gbOutputStream_ctl),
        cbStreamMode_ctl:setText("Handle"),
        cbStreamMode_ctl:setPosition(3, 2),
        cbStreamMode_ctl:setWidth(80),
        lbCodePage_ctl := listButton::new(gbOutputStream_ctl),
        lbCodePage_ctl:setPosition(215, 2),
        lbCodePage_ctl:setWidth(60),
        lbCodePage_ctl:setSort(false),
        lbCodePage_ctl:setEnabled(false),
        pbSetWords_ctl := button::new(gbOutputStream_ctl),
        pbSetWords_ctl:setText("Keywords"),
        pbSetWords_ctl:setPosition(91, 2),
        pbSetWords_ctl:setSize(69, 12),
        pbSetWords_ctl:defaultHeight := false,
        txtCodePage_ctl := textControl::new(gbOutputStream_ctl),
        txtCodePage_ctl:setText("Encoding:"),
        txtCodePage_ctl:setPosition(167, 3),
        txtCodePage_ctl:setSize(44, 10),
        txtCodePage_ctl:setAlignment(alignRight).
% end of automatic code
end implement commandTab
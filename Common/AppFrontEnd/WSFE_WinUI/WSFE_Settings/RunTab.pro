%

implement runTab
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
    editorFileName_P() = editorFileName_ctl.
    browseEditor_P() = browseEditor_ctl.
    editSuffix_P() = editSuffix_ctl.
    resultCommandLine_P() = resultCommandLine_ctl.
    editArgStringForRun_P() = editArgStringForRun_ctl.
    editArgStringForReRun_P() = editArgStringForReRun_ctl.
    lbSteramMode_P() = lbSteramMode_ctl.
    lbCodePage_P() = lbCodePage_ctl.
    browseSuffix_P() = browseSuffix_ctl.
    txtFormatCommand_P() = txtFormatCommand_ctl.
    txtRunFile_P() = txtRunFile_ctl.
    txtSuffix_P() = txtSuffix_ctl.
    gbResultStr_P() = gbResultStr_ctl.
    gbArguments_P() = gbArguments_ctl.
    txtRunMode_P() = txtRunMode_ctl.
    txtReRunMode_P() = txtReRunMode_ctl.
    txtStreamMode_P() = txtStreamMode_ctl.
    txtCodePage_P() = txtCodePage_ctl.
    cbFEMode_P() = cbFEMode_ctl.

% This code is maintained automatically, do not update it manually. 14:13:02-14.9.2016

facts
    txtFormatCommand_ctl : textControl.
    txtRunFile_ctl : textControl.
    editorFileName_ctl : editControl.
    browseEditor_ctl : button.
    editSuffix_ctl : editControl.
    txtSuffix_ctl : textControl.
    gbResultStr_ctl : groupBox.
    resultCommandLine_ctl : textControl.
    gbArguments_ctl : groupBox.
    editArgStringForRun_ctl : editControl.
    txtRunMode_ctl : textControl.
    editArgStringForReRun_ctl : editControl.
    txtReRunMode_ctl : textControl.
    txtStreamMode_ctl : textControl.
    lbSteramMode_ctl : listButton.
    txtCodePage_ctl : textControl.
    lbCodePage_ctl : listButton.
    browseSuffix_ctl : button.
    cbFEMode_ctl : checkButton.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Run"),
        This:setSize(252, 180),
        cbFEMode_ctl := checkButton::new(This),
        cbFEMode_ctl:setText("Perform at Frontend Side"),
        cbFEMode_ctl:setPosition(8, 20),
        cbFEMode_ctl:setWidth(236),
        editorFileName_ctl := editControl::new(This),
        editorFileName_ctl:setText(""),
        editorFileName_ctl:setPosition(48, 36),
        editorFileName_ctl:setWidth(152),
        browseEditor_ctl := button::new(This),
        browseEditor_ctl:setText("Browse..."),
        browseEditor_ctl:setPosition(204, 36),
        browseEditor_ctl:setSize(40, 12),
        browseEditor_ctl:defaultHeight := false,
        lbSteramMode_ctl := listButton::new(This),
        lbSteramMode_ctl:setPosition(64, 54),
        lbSteramMode_ctl:setWidth(68),
        lbSteramMode_ctl:setSort(false),
        lbCodePage_ctl := listButton::new(This),
        lbCodePage_ctl:setPosition(172, 54),
        lbCodePage_ctl:setWidth(72),
        lbCodePage_ctl:setSort(false),
        lbCodePage_ctl:setEnabled(false),
        gbArguments_ctl := groupBox::new(This),
        gbArguments_ctl:setText(" Arguments for: "),
        gbArguments_ctl:setPosition(8, 70),
        gbArguments_ctl:setSize(236, 44),
        editArgStringForRun_ctl := editControl::new(gbArguments_ctl),
        editArgStringForRun_ctl:setText(""),
        editArgStringForRun_ctl:setPosition(51, 2),
        editArgStringForRun_ctl:setWidth(180),
        txtRunMode_ctl := textControl::new(gbArguments_ctl),
        txtRunMode_ctl:setText("Run Mode:"),
        txtRunMode_ctl:setPosition(3, 3),
        txtRunMode_ctl:setSize(40, 10),
        editArgStringForReRun_ctl := editControl::new(gbArguments_ctl),
        editArgStringForReRun_ctl:setText(""),
        editArgStringForReRun_ctl:setPosition(51, 18),
        editArgStringForReRun_ctl:setWidth(180),
        txtReRunMode_ctl := textControl::new(gbArguments_ctl),
        txtReRunMode_ctl:setText("ReRun Mode:"),
        txtReRunMode_ctl:setPosition(3, 19),
        editSuffix_ctl := editControl::new(This),
        editSuffix_ctl:setText(""),
        editSuffix_ctl:setPosition(48, 118),
        editSuffix_ctl:setWidth(152),
        browseSuffix_ctl := button::new(This),
        browseSuffix_ctl:setText("Browse..."),
        browseSuffix_ctl:setPosition(204, 118),
        browseSuffix_ctl:setSize(40, 12),
        browseSuffix_ctl:defaultHeight := false,
        gbResultStr_ctl := groupBox::new(This),
        gbResultStr_ctl:setText(" Result Run Command Line "),
        gbResultStr_ctl:setPosition(8, 132),
        gbResultStr_ctl:setSize(236, 40),
        resultCommandLine_ctl := textControl::new(gbResultStr_ctl),
        resultCommandLine_ctl:setText(""),
        resultCommandLine_ctl:setPosition(3, 2),
        resultCommandLine_ctl:setSize(228, 26),
        txtSuffix_ctl := textControl::new(This),
        txtSuffix_ctl:setText("Suffix:"),
        txtSuffix_ctl:setPosition(8, 119),
        txtSuffix_ctl:setSize(40, 10),
        txtRunFile_ctl := textControl::new(This),
        txtRunFile_ctl:setText("Run File:"),
        txtRunFile_ctl:setPosition(8, 37),
        txtRunFile_ctl:setSize(40, 10),
        txtFormatCommand_ctl := textControl::new(This),
        txtFormatCommand_ctl:setText("Format Command: [Run File] [Arguments] \"$(SourceFile)\" [Suffix]"),
        txtFormatCommand_ctl:setPosition(8, 6),
        txtFormatCommand_ctl:setSize(236, 10),
        txtStreamMode_ctl := textControl::new(This),
        txtStreamMode_ctl:setText("Stream mode:"),
        txtStreamMode_ctl:setPosition(8, 55),
        txtStreamMode_ctl:setSize(52, 10),
        txtCodePage_ctl := textControl::new(This),
        txtCodePage_ctl:setText("Code page"),
        txtCodePage_ctl:setPosition(136, 55),
        txtCodePage_ctl:setSize(36, 10).
% end of automatic code
end implement runTab
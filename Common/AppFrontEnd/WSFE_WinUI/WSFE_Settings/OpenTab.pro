%

implement openTab
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
    editArgString_P() = editArgString_ctl.
    resultCommandLine_P() = resultCommandLine_ctl.
    txtFormatCommand_P() = txtFormatCommand_ctl.
    txtEditorFile_P() = txtEditorFile_ctl.
    txtArguments_P() = txtArguments_ctl.
    gbResultStr_P() = gbResultStr_ctl.
    cbFEMode_P() = cbFEMode_ctl.

% This code is maintained automatically, do not update it manually. 14:21:11-3.10.2017

facts
    txtFormatCommand_ctl : textControl.
    txtEditorFile_ctl : textControl.
    editorFileName_ctl : editControl.
    browseEditor_ctl : button.
    editArgString_ctl : editControl.
    txtArguments_ctl : textControl.
    gbResultStr_ctl : groupBox.
    resultCommandLine_ctl : textControl.
    cbFEMode_ctl : checkButton.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Open"),
        This:setSize(252, 180),
        txtFormatCommand_ctl := textControl::new(This),
        txtFormatCommand_ctl:setText("Format Command: [Editor File] [Arguments] \"$(SourceFile)\""),
        txtFormatCommand_ctl:setPosition(8, 6),
        txtFormatCommand_ctl:setSize(236, 10),
        cbFEMode_ctl := checkButton::new(This),
        cbFEMode_ctl:setText("Perform at Frontend Side"),
        cbFEMode_ctl:setPosition(8, 20),
        cbFEMode_ctl:setWidth(236),
        txtEditorFile_ctl := textControl::new(This),
        txtEditorFile_ctl:setText("Editor File:"),
        txtEditorFile_ctl:setPosition(8, 37),
        txtEditorFile_ctl:setSize(40, 10),
        editorFileName_ctl := editControl::new(This),
        editorFileName_ctl:setText(""),
        editorFileName_ctl:setPosition(48, 36),
        editorFileName_ctl:setWidth(152),
        browseEditor_ctl := button::new(This),
        browseEditor_ctl:setText("Browse..."),
        browseEditor_ctl:setPosition(204, 36),
        browseEditor_ctl:setSize(40, 12),
        browseEditor_ctl:defaultHeight := false,
        editArgString_ctl := editControl::new(This),
        editArgString_ctl:setText(""),
        editArgString_ctl:setPosition(48, 52),
        editArgString_ctl:setWidth(196),
        txtArguments_ctl := textControl::new(This),
        txtArguments_ctl:setText("Arguments:"),
        txtArguments_ctl:setPosition(8, 53),
        txtArguments_ctl:setSize(40, 10),
        gbResultStr_ctl := groupBox::new(This),
        gbResultStr_ctl:setText(" Result Open Command Line "),
        gbResultStr_ctl:setPosition(8, 68),
        gbResultStr_ctl:setSize(236, 40),
        resultCommandLine_ctl := textControl::new(gbResultStr_ctl),
        resultCommandLine_ctl:setText(""),
        resultCommandLine_ctl:setPosition(3, 2),
        resultCommandLine_ctl:setSize(228, 26).
% end of automatic code
end implement openTab
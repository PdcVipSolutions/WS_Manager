%

implement execTab
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
    editCmdString_P() = editCmdString_ctl.
    resultCommandLine_P() = resultCommandLine_ctl.
    getMacroSymbols_P() = getMacroSymbols_ctl.
    cbExecutePossible_P() = cbExecutePossible_ctl.
    editArgString_P() = editArgString_ctl.
    txtCommandLine_P() = txtCommandLine_ctl.
    gbResultStr_P() = gbResultStr_ctl.
    txtArguments_P() = txtArguments_ctl.
    txtFormatCommand_P() = txtFormatCommand_ctl.
    cbFEMode_P() = cbFEMode_ctl.

% This code is maintained automatically, do not update it manually. 13:51:57-14.9.2016

facts
    editCmdString_ctl : editControl.
    txtCommandLine_ctl : textControl.
    gbResultStr_ctl : groupBox.
    resultCommandLine_ctl : textControl.
    getMacroSymbols_ctl : button.
    cbExecutePossible_ctl : checkButton.
    editArgString_ctl : editControl.
    txtArguments_ctl : textControl.
    txtFormatCommand_ctl : textControl.
    cbFEMode_ctl : checkButton.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Execute"),
        This:setSize(252, 180),
        cbFEMode_ctl := checkButton::new(This),
        cbFEMode_ctl:setText("Perform at Frontend Side"),
        cbFEMode_ctl:setPosition(8, 20),
        cbFEMode_ctl:setWidth(236),
        editCmdString_ctl := editControl::new(This),
        editCmdString_ctl:setText(""),
        editCmdString_ctl:setPosition(56, 50),
        editCmdString_ctl:setWidth(176),
        getMacroSymbols_ctl := button::new(This),
        getMacroSymbols_ctl:setText("<<"),
        getMacroSymbols_ctl:setPosition(232, 50),
        getMacroSymbols_ctl:setSize(12, 12),
        getMacroSymbols_ctl:defaultHeight := false,
        editArgString_ctl := editControl::new(This),
        editArgString_ctl:setText(""),
        editArgString_ctl:setPosition(48, 66),
        editArgString_ctl:setWidth(196),
        gbResultStr_ctl := groupBox::new(This),
        gbResultStr_ctl:setText(" Result Execute Command Line "),
        gbResultStr_ctl:setPosition(8, 82),
        gbResultStr_ctl:setSize(236, 40),
        resultCommandLine_ctl := textControl::new(gbResultStr_ctl),
        resultCommandLine_ctl:setText(""),
        resultCommandLine_ctl:setPosition(3, 2),
        resultCommandLine_ctl:setSize(228, 26),
        txtCommandLine_ctl := textControl::new(This),
        txtCommandLine_ctl:setText("CommandLine:"),
        txtCommandLine_ctl:setPosition(8, 51),
        cbExecutePossible_ctl := checkButton::new(This),
        cbExecutePossible_ctl:setText("Execute command enabled"),
        cbExecutePossible_ctl:setPosition(8, 36),
        cbExecutePossible_ctl:setWidth(204),
        txtArguments_ctl := textControl::new(This),
        txtArguments_ctl:setText("Arguments:"),
        txtArguments_ctl:setPosition(8, 67),
        txtArguments_ctl:setSize(40, 10),
        txtFormatCommand_ctl := textControl::new(This),
        txtFormatCommand_ctl:setText("Format Command: [Command Line] [Arguments]"),
        txtFormatCommand_ctl:setPosition(8, 6),
        txtFormatCommand_ctl:setSize(236, 10).
% end of automatic code
end implement execTab
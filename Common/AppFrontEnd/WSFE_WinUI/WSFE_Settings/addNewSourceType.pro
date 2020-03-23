%

implement addNewSourceType
    inherits dialog
    open core, vpiDomains

facts
    is_Ok : boolean := false.
clauses
    getReturnValue() = if true = is_Ok then some(tuple(editName_ctl:getText(), editValue_ctl:getText())) else core::none end if.

clauses
    display(Parent, ValidFunctionName, TitleList) = Dialog:getReturnValue() :-
        Dialog = new(Parent, TitleList),
        Dialog:addValidateResponder(ValidFunctionName),
        Dialog:show().

constructors
    new : (window Parent, namedValue* TitleList).
clauses
    new(Parent, TitleList) :-
        dialog::new(Parent),
        generatedInitialize(),
        setTitles(TitleList).

clauses
    editName_P() = editName_ctl.
    editValue_P() = editValue_ctl.

predicates
    setTitles : (namedValue* TitleList).
clauses
    setTitles(TitleList):-
        setText(namedValue::getNamed_string(TitleList, "addNewSourceType")),
        ok_ctl:setText(namedValue::getNamed_string(TitleList, "pbOk")),
        cancel_ctl:setText(namedValue::getNamed_string(TitleList, "pbCancel")),
        help_ctl:setText(namedValue::getNamed_string(TitleList, "pbHelp")),
        name_ctl:setText(namedValue::getNamed_string(TitleList, "txtName")),
        value_ctl:setText(namedValue::getNamed_string(TitleList, "txtValue")).

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::defaultAction :-
        is_Ok := true.

% This code is maintained automatically, do not update it manually.
%  13:16:15-10.10.2018

facts
    editName_ctl : editControl.
    editValue_ctl : editControl.
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    name_ctl : textControl.
    value_ctl : textControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Add New Source Type"),
        setRect(rct(50, 40, 366, 91)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        editName_ctl := editControl::new(This),
        editName_ctl:setText(""),
        editName_ctl:setPosition(4, 16),
        editName_ctl:setWidth(76),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(188, 36),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("&Cancel"),
        cancel_ctl:setPosition(252, 36),
        cancel_ctl:setSize(56, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(4, 36),
        help_ctl:setWidth(56),
        help_ctl:defaultHeight := true,
        help_ctl:setAnchors([control::right, control::bottom]),
        help_ctl:setVisible(false),
        value_ctl := textControl::new(This),
        value_ctl:setText("Extensions List:"),
        value_ctl:setPosition(86, 4),
        value_ctl:setSize(68, 10),
        name_ctl := textControl::new(This),
        name_ctl:setText("Source Type Name:"),
        name_ctl:setPosition(4, 4),
        name_ctl:setSize(68, 10),
        editValue_ctl := editControl::new(This),
        editValue_ctl:setText(""),
        editValue_ctl:setPosition(84, 16),
        editValue_ctl:setWidth(224),
        editValue_ctl:setTabStop(false).
% end of automatic code
end implement addNewSourceType
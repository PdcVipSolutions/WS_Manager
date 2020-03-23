%

implement editVirtualDir inherits dialog
    open core, vpiDomains

facts
    is_Ok : boolean := false.
    messageFormatStr : string := "The directory '%s' not exist. Do you create it?".

clauses
    setVirtualDirectory(Name, Directory, TitleList) :-
        editName_ctl:setText(Name),
        if "" <> Name then
            editName_ctl:setReadOnly(true),
            setText(namedValue::getNamed_string(TitleList, "ttlEditVirtDir"))
        else
            setText(namedValue::getNamed_string(TitleList, "ttlCreateVirtDir")),
            editName_ctl:addModifiedListener(onEditNameModified),
            editValue_ctl:addModifiedListener(onEditNameModified)
        end if,
        ok_ctl:setText(namedValue::getNamed_string(TitleList, "pbOk")),
        cancel_ctl:setText(namedValue::getNamed_string(TitleList, "pbCancel")),
        help_ctl:setText(namedValue::getNamed_string(TitleList, "pbHelp")),
        browse_ctl:setText(namedValue::getNamed_string(TitleList, "pbBrowse")),
        name_ctl:setText(namedValue::getNamed_string(TitleList, "txtName")),
        value_ctl:setText(namedValue::getNamed_string(TitleList, "txtDir")),
        errorText_ctl:setText(namedValue::getNamed_string(TitleList, "txtError")),
        editValue_ctl:setText(Directory),
        messageFormatStr := namedValue::getNamed_string(TitleList, "msgFormat").

clauses
    getReturnValue() = if true = is_Ok then some(tuple(editName_ctl:getText(), editValue_ctl:getText())) else core::none end if.

clauses
    display(Parent, Name, Directory, TitleList) = Dialog:getReturnValue() :-
        Dialog = new(Parent),
        Dialog:setVirtualDirectory(Name, Directory, TitleList),
        Dialog:show().

constructors
    new : (window Parent).
clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize().

predicates
    onBrowseClick : button::clickResponder.
clauses
    onBrowseClick(_Source) = button::defaultAction :-
        CurrentDirectory = directory::getCurrentDirectory(),
        Answer = vpiDlgDir::getDirectoryName(getVpiWindow(), editValue_ctl:getText(), Directory),
        directory::setCurrentDirectory(CurrentDirectory),
        if b_true = Answer then
            editValue_ctl:setText(Directory),
            ok_ctl:setEnabled(true)
       end if.

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = Action :-
        Dir = editValue_ctl:getText(),
        not(directory::existExactDirectory(Dir)),
        !,
        Message = string::format(messageFormatStr, Dir),
        Answer = vpiCommonDialogs::messageBox("Warning", Message, mesbox_iconQuestion,
                    mesbox_buttonsYesNo, mesbox_defaultFirst, mesbox_suspendApplication),
        if idc_ok = Answer then
            directory::makeDirectoryPath(Dir),
            is_Ok := true,
            Action = button::defaultAction
        else
            Action = button::noAction
        end if.
    onOkClick(_Source) = button::defaultAction :-
        is_Ok := true.

predicates
    onEditNameModified : editControl::modifiedListener.
clauses
    onEditNameModified(_Source):-
        Name = editName_ctl:getText(),
        errorText_ctl:setVisible(false),
        if "" = Name or isNotValidName(Name) then
            ok_ctl:setEnabled(false),
            browse_ctl:setEnabled(false)
        else
            ok_ctl:setEnabled(toBoolean("" <> editValue_ctl:getText())),
            browse_ctl:setEnabled(true)
        end if.

predicates
    isNotValidName : (string Name) determ.
clauses
    isNotValidName(Name):-
        convert(virtualDirTab, getParent()):existName(string::concat("$(", Name, ")")),
        errorText_ctl:setVisible(true).

% This code is maintained automatically, do not update it manually.
%  13:15:21-10.10.2018

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    name_ctl : textControl.
    value_ctl : textControl.
    editName_ctl : editControl.
    editValue_ctl : editControl.
    browse_ctl : button.
    errorText_ctl : textControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Virtual Directory"),
        setRect(rct(50, 40, 422, 92)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        editName_ctl := editControl::new(This),
        editName_ctl:setText("Edit"),
        editName_ctl:setPosition(16, 16),
        editName_ctl:setWidth(68),
        editValue_ctl := editControl::new(This),
        editValue_ctl:setText("Edit"),
        editValue_ctl:setPosition(98, 16),
        editValue_ctl:setWidth(226),
        editValue_ctl:setTabStop(false),
        browse_ctl := button::new(This),
        browse_ctl:setText("&Browse..."),
        browse_ctl:setPosition(328, 16),
        browse_ctl:setSize(36, 12),
        browse_ctl:defaultHeight := false,
        browse_ctl:setAnchors([control::left, control::top, control::right]),
        browse_ctl:setClickResponder(onBrowseClick),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(244, 36),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("&Cancel"),
        cancel_ctl:setPosition(308, 36),
        cancel_ctl:setSize(56, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(156, 36),
        help_ctl:setWidth(56),
        help_ctl:defaultHeight := true,
        help_ctl:setAnchors([control::right, control::bottom]),
        help_ctl:setVisible(false),
        name_ctl := textControl::new(This),
        name_ctl:setText("Name:"),
        name_ctl:setPosition(16, 4),
        name_ctl:setSize(68, 10),
        value_ctl := textControl::new(This),
        value_ctl:setText("Directory:"),
        value_ctl:setPosition(98, 4),
        value_ctl:setSize(68, 10),
        Prefix_ctl = textControl::new(This),
        Prefix_ctl:setText("$("),
        Prefix_ctl:setPosition(4, 17),
        Prefix_ctl:setSize(10, 10),
        Prefix_ctl:setAlignment(alignRight),
        Suffix_ctl = textControl::new(This),
        Suffix_ctl:setText(")"),
        Suffix_ctl:setPosition(88, 17),
        Suffix_ctl:setSize(10, 10),
        errorText_ctl := textControl::new(This),
        errorText_ctl:setText("This Name exists!"),
        errorText_ctl:setPosition(16, 28),
        errorText_ctl:setSize(132, 10),
        errorText_ctl:setVisible(false).
    % end of automatic code

end implement editVirtualDir

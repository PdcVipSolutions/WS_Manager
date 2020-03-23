%

implement addErrorWarningWords
    inherits dialog
    open core, vpiDomains

facts
    is_Ok : optional{namedValue*} := core::none.
clauses
    getReturnValue() = is_Ok.

clauses
    display(Parent, WordsList, TitleList) = Dialog:getReturnValue() :-
        Dialog = new(Parent, WordsList, TitleList),
        Dialog:show().

facts
    titleDialogList_F : namedValue*.

clauses
    new(Parent, WordsList, TitleList) :-
        dialog::new(Parent),
        generatedInitialize(),
        titleDialogList_F := TitleList,
        setTitleControls(titleDialogList_F),
        initTabControl(WordsList).

predicates
    setTitleControls : (namedValue* TitleList).
clauses
    setTitleControls(TitleList):-
    new_ctl:setText(namedValue::getNamed_string(TitleList, "pbNew")),
    edit_ctl:setText(namedValue::getNamed_string(TitleList, "pbEdit")),
    delete_ctl:setText(namedValue::getNamed_string(TitleList, "pbDelete")),
    pbGroupDelete_ctl:setText(namedValue::getNamed_string(TitleList, "pbDelete")),
    pbGroupAdd_ctl:setText(namedValue::getNamed_string(TitleList, "pbAdd")),
    stKeywordGroup_ctl:setText(namedValue::getNamed_string(TitleList, "stGroupName")),
    !.

constants
    listControl_id = 2000.
predicates
    initTabControl : (namedValue* WordsList).
clauses
    initTabControl(WordsList):-
        foreach namedValue(GroupName, string(GroupWords)) in WordsList do
            GroupTab = tabPage::new(),
            GroupTab:setText(GroupName),
            ListControl = listBox::new(GroupTab:getContainerControl()),
            ListControl:setPosition(4, 4),
            ListControl:setSize(172, 160),
            ListControl:setCtrlId(listControl_id),
            WordList = toTerm(GroupWords),
            ListControl:addList(WordList),
            tabControl_ctl:addPage(GroupTab)
        end foreach.

predicates
    onNewClick : button::clickResponder.
clauses
    onNewClick(_Source) = button::defaultAction :-
        if
            TabPage = tabControl_ctl:getCurrentPage(),
            Title = TabPage:getText(),
            ListBox = convert(listbox, TabPage:lookupControl_dt(listControl_id)),
            TitleFormat = namedValue::getNamed_string(titleDialogList_F, "txtFormatAddWord"),
            PromptFormat = namedValue::getNamed_string(titleDialogList_F, "txtPromptWord"),
            NewWord = commonDialogs::tryGet_string(getParent(), string::format(TitleFormat,Title), "", string::format(PromptFormat, Title), "", 200, {= _ :- fail})
        then
            ListBox:add(NewWord),
            Length = list::length(ListBox:getAll()),
            ListBox:selectAt(Length - 1, true)
        end if.

predicates
    onEditClick : button::clickResponder.
clauses
    onEditClick(_Source) = button::defaultAction :-
        if
            TabPage = tabControl_ctl:getCurrentPage(),
            Title = TabPage:getText(),
            ListBox = convert(listbox, TabPage:lookupControl_dt(listControl_id)),
            ListBox:getAllSelected(SelectedItemList, IndexList),
            SelectedItemList = [Word], IndexList = [Index],
            WordList = ListBox:getAll(),
            TitleFormat = namedValue::getNamed_string(titleDialogList_F, "txtFormatEditWord"),
            PromptFormat = namedValue::getNamed_string(titleDialogList_F, "txtPromptWord"),
            NewWord = commonDialogs::tryGet_string(getParent(), string::format(TitleFormat, Title), "", string::format(PromptFormat, Title), Word, 200, {= _ :- fail})
        then
            NewList = list::setNth(Index, WordList, NewWord),
            ListBox:clearAll(),
            ListBox:addList(NewList),
            ListBox:selectAt(Index, true)
        end if.

predicates
    onDeleteClick : button::clickResponder.
clauses
    onDeleteClick(_Source) = button::defaultAction :-
        if
            TabPage = tabControl_ctl:getCurrentPage(),
%            Title = TabPage:getText(),
            ListBox = convert(listbox, TabPage:lookupControl_dt(listControl_id)),
            ListBox:getAllSelected(SelectedItemList, IndexList),
            SelectedItemList = [Word], IndexList = [Index],
            WordList = ListBox:getAll()
        then
            NewList = list::remove(WordList, Word),
            ListBox:clearAll(),
            ListBox:addList(NewList),
            NewLength = list::length(NewList),
            if Index < NewLength then
                ListBox:selectAt(Index, true)
            elseif Index = NewLength and Index <> 0 then
                ListBox:selectAt(Index - 1, true)
            end if
        end if.

predicates
    onAddClick : button::clickResponder.
clauses
    onAddClick(_Source) = button::defaultAction :-
        if
            NewGroup = commonDialogs::tryGet_string(getParent(), "Add new Group", "", "Group name: ", "", 200, {= _ :- fail})
        then
            GroupTab = tabPage::new(),
            GroupTab:setText(NewGroup),
            ListControl = listBox::new(GroupTab:getContainerControl()),
            ListControl:setPosition(4, 4),
            ListControl:setSize(172, 160),
            ListControl:setCtrlId(listControl_id),
            ListControl:addList([]),
            tabControl_ctl:addPage(GroupTab),
            tabControl_ctl:setCurrentPage(GroupTab)
        end if.

predicates
    onDelGroupClick : button::clickResponder.
clauses
    onDelGroupClick(_Source) = button::defaultAction :-
        TabPage = tabControl_ctl:getCurrentPage(),
        GroupName = TabPage:getText(),
        MessageFormat = namedValue::getNamed_string(titleDialogList_F, "txtDeleteGroup"),
        Message = string::format(MessageFormat, GroupName),
        Answer = vpiCommonDialogs::messageBox(namedValue::getNamed_string(titleDialogList_F, "ttlWarning"), Message, mesbox_iconQuestion,
                    mesbox_buttonsYesNo, mesbox_defaultFirst, mesbox_suspendApplication),
        if idc_ok = Answer then
            tabControl_ctl:removePage(TabPage)
        end if.

predicates
    onOkClick : button::clickResponder.
clauses
    onOkClick(_Source) = button::defaultAction :-
        is_Ok := some([namedValue(Title, string(WordList))||
            TabPage = tabControl_ctl:getPage_nd(),
            Title = TabPage:getText(),
            ListBox = convert(listbox, TabPage:lookupControl_dt(listControl_id)),
            WordList = toString(ListBox:getAll())
            ]).


% This code is maintained automatically, do not update it manually.
%  14:42:30-31.12.2018

facts
    ok_ctl : button.
    cancel_ctl : button.
    tabControl_ctl : tabcontrol.
    new_ctl : button.
    edit_ctl : button.
    delete_ctl : button.
    pbGroupAdd_ctl : button.
    pbGroupDelete_ctl : button.
    stKeywordGroup_ctl : textControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("addErrorWarningWords"),
        setRect(rct(50, 40, 242, 284)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        tabControl_ctl := tabcontrol::new(This),
        tabControl_ctl:setPosition(4, 4),
        tabControl_ctl:setSize(184, 182),
        new_ctl := button::new(This),
        new_ctl:setText("&New..."),
        new_ctl:setPosition(28, 208),
        new_ctl:setSize(48, 12),
        new_ctl:defaultHeight := false,
        new_ctl:setAnchors([control::left, control::bottom]),
        new_ctl:setClickResponder(onNewClick),
        edit_ctl := button::new(This),
        edit_ctl:setText("&Edit..."),
        edit_ctl:setPosition(84, 208),
        edit_ctl:setSize(48, 12),
        edit_ctl:defaultHeight := false,
        edit_ctl:setAnchors([control::left, control::bottom]),
        edit_ctl:setClickResponder(onEditClick),
        delete_ctl := button::new(This),
        delete_ctl:setText("&Delete"),
        delete_ctl:setPosition(140, 208),
        delete_ctl:setSize(48, 12),
        delete_ctl:defaultHeight := false,
        delete_ctl:setAnchors([control::left, control::bottom]),
        delete_ctl:setClickResponder(onDeleteClick),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(84, 228),
        ok_ctl:setSize(48, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        ok_ctl:setClickResponder(onOkClick),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(140, 228),
        cancel_ctl:setSize(48, 12),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        pbGroupAdd_ctl := button::new(This),
        pbGroupAdd_ctl:setText("&Add..."),
        pbGroupAdd_ctl:setPosition(84, 190),
        pbGroupAdd_ctl:setSize(48, 12),
        pbGroupAdd_ctl:defaultHeight := false,
        pbGroupAdd_ctl:setAnchors([control::left, control::bottom]),
        pbGroupAdd_ctl:setClickResponder(onAddClick),
        pbGroupDelete_ctl := button::new(This),
        pbGroupDelete_ctl:setText("De&lete"),
        pbGroupDelete_ctl:setPosition(140, 190),
        pbGroupDelete_ctl:setSize(48, 12),
        pbGroupDelete_ctl:defaultHeight := false,
        pbGroupDelete_ctl:setAnchors([control::left, control::bottom]),
        pbGroupDelete_ctl:setClickResponder(onDelGroupClick),
        stKeywordGroup_ctl := textControl::new(This),
        stKeywordGroup_ctl:setText("Keyword Group"),
        stKeywordGroup_ctl:setPosition(8, 191),
        stKeywordGroup_ctl:setSize(72, 10),
        stKeywordGroup_ctl:setAlignment(alignRight).
% end of automatic code
end implement addErrorWarningWords
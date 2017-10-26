%

implement generalTab
    inherits userControlSupport
    open core, vpiDomains, listViewControl

domains
    langList = tuple{boolean,string,string}*.

facts
    languageList_V : langList := [].

clauses
    listViewControl_P() = listViewControl_ctl.
    pbFontColor_P() = pbFontColor_ctl.
    staticText_P() = staticText_ctl.
    pbBGColor_P() = pbBGColor_ctl.
    stCurrentLang_P() = stCurrentLang_ctl.
    languageList_P() = languageList_ctl.

constants
    columnList : listViewControl::column* = [column("Source File", 200, alignleft), column("Color Value", 102, alignleft)].

clauses
    new(Parent, SourceColorsList, Languages):-
        new(),
        setContainer(Parent),
        listViewControl_ctl:insertColumnList(1, columnList),
        listViewControl_ctl:setLvType(lvs_report),
        listViewControl_ctl:setLvExStyle(listViewControl::lvs_ex_labeltip),
        listViewControl_ctl:setStyle([lvs_showselalways, lvs_editlabels, lvs_autoarrange, lvs_singlesel]),
        listViewControl_ctl:setState([wsf_border]),
        cursorSet(cursor_Wait),
        ListViewItems =
            [ listViewControl::item(ItemID, Name, 0, [], [string::format("0x%06X", FGColor)])
            ||
                namedValue(Name, string(ColorsStr)) in SourceColorsList,
                tuple(FGColor, BGColor) = toTerm(tuple{integer, integer}, ColorsStr),
                ItemID = getNextId(),
                assert(rowColor(ItemID, FGColor, BGColor))
            ],
       listViewControl_ctl:insertItemList(ListViewItems),
       listViewControl_ctl:customDraw := some(anotherColorForFailed),
       languageList_V := toTerm(langList, Languages),
       LangNameList = [Name || tuple(_, _, Name) in languageList_V],
       languageList_ctl:addList(LangNameList),
       if Index = list::tryGetIndexEq(isCurrentLang, true, languageList_V) then
            languageList_ctl:selectAt(Index, true)
        end if.

predicates
    isCurrentLang : (boolean, tuple{boolean,string,string}) determ.
clauses
    isCurrentLang(IsSelect, tuple(IsSelect,_,_)).

clauses
    tryGetNewLanguage() = NewLangCode :-
        NewIndex = languageList_ctl:tryGetSelectedIndex(),
        StartIndex = list::tryGetIndexEq(isCurrentLang, true, languageList_V),
        NewIndex <> StartIndex,
        !,
        tuple(_, NewLangCode, _) = list::nth(NewIndex, languageList_V).

clauses
    new():-
        userControlSupport::new(),
        generatedInitialize().

facts
    lastId : unsignedNative := 20.
    rowColor : (listViewControl::itemId ItemID,integer FGColor,integer BGColor).

clauses
    getNewSourceColors() =
        [namedValue(listViewControl_ctl:getItemColumnText(listViewControl_ctl:tryGetItemIndex(ItemID), 0), string(toString(tuple(FGColor, BGColor)))) ||
            rowColor(ItemID, FGColor, BGColor)].

predicates
    getNextId : () -> listViewControl::itemId.
clauses
    getNextId() = uncheckedConvert(listViewControl::itemId, lastId) :-
        lastId := lastId + 1.

predicates
    anotherColorForFailed : listViewControl::customDraw.
clauses
    anotherColorForFailed(_ListViewControl,
        nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_prepaint, _, _, _ItemHandle, _, _),_TextForeground, _TextBackground, _SubItem, _, _, _, _, _, _, _, _)
        ) = window::nativeResult(gui_native::cdrf_notifyitemdraw) :-
        !.
    anotherColorForFailed(_ListViewControl,
        nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_itemprepaint, _, _, _ItemHandle, _, _),_TextForeground, _TextBackground, _SubItem, _, _, _, _, _, _, _, _)
        ) = window::nativeResult(gui_native::cdrf_notifysubitemdraw) :-
        !.
    anotherColorForFailed(ListViewControl, NmLvCustomDraw) = _ :-
        nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_itemprepaint+gui_native::cdds_subitem, _, _, ItemSpec, _, _), _TextForeground, _TextBackground, 1, _, _, _, _, _, _, _, _) = NmLvCustomDraw,
        ItemId = ListViewControl:getItemId(convert(unsigned, uncheckedConvert(unsignedNative, ItemSpec))),
        if
            rowColor(ItemID, FGColor, BGColor)
        then
            memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), FGColor),
            memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)+sizeOfDomain(color)), BGColor)
        end if,
        fail.
    anotherColorForFailed(_Source, _NmLvCustomDraw) = window::nativeResult(gui_native::cdrf_dodefault).

predicates
    onPbFontColorClick : button::clickResponder.
clauses
    onPbFontColorClick(_Source) = button::defaultAction :-
        if
            [ItemID] = listViewControl_ctl:getSel(),
            rowColor(ItemID, FGColor, BGColor),
            NewColor = vpiCommonDialogs::getColor(FGColor)
        then
            retractAll(rowColor(ItemID, _, _)),
            assert(rowColor(ItemID, NewColor, BGColor)),
            listViewControl_ctl:updateTextSubItem(ItemID, 0, string::format("0x%06X", NewColor)),
            vpi::winInvalidate(listViewControl_ctl:getVpiWindow())
        end if.

predicates
    onPbBGColorClick : button::clickResponder.
clauses
    onPbBGColorClick(_Source) = button::defaultAction :-
        if
            [ItemID] = listViewControl_ctl:getSel(),
            rowColor(ItemID, FGColor, BGColor),
            NewColor = vpiCommonDialogs::getColor(BGColor)
        then
            retractAll(rowColor(ItemID, _, _)),
            assert(rowColor(ItemID, FGColor, NewColor)),
            vpi::winInvalidate(listViewControl_ctl:getVpiWindow())
        end if.

predicates
    onListViewControlMouseClick : listViewControl::mouseClickListener.
clauses
    onListViewControlMouseClick(Source, PNT):-
        if
            rowColor(ItemID, _FGColor, _BGColor),
            RCT = Source:getItemRect(ItemID, bounds),
            gui::rectPntInside(RCT, PNT)
        then
            listViewControl_ctl:select([ItemId], true)
        end if.

predicates
    onListViewControlColumnClick : listViewControl::columnClickListener.
clauses
    onListViewControlColumnClick(_Source, _ColumnNumber).

% This code is maintained automatically, do not update it manually. 14:55:34-30.9.2017

facts
    listViewControl_ctl : listviewcontrol.
    staticText_ctl : textControl.
    pbFontColor_ctl : button.
    pbBGColor_ctl : button.
    stCurrentLang_ctl : textControl.
    languageList_ctl : listBox.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Misc"),
        This:setSize(380, 212),
        listViewControl_ctl := listviewcontrol::new(This),
        listViewControl_ctl:setPosition(4, 16),
        listViewControl_ctl:setSize(176, 84),
        listViewControl_ctl:addColumnClickListener(onListViewControlColumnClick),
        listViewControl_ctl:addMouseClickListener(onListViewControlMouseClick),
        staticText_ctl := textControl::new(This),
        staticText_ctl:setText("Font Color of Source List:"),
        staticText_ctl:setPosition(4, 4),
        staticText_ctl:setSize(88, 10),
        pbFontColor_ctl := button::new(This),
        pbFontColor_ctl:setText("Set FG Color..."),
        pbFontColor_ctl:setPosition(4, 102),
        pbFontColor_ctl:setSize(80, 12),
        pbFontColor_ctl:defaultHeight := false,
        pbFontColor_ctl:setClickResponder(onPbFontColorClick),
        pbBGColor_ctl := button::new(This),
        pbBGColor_ctl:setText("Set BG Color..."),
        pbBGColor_ctl:setPosition(100, 102),
        pbBGColor_ctl:setSize(80, 12),
        pbBGColor_ctl:defaultHeight := false,
        pbBGColor_ctl:setClickResponder(onPbBGColorClick),
        stCurrentLang_ctl := textControl::new(This),
        stCurrentLang_ctl:setText("UI Language"),
        stCurrentLang_ctl:setPosition(188, 4),
        stCurrentLang_ctl:setSize(100, 10),
        languageList_ctl := listBox::new(This),
        languageList_ctl:setPosition(188, 16),
        languageList_ctl:setSize(104, 84).
% end of automatic code
end implement generalTab
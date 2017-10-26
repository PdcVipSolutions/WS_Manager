%

implement virtualDirList inherits dialog inherits wsFE_Connector
    open core, vpiDomains, listViewControl, ws_EventManager

constants
    vip_C : binary = #bininclude(@"WS_manager\Common\AppFrontEnd\Icons\VISUAL.ICO").
    sm_C : binary = #bininclude(@"WS_manager\Common\AppFrontEnd\Icons\Solution.ico").

constants
    imageWidth_C = 16.
    imageHeight_C = 16.

facts
    lastId : unsignedNative := 10.
    vip_P : integer.
    sm_P : integer.

predicates
    getNextId : () -> listViewControl::itemId.
clauses
    getNextId() = uncheckedConvert(listViewControl::itemId, lastId) :-
        lastId := lastId + 1.

clauses
    setVirtualDirList(PerformResultParams) :-
        virtualDirListView_ctl:deleteAllItems(),
        cursorSet(cursor_Wait),
        ListViewItems =
            [ listViewControl::item(getNextId(), Name,
                    if true = IsVip then
                        vip_P
                    else
                        sm_P
                    end if, [], [MacroDir])
            ||
                namedValue(Name, string(Value)) in PerformResultParams,
                tuple(MacroDir, IsVip) = toTerm(tuple{string, boolean}, Value)
            ],
        virtualDirListView_ctl:insertItemList(ListViewItems).

clauses
    existName(Name):-
        MacroName = list::getMember_nd(list::map(virtualDirListView_ctl:getAll(), {(ItemId) = ItemName :-
            item(_, ItemName, _, _, _) == virtualDirListView_ctl:getItem(ItemId)})),
        equal = string::compareIgnoreCase(Name, MacroName),
        !.

clauses
    display(Parent, WS_FrontEnd, PerformResultParams) = Dialog :-
        Dialog = new(Parent, WS_FrontEnd),
        Dialog:setVirtualDirList(PerformResultParams),
        Dialog:show().

constants
    columnList : listViewControl::column* = [column("Name", 200, alignleft), column("MacroDir Value", 400, alignleft)].

constructors
    new : (window Parent, ws_FrontEnd).
clauses
    new(Parent, WS_FrontEnd) :-
        wsFE_Connector::new(WS_FrontEnd),
        dialog::new(Parent),
        generatedInitialize(),
        virtualDirListView_ctl:insertColumnList(1, columnList),
        virtualDirListView_ctl:setLvType(lvs_report),
        virtualDirListView_ctl:setStyle([lvs_showselalways, lvs_editlabels, lvs_autoarrange, lvs_singlesel]),
        ImageList = imageList::new(imageWidth_C, imageHeight_C),
        virtualDirListView_ctl:imageList := ImageList,
        vip_P := ImageList:addGDIplusBitmap(bitmap::createFromBinary(vip_C)),
        sm_P := ImageList:addGDIplusBitmap(bitmap::createFromBinary(sm_C)).

predicates
    onNewClick : button::clickResponder.
clauses
    onNewClick(_Source) = button::defaultAction :-
        ReturnValue = editVirtualDir::display(This, "", "", []),
        if tuple(Name, NewDirValue) = isSome(ReturnValue) then
            virtualDirListView_ctl:insertItem(listViewControl::item(getNextId(), string::concat("$(", Name, ")"), sm_P, [], [NewDirValue])),
            notify(methodRequest,ws_EventManager::insertVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.

predicates
    onEditClick : button::clickResponder.
clauses
    onEditClick(_Source) = button::defaultAction :-
        ItemId = virtualDirListView_ctl:getFocus(),
        item(ItemId, MacroName, BitmapIdx, Flags, [DirValue]) = virtualDirListView_ctl:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        ReturnValue = editVirtualDir::display(This, Name, DirValue, []),
        if tuple(_, NewDirValue) = isSome(ReturnValue) and equal <> string::compareIgnoreCase(NewDirValue, DirValue) then
            virtualDirListView_ctl:updateItem(item(ItemId, MacroName, BitmapIdx, Flags, [NewDirValue])),
            notify(methodRequest,ws_EventManager::updateVirtualDir_C, [namedValue(Name,string(NewDirValue))])
        end if.

    onEditClick(_Source) = button::defaultAction.

predicates
    onDeleteClick : button::clickResponder.
clauses
    onDeleteClick(_Source) = button::defaultAction :-
        ItemId = virtualDirListView_ctl:getFocus(),
        item(_, MacroName, _, _, _) = virtualDirListView_ctl:tryGetItem(ItemId),
        string::hasPrefix(MacroName, "$(", Rest),
        string::hasSuffix(Rest, ")", Name),
        !,
        Message = string::format("Are you sure you want to delete virtual directory '%s'?", MacroName),
        Answer = vpiCommonDialogs::messageBox("Warning", Message, mesbox_iconQuestion,
                    mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
        if idc_ok = Answer then
            notify(methodRequest,ws_EventManager::deleteVirtualDir_C, [namedValue(Name, core::none)]),
            virtualDirListView_ctl:deleteItem(ItemId)
        end if.
    onDeleteClick(_Source) = button::defaultAction.

predicates
    onVirtualDirListViewSelectEnd : listViewControl::selectEndListener.
clauses
    onVirtualDirListViewSelectEnd(_Source, ItemId, _Select) :-
        virtualDirListView_ctl:getItem(ItemId) = item(_, _, IconId, _, _),
        edit_ctl:setEnabled(toBoolean(sm_P = IconId)),
        delete_ctl:setEnabled(toBoolean(sm_P = IconId)).

predicates
    onVirtualDirListViewMouseDblClick : listViewControl::mouseDblClickListener.
clauses
    onVirtualDirListViewMouseDblClick(Source, _Point):-
        N = Source:getFocus(),
        if listViewControl::item(_, _, sm_P, _, _) = Source:tryGetItem(N) then
            _ = onEditClick(edit_ctl)
        end if.

constants
    minWidth:integer = 284.
    minHeight:integer = 110.
predicates
    onSize : window::sizeListener.
clauses
    onSize(_Source):-
        if getWidth() < minWidth then
            setWidth(minWidth)
        end if,
        if getHeight() < minHeight then
            setHeight(minHeight)
        end if.

% This code is maintained automatically, do not update it manually. 17:50:41-7.6.2016

facts
    ok_ctl : button.
    virtualDirListView_ctl : listviewcontrol.
    new_ctl : button.
    edit_ctl : button.
    delete_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Virtual Directory"),
        setRect(rct(50, 40, 334, 150)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setBorder(sizeBorder()),
        setState([wsf_NoClipSiblings]),
        addSizeListener(onSize),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&Close"),
        ok_ctl:setPosition(224, 94),
        ok_ctl:setSize(56, 12),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        virtualDirListView_ctl := listviewcontrol::new(This),
        virtualDirListView_ctl:setPosition(4, 4),
        virtualDirListView_ctl:setSize(276, 86),
        virtualDirListView_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        virtualDirListView_ctl:addMouseDblClickListener(onVirtualDirListViewMouseDblClick),
        virtualDirListView_ctl:addSelectEndListener(onVirtualDirListViewSelectEnd),
        new_ctl := button::new(This),
        new_ctl:setText("&New..."),
        new_ctl:setPosition(4, 94),
        new_ctl:setSize(56, 12),
        new_ctl:defaultHeight := false,
        new_ctl:setAnchors([control::left, control::bottom]),
        new_ctl:setClickResponder(onNewClick),
        edit_ctl := button::new(This),
        edit_ctl:setText("&Edit..."),
        edit_ctl:setPosition(68, 94),
        edit_ctl:setSize(56, 12),
        edit_ctl:defaultHeight := false,
        edit_ctl:setAnchors([control::left, control::bottom]),
        edit_ctl:setClickResponder(onEditClick),
        delete_ctl := button::new(This),
        delete_ctl:setText("&Delete"),
        delete_ctl:setPosition(132, 94),
        delete_ctl:setSize(56, 12),
        delete_ctl:defaultHeight := false,
        delete_ctl:setAnchors([control::left, control::bottom]),
        delete_ctl:setClickResponder(onDeleteClick).
    % end of automatic code

end implement virtualDirList

%

implement virtualDirTab
    inherits userControlSupport
    open core, vpiDomains, listViewControl

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

constants
    columnList : listViewControl::column* = [column("Name", 200, alignleft), column("MacroDir Value", 400, alignleft)].

clauses
    new(Parent, PerformResultParams):-
        userControlSupport::new(),
        generatedInitialize(),
        setContainer(Parent),
        virtualDirListView_ctl:insertColumnList(1, columnList),
        virtualDirListView_ctl:setLvType(lvs_report),
        virtualDirListView_ctl:setStyle([lvs_showselalways, lvs_editlabels, lvs_autoarrange, lvs_singlesel]),
        virtualDirListView_ctl:setState([wsf_border]),
        ImageList = imageList::new(imageWidth_C, imageHeight_C),
        virtualDirListView_ctl:imageList := ImageList,
        vip_P := ImageList:addGDIplusBitmap(bitmap::createFromBinary(vip_C)),
        sm_P := ImageList:addGDIplusBitmap(bitmap::createFromBinary(sm_C)),
        setVirtualDirList(PerformResultParams).

% This code is maintained automatically, do not update it manually. 13:38:43-29.7.2016

facts
    virtualDirListView_ctl : listviewcontrol.
    new_ctl : button.
    edit_ctl : button.
    delete_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Virtual Directory"),
        This:setSize(380, 212),
        virtualDirListView_ctl := listviewcontrol::new(This),
        virtualDirListView_ctl:setPosition(4, 4),
        virtualDirListView_ctl:setSize(368, 192),
        virtualDirListView_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        new_ctl := button::new(This),
        new_ctl:setText("&New..."),
        new_ctl:setPosition(4, 198),
        new_ctl:setSize(56, 12),
        new_ctl:defaultHeight := false,
        new_ctl:setAnchors([control::left, control::bottom]),
        edit_ctl := button::new(This),
        edit_ctl:setText("&Edit..."),
        edit_ctl:setPosition(68, 198),
        edit_ctl:setSize(56, 12),
        edit_ctl:defaultHeight := false,
        edit_ctl:setAnchors([control::left, control::bottom]),
        delete_ctl := button::new(This),
        delete_ctl:setText("&Delete"),
        delete_ctl:setPosition(132, 198),
        delete_ctl:setSize(56, 12),
        delete_ctl:defaultHeight := false,
        delete_ctl:setAnchors([control::left, control::bottom]).
% end of automatic code
end implement virtualDirTab
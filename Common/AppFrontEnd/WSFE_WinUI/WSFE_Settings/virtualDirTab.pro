%

implement virtualDirTab
    inherits userControlSupport
    open core, vpiDomains, listViewControl

constants
    vip_C : binary = #bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\VISUAL.ICO").
    sm_C : binary = #bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\wsm.ico").

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

clauses
    virtualDirListView_P() = virtualDirListView_ctl.
    new_P() = new_ctl.
    edit_P() = edit_ctl.
    delete_P() = delete_ctl.
    stWSVariable_P() = stWSVariable_ctl.
    edWSVariableFile_P() = edWSVariableFile_ctl.
    pbBrowse_P() = pbBrowse_ctl.

% This code is maintained automatically, do not update it manually.
%  12:33:57-7.12.2018

facts
    virtualDirListView_ctl : listviewcontrol.
    new_ctl : button.
    edit_ctl : button.
    delete_ctl : button.
    stWSVariable_ctl : textControl.
    edWSVariableFile_ctl : editControl.
    pbBrowse_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("Virtual Directory"),
        This:setSize(380, 250),
        virtualDirListView_ctl := listviewcontrol::new(This),
        virtualDirListView_ctl:setPosition(4, 22),
        virtualDirListView_ctl:setSize(368, 208),
        virtualDirListView_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        new_ctl := button::new(This),
        new_ctl:setText("&New..."),
        new_ctl:setPosition(4, 234),
        new_ctl:setSize(56, 12),
        new_ctl:defaultHeight := false,
        new_ctl:setAnchors([control::left, control::bottom]),
        edit_ctl := button::new(This),
        edit_ctl:setText("&Edit..."),
        edit_ctl:setPosition(68, 234),
        edit_ctl:setSize(56, 12),
        edit_ctl:defaultHeight := false,
        edit_ctl:setAnchors([control::left, control::bottom]),
        delete_ctl := button::new(This),
        delete_ctl:setText("&Delete"),
        delete_ctl:setPosition(132, 234),
        delete_ctl:setSize(56, 12),
        delete_ctl:defaultHeight := false,
        delete_ctl:setAnchors([control::left, control::bottom]),
        stWSVariable_ctl := textControl::new(This),
        stWSVariable_ctl:setText("WS Variable File:"),
        stWSVariable_ctl:setPosition(4, 5),
        stWSVariable_ctl:setSize(60, 10),
        edWSVariableFile_ctl := editControl::new(This),
        edWSVariableFile_ctl:setText(""),
        edWSVariableFile_ctl:setPosition(68, 4),
        edWSVariableFile_ctl:setWidth(288),
        edWSVariableFile_ctl:setReadOnly(),
        pbBrowse_ctl := button::new(This),
        pbBrowse_ctl:setText("..."),
        pbBrowse_ctl:setPosition(360, 4),
        pbBrowse_ctl:setSize(12, 12),
        pbBrowse_ctl:defaultHeight := false.
% end of automatic code
end implement virtualDirTab
%

implement wSFE_SourceList
    inherits wsFE_Connector
    open core, vpiDomains, listViewControl, ws_eventManager, window

facts
    sourceList_P:listviewcontrol:=erroneous.
    sourceItemGroup : (itemId, tuple{string, string, string}).
    localItemStatus : (itemId, string).

    vip_P : integer:=erroneous.
    vip_bw_P : integer:=erroneous.
    txt_P : integer:=erroneous.
    txt_light_P : integer:=erroneous.
    pzl_bk_P : integer:=erroneous.
    pzl_wt_P : integer:=erroneous.

    slDropSite : dropsite.
    trDropSite : dropsite.
    dragMoveItems : tuple{itemId, itemId} := erroneous.
    dragMoveToTree : tuple{any, treeNode_std} := erroneous.

    rowColor : (string SourceType,integer FGColor,integer BGColor).

constants
    vip_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\VISUAL.png").
    vip_bw_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\VISUAL-light.png").
    txt_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\doc-text-inv-16-000000.png").
    txt_light_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\doc-text-16-000000.png").
    pzl_bk_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\icon_puzzle-16-000000.png").
    pzl_wt_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\icon_puzzle_alt-16-000000.png").

constants
    imageWidth_C=16.
    imageHeight_C=16.

clauses
    setSourceItemGroup(ItemId, Value):-
        assert(sourceItemGroup(ItemId, Value)).

clauses
    tryGetSourceItemGroup(ItemId) = Value :-
        sourceItemGroup(ItemId, Value),
        !.

clauses
    clearSourceItemGroup():-
        retractall(sourceItemGroup(_, _)).

clauses
    getItemStatus(ItemId) = LocalStatus :-
        if localItemStatus(ItemId, LocalStatus) then
        else
            LocalStatus = emptyString_C
        end if.

clauses
    clearLocalStatus():-
        retractall(localItemStatus(_, _)).

clauses
    setSourceColors(SourceColorsList):-
        retractAll(rowColor(_, _, _)),
        foreach
            namedValue(Name, string(ColorsStr)) in SourceColorsList,
            tuple(FGColor, BGColor) = toTerm(tuple{integer, integer}, ColorsStr)
        do
            assert(rowColor(Name, FGColor, BGColor))
        end foreach,
        vpi::winInvalidate(sourceList_P:getVpiWindow()).

domains
    dragItem = dragItem(itemId SourceId).
    dragMulti = dragMulti(itemId* SourceId).
predicates
    dragBeginSourceList : dragBeginResponder.
clauses
    dragBeginSourceList(Source, _ItemIndex, _MouseButton):-
        SelList = sourceList_P:getSel(),
        if
            SelList = [SourceID],
            slDropSite:tipText := tryGetSelectSourceFileName(),
            listViewControl::item(SourceID, _FileName, ImageId, _, [_Path, _OldStatus, _OldErrWarn, _OldDateTime]) = sourceList_P:getItem(SourceID)
        then
            if ImageId =  vip_P then DragBitmap = bitmap::createFromBinary(vip_C)
            elseif ImageId =  vip_bw_P then DragBitmap = bitmap::createFromBinary(vip_bw_C)
            elseif ImageId =  txt_P then DragBitmap = bitmap::createFromBinary(txt_C)
            elseif ImageId =  txt_light_P then DragBitmap = bitmap::createFromBinary(txt_light_C)
            elseif ImageId =  pzl_bk_P then DragBitmap = bitmap::createFromBinary(pzl_bk_C)
            elseif ImageId =  pzl_wt_P then DragBitmap = bitmap::createFromBinary(pzl_wt_C)
            else
                DragBitmap = bitmap::createFromBinary(#bininclude(@"pfc\gui\controls\ribbonControl\ribbonDesignerDlg\Icons\Section.png"))
            end if,
            DragItemList = [toAny(dragItem(SourceId))],
            dragNdrop::beginDrag(Source:getParent(), DragItemList,
                dragNdrop::dragShape(DragBitmap, tuple(dragNdrop::relative(1 / 8), dragNdrop::relative(7 / 8))))
        elseif SelList <> [] then
            slDropSite:tipText := ws_Events():getString(tipDnD_C),
            DragBitmap = bitmap::createFromBinary(#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\DndToTree16.png")),
            DragItemList = [toAny(dragMulti(SelList))],
            dragNdrop::beginDrag(Source:getParent(), DragItemList,
                dragNdrop::dragShape(DragBitmap, tuple(dragNdrop::relative(1 / 8), dragNdrop::relative(7 / 8))))
        end if.

predicates
    onSLDrop : (dropsite DropAt, window Source, any* ItemList, dragNdrop::dragShape, vpiDomains::pnt DropSitePnt,
        vpiDomains::keyModifier ModifierKeys).
clauses
    onSLDrop(_DropAt, _Source, AnyItemList, _DragShape, pnt(X,Y), _ModifierKeys) :-
        if
            [Item] = AnyItemList,
            dragItem(SourceItemId) = tryToTerm(dragItem, Item),
            sourceList_P:getItemPosition(X, Y, TargetItemId, _),
            SourceItemId <> TargetItemId,
            sourceItemGroup(SourceItemId, tuple(_, _, SPath)),
            sourceItemGroup(TargetItemId, tuple(_, TargetName, TPath)),
            if
                SPath <> TPath,
                Message = string::format("% '%s'?", ws_Events():getString(qstMoveSource), TargetName),
                Answer = vpiCommonDialogs::messageBox(ws_Events():getString(questionTitle_C), Message, mesbox_iconQuestion,
                            mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
                idc_cancel = Answer
            then
                fail
            else
                succeed
            end if
        then
            dragMoveItems := tuple(SourceItemId, TargetItemId),
            showPopup(pnt(X,Y), dragNdropOperation_V)
        end if.

predicates
    onSLDrag : (dropsite DropAt, window Source, any* ItemList, dragNdrop::dragShape, vpiDomains::pnt DropSitePnt,
        vpiDomains::keyModifier ModifierKeys).
clauses
    onSLDrag(_DropAt, _Source, AnyItemList, _DragShape, pnt(X, Y), _ModifierKeys) :-
        sourceList_P:getItemPosition(X, Y, ItemId, _),
        if
            ItemId <> itemId_null,
            isDropPossible(ItemId, AnyItemList)
        then
            slDropSite:dropAllowed := true,
            SelList = sourceList_P:getSel(),
            sourceList_P:select(SelList, false),
            sourceList_P:select([ItemId], true)
        else
            slDropSite:dropAllowed := false
        end if.

predicates
    isDropPossible : (itemId ItemId, any* AnyItemList) determ.
clauses
    isDropPossible(ItemId, AnyItemList):-
        [Item] = AnyItemList,
        dragItem(SourceItemId) = tryToTerm(dragItem, Item),
        sourceItemGroup(SourceItemId, tuple(SNodeType, _, SPath)),
        sourceItemGroup(ItemId, tuple(TNodeType, _, TPath)),
        SNodeType1 = if SNodeType = workSpace_C then groupNode_C else SNodeType end if,
        TNodeType1 = if TNodeType = workSpace_C then groupNode_C else TNodeType end if,
        !,
        if SNodeType1 <> TNodeType1 then
            fail
        elseif TNodeType = folder_C and SPath <> TPath then
            fail
        end if.

predicates
    onTRDrag : (dropsite DropAt, window Source, any* ItemList, dragNdrop::dragShape, vpiDomains::pnt DropSitePnt,
        vpiDomains::keyModifier ModifierKeys).
clauses
    onTRDrag(_DropAt, _Source, _AnyItemList, _DragShape, pnt(X, Y), _ModifierKeys) :-
        Node = wsFE_SourceTree():treeControl_P:tryGetNodeAtPosition(pnt(X, Y)),
        wsFE_SourceTree():treeControl_P:expand(Node),
        string(folder_C) = Node:tryGetValue(nodeID_C),
        !,
        trDropSite:dropAllowed := false.
    onTRDrag(_DropAt, _Source, AnyItemList, _DragShape, pnt(X, Y), _ModifierKeys) :-
        if
            [Item] = AnyItemList,
            dragItem(SourceItemId) = tryToTerm(dragItem, Item),
            sourceItemGroup(SourceItemId, tuple(NodeType, _, SNodeStr)),
            NodeType <> folder_C,
            Node = wsFE_SourceTree():treeControl_P:tryGetNodeAtPosition(pnt(X, Y)),
            string(TNodeStr) = Node:tryGetValue(xmlObj_C),
            not(string::hasPrefix(SNodeStr, string::concat("[",TNodeStr), _))
        then
            trDropSite:tipText := slDropSite:tipText,
            trDropSite:dropAllowed := true
        elseif
            [Item] = AnyItemList,
            dragMulti(_) = tryToTerm(dragMulti, Item)
        then
            trDropSite:tipText := slDropSite:tipText,
            trDropSite:dropAllowed := true
        else
            trDropSite:dropAllowed := false
        end if.

predicates
    onTRDrop : (dropsite DropAt, window Source, any* ItemList, dragNdrop::dragShape, vpiDomains::pnt DropSitePnt,
        vpiDomains::keyModifier ModifierKeys).
clauses
    onTRDrop(_DropAt, _Source, AnyItemList, _DragShape, pnt(X,Y), _ModifierKeys) :-
        if
            [AnyItem] = AnyItemList,
            TNode = wsFE_SourceTree():treeControl_P:tryGetNodeAtPosition(pnt(X, Y))
        then
            dragMoveToTree := tuple(AnyItem, TNode),
            showPopupTree(pnt(X,Y), dragNdropTreeOperation_V)
        end if.

clauses
    new(FrontEnd,ListControl):-
        wsFE_Connector::new(FrontEnd),
        sourceList_P:=ListControl,
        sourceList_P:dockStyle := control::dockfill,
        sourceList_P:addMouseDblClickListener(onListViewControlMouseDblClick),
        sourceList_P:addSelectEndListener(onSelectEnd),
        sourceList_P:dragBeginResponder := dragBeginSourceList,

        slDropSite := dropsite::createDropsite(sourceList_P),
        slDropSite:dropAllowed := true,
        slDropSite:tipText := "TipText",
        slDropSite:dropResponder := onSLDrop,
        slDropSite:dragResponder := onSLDrag,
        dragNDrop::addDropsite(slDropSite),

        trDropSite := dropsite::createDropsite(wsFE_SourceTree():treeControl_P),
        trDropSite:dropAllowed := true,
        trDropSite:dropResponder := onTRDrop,
        trDropSite:dragResponder := onTRDrag,
        dragNDrop::addDropsite(trDropSite),

        sourceList_P:addKeyDownListener(onKeyDown),
        addMenuHandlers(),

        ImageList = imageList::new(imageWidth_C, imageHeight_C),
        sourceList_P:imageList := ImageList,
        vip_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(vip_C)),
        vip_bw_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(vip_bw_C)),
        txt_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(txt_C)),
        txt_light_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(txt_light_C)),
        pzl_bk_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(pzl_bk_C)),
        pzl_wt_P:=ImageList:addGDIplusBitmap(bitmap::createFromBinary(pzl_wt_C)),
        ColumnList = [
             column(ws_Events():getString(colSourceFile), 200, alignleft),
             column(ws_Events():getString(colPath), 300, alignleft),
             column(ws_Events():getString(colStatus), 80, alignleft),
             column(ws_Events():getString(colErrors), 60, alignright),
             column(ws_Events():getString(colDateTime), 150, alignright)
                             ],
        sourceList_P:insertColumnList(1, ColumnList),
        ws_Events():changeLanguageEvent:addListener(
            {:-
             column(_, W1, A1) = sourceList_P:getColumn(1),
             sourceList_P:setColumn(1, column(ws_Events():getString(colSourceFile), W1, A1)),
             column(_, W2, A2) = sourceList_P:getColumn(2),
             sourceList_P:setColumn(2, column(ws_Events():getString(colPath), W2, A2)),
             column(_, W3, A3) = sourceList_P:getColumn(3),
             sourceList_P:setColumn(3, column(ws_Events():getString(colStatus), W3, A3)),
             column(_, W4, A4) = sourceList_P:getColumn(4),
             sourceList_P:setColumn(4, column(ws_Events():getString(colErrors), W4, A4)),
             column(_, W5, A5) = sourceList_P:getColumn(5),
             sourceList_P:setColumn(5, column(ws_Events():getString(colDateTime), W5, A5))
            }),
        sourceList_P:setLVType(listViewControl::lvs_report),
        sourceList_P:setStyle([
            listViewControl::lvs_showselalways,
            listViewControl::lvs_autoarrange,
            listViewControl::lvs_noSortHeader
            ]),
       sourceList_P:customDraw := some(anotherColorForFailed).

/********  Popup Menu definition and invocation ******/
constants
    menu_ShowInTree  : menuTag = 10500.
    menu_Explore  : menuTag = 10501.
    menu_put_above : menuTag = 10502.
    menu_put_below : menuTag = 10503.
    menu_put_top : menuTag = 10504.
    menu_put_end : menuTag = 10505.
    menu_RestoreItem : menuTag = 10506.
    menu_LocalOptions : menuTag = 10507.

facts
    sourceListOperation_V:menuItem* := [].
    dragNdropOperation_V:menuItem* := [].
    dragNdropTreeOperation_V:menuItem* := [].

predicates
    addMenuHandlers : ().
clauses
    addMenuHandlers():-
        sourceListOperation_V :=
                [
                txt(menu_ShowInTree, ws_Events():getString(pmnShowInTree), noAccelerator, b_true, mis_none, []),
                txt(menu_Explore, ws_Events():getString(pmnExplore), noAccelerator, b_true, mis_none, [])
                ],
        dragNdropOperation_V :=
                [
                txt(menu_put_above, ws_Events():getString(pmnSrcMoveAbove), noAccelerator, b_true, mis_none, []),
                txt(menu_put_below, ws_Events():getString(pmnSrcMoveBelow), noAccelerator, b_true, mis_none, [])
                ],
        dragNdropTreeOperation_V :=
                [
                txt(menu_put_top, ws_Events():getString(pmnSrcMoveTop), noAccelerator, b_true, mis_none, []),
                txt(menu_put_end, ws_Events():getString(pmnSrcMoveLast), noAccelerator, b_true, mis_none, [])
                ],
        sourceList_P:addMenuItemListener(menu_ShowInTree, onMenuShowInTree),
        sourceList_P:addMenuItemListener(menu_Explore, onMenuExplore),
        sourceList_P:addMenuItemListener(menu_RestoreItem, onMenuRestoreItem),
        sourceList_P:addMenuItemListener(menu_put_above, onMenuMoveAbove),
        sourceList_P:addMenuItemListener(menu_put_below, onMenuMoveBelow),
        sourceList_P:addMenuItemListener(menu_LocalOptions, onMenuLocalOptions),
        wsFE_SourceTree():treeControl_P:addMenuItemListener(menu_put_top, onMenuMoveTop),
        wsFE_SourceTree():treeControl_P:addMenuItemListener(menu_put_end, onMenuMoveEnd),
        sourceList_P:setContextMenuResponder(onContextMenu).

predicates
    showPopup : (pnt Pnt,menuItem* MenuItemList).
clauses
    showPopup(PntTree,MenuItemList) :-
        PntForm = sourceList_P:mapPointTo(sourceList_P, PntTree),
        sourceList_P:menuPopup(dynmenu(MenuItemList), PntForm, align_left).

predicates
    showPopupTree : (pnt Pnt,menuItem* MenuItemList).
clauses
    showPopupTree(PntTree,MenuItemList) :-
        PntForm = wsFE_SourceTree():treeControl_P:mapPointTo(wsFE_SourceTree():treeControl_P, PntTree),
        wsFE_SourceTree():treeControl_P:menuPopup(dynmenu(MenuItemList), PntForm, align_left).

predicates
    onContextMenu : contextMenuResponder.
clauses
    onContextMenu(_, mouse(Pnt)) = contextMenuHandled :-
        Pnt = pnt(X,Y),
        try
            sourceList_P:getItemPosition(X, Y, ItemId, _),
            !,
            sourceList_P:selectAndFocused(ItemId),
            if
                listViewControl::item(_, _, _, _, [_File, ItemStatus, _OldErrWarn, _DateTime]) = sourceList_P:getItem(ItemId),
                ItemStatus = ws_Events():getString(excludedStatus_C)
            then
                MenuItemList = [txt(menu_RestoreItem, ws_Events():getString(pmnSrcRestore), noAccelerator, b_true, mis_none, []) | sourceListOperation_V]
            else
                MenuItemList = sourceListOperation_V
            end if,
            showPopup(Pnt, MenuItemList)
        catch _TraceId do
            fail
        end try.
    onContextMenu(_, keyboard) = contextMenuHandled :-
        [ItemIDSelected]=sourceList_P:getSel(),
        pnt(X,Y) = sourceList_P:getItemPosition(ItemIDSelected),
        !,
        if
            listViewControl::item(_, _, _, _, [_File, ItemStatus, _OldErrWarn, _DateTime]) = sourceList_P:getItem(ItemIDSelected),
            ItemStatus = ws_Events():getString(excludedStatus_C)
        then
            MenuItemList = [txt(menu_RestoreItem, ws_Events():getString(pmnSrcRestore), noAccelerator, b_true, mis_none, []) | sourceListOperation_V]
        else
            MenuItemList = sourceListOperation_V
        end if,
        showPopup(pnt(X+10,Y+10), MenuItemList).
    onContextMenu(_, _) = defaultContextMenuHandling.

predicates
    onMenuShowInTree : menuItemListener.
clauses
    onMenuShowInTree(_, _):-
        wsFE_Tasks():showSourceInTree().

predicates
    onMenuRestoreItem : menuItemListener.
clauses
    onMenuRestoreItem(_, _):-
        wsFE_Tasks():restoreExcludedSource().

predicates
    onMenuExplore : menuItemListener.
clauses
    onMenuExplore(_, _):-
        if ShorFileName = tryGetSelectSourceFileName() then
            wsFE_Tasks():getFullFileName(ShorFileName,
                {
                (FileName) :-
                    _ = shell_native::shellExecute(nullHandle, "", "explorer", string::format("/select, %s", file::getPhysicalName(FileName)), "",windowsAPI::sw_show)
                })
        end if.

predicates
    onMenuLocalOptions : menuItemListener.
clauses
    onMenuLocalOptions(_, _):-
        wsFE_Form():showLocalOptionsPanel().

predicates
    onMenuMoveAbove : menuItemListener.
clauses
    onMenuMoveAbove(_, _):-
        tuple(ItemIDSelected, ItemIDBelow) = dragMoveItems,
        wsFE_Tasks():moveSourceUp(toString(ItemIDSelected),toString(ItemIDBelow)),
        predicateDelayedQueue_P:enqueue(tuple(setSelection,uncheckedConvert(unsigned,ItemIDSelected))),
%        sourceList_P:select([ItemIDSelected],true),
%        sourceList_P:ensureVisible(ItemIDSelected),
        dragMoveItems := erroneous.

predicates
    onMenuMoveBelow : menuItemListener.
clauses
    onMenuMoveBelow(_, _):-
        tuple(ItemIDSelected, ItemIDBelow) = dragMoveItems,
        wsFE_Tasks():moveSourceDown(toString(ItemIDSelected),toString(ItemIDBelow)),
        predicateDelayedQueue_P:enqueue(tuple(setSelection,uncheckedConvert(unsigned,ItemIDSelected))),
%        sourceList_P:select([ItemIDSelected],true),
%        sourceList_P:ensureVisible(ItemIDSelected),
        dragMoveItems := erroneous.

predicates
    onMenuMoveTop : menuItemListener.
clauses
    onMenuMoveTop(_, _):-
        tuple(AnyItem, Node) = dragMoveToTree,
        tuple(NodePath,_NodePathObj) = wsFE_SourceTree::getNodePath(Node,tuple([],[])),
        if
            dragItem(ItemIDSelected) = tryToTerm(dragItem, AnyItem)
        then
            wsFE_Tasks():moveSourceToTree(true, toString(ItemIDSelected), NodePath)
        elseif
            dragMulti(SelList) = tryToTerm(dragMulti, AnyItem)
        then
            foreach ItemID in list::reverse(SelList),
                sourceItemGroup(ItemId, tuple(NodeType, _, _))
            do
                if NodeType <> folder_C then
                    wsFE_Tasks():moveSourceToTree(true, toString(ItemID), NodePath)
%                else
%                    wsFE_Tasks():cloneSourceToTree(true, toString(ItemID), NodePath)
                end if
            end foreach
        end if,
        dragMoveToTree := erroneous.

predicates
    onMenuMoveEnd : menuItemListener.
clauses
    onMenuMoveEnd(_, _):-
        tuple(AnyItem, Node) = dragMoveToTree,
        tuple(NodePath,_NodePathObj) = wsFE_SourceTree::getNodePath(Node,tuple([],[])),
        if
            dragItem(ItemIDSelected) = tryToTerm(dragItem, AnyItem)
        then
            wsFE_Tasks():moveSourceToTree(false, toString(ItemIDSelected), NodePath)
        elseif
            dragMulti(SelList) = tryToTerm(dragMulti, AnyItem)
        then
            foreach ItemID in SelList,
                sourceItemGroup(ItemId, tuple(NodeType, _, _))
            do
                if NodeType <> folder_C then
                    wsFE_Tasks():moveSourceToTree(false, toString(ItemID), NodePath)
%                else
%                    wsFE_Tasks():cloneSourceToTree(false, toString(ItemID), NodePath)
                end if
            end foreach
        end if,
        dragMoveToTree := erroneous.

predicates
    anotherColorForFailed : listViewControl::customDraw.
clauses
    anotherColorForFailed(_ListViewControl,
        nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_prepaint, _, _, _ItemHandle, _, _),_TextForeground, _TextBackground, _SubItem, _, _, _, _, _, _, _, _)
        ) = window::nativeResult(gui_native::cdrf_notifyitemdraw) :-
        !.
    anotherColorForFailed(ListViewControl, NmLvCustomDraw) = _ :-
        nmLvCustomDraw(gui_native::nmCustomDraw(_, _, _, _, ItemSpec, _, _), _TextForeground, _TextBackground, _SubItem, _, _, _, _, _, _, _, _) = NmLvCustomDraw,
        ItemId = ListViewControl:getItemId(convert(unsigned, uncheckedConvert(unsignedNative, ItemSpec))),
        if
            sourceItemGroup(ItemID, tuple(folder_C, _, _)),
            listViewControl::item(_, _, _, _, [_File, ItemStatus, _OldErrWarn, _DateTime]) = sourceList_P:getItem(ItemId),
            ItemStatus = ws_Events():getString(excludedStatus_C)
        then
            rowColor(excludedSource_C, FGColor, BGColor)
        elseif
            sourceItemGroup(ItemID, tuple(folder_C, _, _))
        then
            rowColor(folderSource_C, FGColor, BGColor)
        else
            rowColor(groupSource_C, FGColor, BGColor)
        end if,
        memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), FGColor),
        memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)+sizeOfDomain(color)), BGColor),
        fail.
    anotherColorForFailed(_ListViewControl,
        NmLvCustomDraw) = window::nativeResult(gui_native::cdrf_notifysubitemdraw) :-
        NmLvCustomDraw = nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_itemprepaint, _, _, _ItemHandle, _, _),_TextForeground, _TextBackground, _SubItem, _, _, _, _, _, _, _, _),
        !.
    anotherColorForFailed(ListViewControl, NmLvCustomDraw) = window::nativeResult(gui_native::cdrf_dodefault) :-
        try
            listViewControl::nmLvCustomDraw(gui_native::nmCustomDraw(_, gui_native::cdds_itemprepaint+gui_native::cdds_subitem, _, _, ItemSpec, _, ItemID_lpar), _TextForeground, _TextBackground, IndexCol, ItemType, _, _, _, _, _, _, _) = NmLvCustomDraw,
            if ItemType=0, Item_ID=convert(itemID,ItemID_lpar),not(Item_ID=uncheckedConvert(handle,0)) then
                ItemId = ListViewControl:getItemId(convert(unsigned, uncheckedConvert(unsignedNative, ItemSpec))),
                listViewControl::item(_, _, _, _, [_File, StatusText, _OldErrWarn, _DateTime]) = sourceList_P:getItem(ItemId),
                if IndexCol = 3 and ws_Events():getString(failedStatus_C) = StatusText then
                    memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), vpiDomains::color_Red)
                elseif ws_Events():getString(excludedStatus_C) = StatusText, rowColor(excludedSource_C, FGColor, _BGColor) then
                    memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), FGColor)
                elseif sourceItemGroup(ItemID, tuple(folder_C, _, _)), rowColor(folderSource_C, FGColor, _BGColor) then
                    memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), FGColor)
                elseif rowColor(groupSource_C, FGColor, _BGColor) then
                    memory::setInteger(memory::pointerAdd(NmLvCustomDraw, sizeOfDomain(gui_native::nmCustomDraw)), FGColor)
                end if
            end if
        catch _TraceId do
            succeed
        end try,
        !.
    anotherColorForFailed(_Source, _NmLvCustomDraw) = window::nativeResult(gui_native::cdrf_dodefault).

clauses
    addSource(_Parameters).

clauses
    sourceUp(_Command):-
        sourceUp().

predicates
    sourceUp:().
clauses
    sourceUp():-
        if
            [ItemIDSelected]=sourceList_P:getSel(),
            Index=sourceList_P:tryGetItemIndex(ItemIDSelected),
            Index>0,
            ItemIDAbove=sourceList_P:getItemId(Index-1),
            sourceItemGroup(ItemIDSelected, tuple(_, _, SPath)),
            sourceItemGroup(ItemIDAbove, tuple(_, TargetName, TPath)),
            if
                SPath <> TPath,
                Message = string::format("% '%s'?", ws_Events():getString(qstMoveSource), TargetName),
                Answer = vpiCommonDialogs::messageBox(ws_Events():getString(questionTitle_C), Message, mesbox_iconQuestion,
                            mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
                idc_cancel = Answer
            then
                fail
            else
                succeed
            end if
        then
            wsFE_Tasks():moveSourceUp(toString(ItemIDSelected),toString(ItemIDAbove)),
            predicateDelayedQueue_P:enqueue(tuple(setSelection,uncheckedConvert(unsigned,ItemIDSelected)))
%            sourceList_P:select([ItemIDSelected],true),
%            sourceList_P:ensureVisible(ItemIDSelected)
        end if.
%    sourceUp().

predicates
    setSelection:(unsigned ItemID).
clauses
    setSelection(ItemIDSelected):-
        sourceList_P:select([uncheckedConvert(itemID,ItemIDSelected)],true),
        sourceList_P:ensureVisible(uncheckedConvert(itemID,ItemIDSelected)).

clauses
    sourceDown(_Command):-
        sourceDown().

predicates
    sourceDown:().
clauses
    sourceDown():-
        if
            [ItemIDSelected]=sourceList_P:getSel(),
            NoOfItems=sourceList_P:getItemCount(),
            Index=sourceList_P:tryGetItemIndex(ItemIDSelected),
            Index+1<NoOfItems,
            ItemIDBelow=sourceList_P:getItemId(Index+1),
            sourceItemGroup(ItemIDSelected, tuple(_, _, SPath)),
            sourceItemGroup(ItemIDBelow, tuple(_, TargetName, TPath)),
            if
                SPath <> TPath,
                Message = string::format("% '%s'?", ws_Events():getString(qstMoveSource), TargetName),
                Answer = vpiCommonDialogs::messageBox(ws_Events():getString(questionTitle_C), Message, mesbox_iconQuestion,
                            mesbox_buttonsYesNo,mesbox_defaultFirst,mesbox_suspendApplication),
                idc_cancel = Answer
            then
                fail
            else
                succeed
            end if
        then
            wsFE_Tasks():moveSourceDown(toString(ItemIDSelected),toString(ItemIDBelow)),
            sourceList_P:select([ItemIDSelected],true),
            sourceList_P:ensureVisible(ItemIDSelected)
        end if.

clauses
    run(false,TrueIfReRun):-
        ItemsIDToRun=sourceList_P:getSel(),
        !,
        wsFE_Tasks():run(TrueIfReRun,[toString(ItemID)||ItemID=list::getMember_nd(ItemsIDToRun)]).
    run(true,TrueIfReRun):-
        ItemsIDToRun=sourceList_P:getAll(),
        wsFE_Tasks():run(TrueIfReRun,[toString(ItemID)||ItemID=list::getMember_nd(ItemsIDToRun)]).

clauses
    execRun():-
        if ItemsIDToExec=sourceList_P:getSel(), []<>ItemsIDToExec then
            wsFE_Tasks():execRun([toString(ItemID)||ItemID=list::getMember_nd(ItemsIDToExec)])
        end if.

clauses
    setAllInQueue(NodeIdList) = QueueNodeIdList :-
        InQueue = ws_Events():getString(inQueueStatus_C),
        Done = ws_Events():getString(doneStatus_C),
        Excluded = ws_Events():getString(excludedStatus_C),
        retractAll(localItemStatus(_, _)),
        QueueNodeIdList=[NodeIdStr||
            NodeIdStr in NodeIdList,
            SourceIDuns=toTerm(NodeIdStr),
            listViewControl::item(SourceIDuns, _, _, _, [_File, OldStatus, _OldErrWarn, _DateTime]) = sourceList_P:getItem(SourceIDuns),
            not(OldStatus in [Done,Excluded]),
            showPerformStatus(NodeIdStr,InQueue,"0","0",""),
            assertz(localItemStatus(SourceIDuns, InQueue))
            ].

clauses
    getAllInQueue() = QueueNodeIdList :-
        InQueue = ws_Events():getString(inQueueStatus_C),
        Stopped = ws_Events():getString(stoppedStatus_C),
        QueueNodeIdList=[toString(SourceIDuns)||
            SourceIDuns in sourceList_P:getAll(),
                listViewControl::item(SourceIDuns, _, _, _, [_File, Status, _OldErrWarn, _DateTime]) = sourceList_P:getItem(SourceIDuns),
            Status in [InQueue, Stopped]].

clauses
    tryGetSelectSourceFileName() = string::concat(Path,FileName) :-
        SourceIDs=sourceList_P:getAll(),
        not (SourceIDs=[]),
        [SourceID] = sourceList_P:getSel(),
        listViewControl::item(SourceID, FileName, _, _, [Path, _OldStatus, _OldErrWarn, _OldDateTime]) = sourceList_P:getItem(SourceID).

clauses
    showPerformStatus(SourceID,ws_Events():getString(performingStatus_C),_Errors,_Warnings,_DateTime):-
        Selected=sourceList_P:getSel(),
        sourceList_P:select(Selected,false),
        SourceIDuns=toTerm(SourceID),
        sourceList_P:selectAndFocused(SourceIDuns),
        fail.
    showPerformStatus(SourceID,Status,Errors,Warnings,DateTime):-
        SourceIDuns=toTerm(SourceID),
        if
            localItemStatus(SourceIDuns, InQueue),
            Status <> InQueue
        then
            retractAll(localItemStatus(SourceIDuns, _))
        end if,
        !,
        if listViewControl::item(SourceIDuns, Name, IconID, Flags, [Path, _OldStatus, _OldErrWarn, _OldDateTime]) = sourceList_P:tryGetItem(SourceIDuns) then
            if Errors="0", Warnings="0" then
                ErrWarn=""
            else
                ErrWarn=string::format("%s/%s",Errors,Warnings)
            end if,
            sourceList_P:updateItem(listViewControl::item(SourceIDuns, Name, IconID, Flags, [Path, Status, ErrWarn, DateTime]))
        end if.
%    showPerformStatus(_SourceID,_Status,_Errors,_Warnings,_DateTime):-
%        exception::raise_User("Unexpected Alternative").

predicates
    onListViewControlMouseDblClick : listViewControl::mouseDblClickListener.
clauses
    onListViewControlMouseDblClick(_Source, _Point):-
        wsFE_Tasks():openSource().

predicates
    onSelectEnd : listViewControl::selectEndListener.
clauses
    onSelectEnd(_Source, _ItemId, _Select).

predicates
    onKeyDown : keyDownListener.
clauses
    onKeyDown(_Source, 38, 2) :-
        sourceUp(),
        !.
    onKeyDown(_Source, 40, 2) :-
        sourceDown(),
        !.
    onKeyDown(_Source, 46, 0) :-
        wsFE_Tasks():removeSource(),
        !.
    onKeyDown(_Source, 65, 2) :-
        sourceList_P:selectAll(true),
        !.
   onKeyDown(_Source, _Key, _ShiftControlAlt).

end implement wSFE_SourceList
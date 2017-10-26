%

implement wsFE_SourceTree
    inherits wsFE_Connector
    open core,vpiDomains,ws_eventManager, treeControl{treeNode_std}, window, pfc\log\

facts
    treeControl_P:treeControl{treeNode_std}:=erroneous.

constants
    imageWidth_C=16.
    imageHeight_C=16.

constants
    flowChartImg_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\SmallCircle.png").

    folderImg_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\folder-16-000000.png").
    groupImg_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\PNGwrk\squares-16-000000.png").

clauses
    new(FrontEndObj,TreeControl):-
        treeControl_P:=TreeControl,
        wsFE_Connector::new(FrontEndObj),
        Model = treeModel_std::new(),
        treeControl_P:dockStyle := control::dockFill,
        treeControl_P:model := Model,
        treeControl_P:setNodeCompareResponder(noSort)=_OldResponder,
        treeControl_P:imageList := imageList::new(imageWidth_C, imageHeight_C),
        treeControl_P:stateImageList := imageList::new(imageWidth_C, imageHeight_C),

        _Bmp1 = treeControl_P:imageList:addGDIplusBitmap(bitmap::createFromBinary(flowChartImg_C)),
        _Bmp2 = treeControl_P:imageList:addGDIplusBitmap(bitmap::createFromBinary(groupImg_C)),
        _Bmp3 = treeControl_P:imageList:addGDIplusBitmap(bitmap::createFromBinary(folderImg_C)),

        treeControl_P:setWindowTheme("explorer"),
        treeControl_P:nodeRenderer := Model:nodeRenderer,

        treeControl_P:setContextMenuResponder(onContextMenu),
        _ = treeControl_P:setNodeEditBeginResponder(onNodeEditBegin),
        _ = treeControl_P:setNodeEditEndResponder(onNodeEditEnd),
        addMenuListeners(),

        treeControl_P:setKeyDownResponder(onKeyDown),
        treeControl_P:dragBeginResponder := dragBeginResponder,
        treeControl_P:dropResponder := dropResponder,
        treeControl_P:endDropEvent:addListener(endDropListener),
        treeControl_P:addExpandEndListener(expandEndListener),
        _OldResponder2=treeControl_P:setSelectBeginResponder(nodeSelectedResponder).

predicates
    nodeSelectedResponder:selectBeginResponder.
clauses
    nodeSelectedResponder(_TreeControl, _Unselected,node(Node))=acceptSelect:-
        wsFE_Form():wsFE_Command_P:disabledRibbonPauseBlock(),
        tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
        !,
        wsFE_Tasks():showNodeContent(NodePath).
    nodeSelectedResponder(_TreeControl, _Unselected,_NodeSelected)=cancelSelect.

clauses
    resetModel():-
        Model=convert(treeModel_std,treeControl_P:model),
        [Tree]=Model:getTreeList(),
        !,
        Model:deleteTree(Tree).
    resetModel().

clauses
    setSourceTree(SourceTree, NodePathList):-
        TreeControlTree=xmlTermTree2TreeControl(SourceTree),
        TreeModel=convert(treeModel_std,treeControl_P:model),
        TreeModel:addTree(TreeControlTree),
        treeControl_P:expand(TreeControlTree),
        if NodePathList = [_|SelectNodePath] then
            selectNodePath(SelectNodePath, TreeControlTree)
        else
            treeControl_P:select(TreeControlTree)
        end if.

clauses
    setSourceInTask(NodePathList):-
        TreeModel=convert(treeModel_std,treeControl_P:model),
        TreeControlTree=TreeModel:getRoot_nd(),
        !,
        if NodePathList = [_|SelectNodePath] then
            selectNodePath(SelectNodePath, TreeControlTree)
        else
            treeControl_P:select(TreeControlTree)
        end if.
    setSourceInTask(_NodePathList).

predicates
    selectNodePath : (namedValue* SelectNodePath,treeNode_std ParentNode).
clauses
    selectNodePath([namedValue(NodeType, string(Title))| RestList], ParentNode):-
        list::isMember(NodeType, [groupNode_C,folder_C]),
        ChildNode = ParentNode:getChild_nd(),
        Title = ChildNode:label,
        !,
        selectNodePath(RestList, ChildNode).
    selectNodePath(_, ParentNode):-
        treeControl_P:select(ParentNode).

predicates
    onNodeEditBegin : nodeEditBeginResponder.
clauses
    onNodeEditBegin(_Node, _) = allowEdit.

predicates
    onNodeEditEnd : nodeEditEndResponder.
clauses
    onNodeEditEnd(_, Node, editResult(NewTitle)) = acceptLabel :-
        if Parent=Node:tryGetParent() then
            ContextNode=Parent
        else
            ContextNode=Node
        end if,
        if ChildNode=ContextNode:getChild_nd(),ChildNode:label=NewTitle then
            FormatStr = ws_Events():getString(msgUniqNodeName),
            wsFE_Tasks():explain(string::format(FormatStr, NewTitle)),
            fail
        else
            tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
            !,
            wsFE_Tasks():setNewTitle(NodePath,NewTitle),
            Node:label:=NewTitle,
            Node:setValue(title_C,string(NewTitle))
        end if,
        !.
    onNodeEditEnd(_, _, _) = rejectLabel.

facts
    contextCallNode_V:treeNode_std:=erroneous.

/******** Add Group *************/
predicates
    onMenuAddGroup : menuItemListener.
clauses
    onMenuAddGroup(_, _) :-
        addGroup().
clauses
    addGroup(_):-
        addGroup().

predicates
    addGroup:().
clauses
    addGroup():-
        addGroupHandler_V:={(NodeObj,NodeParamList):-addNewGroup(NodeObj,NodeParamList)},
        Node = treeControl_P:tryGetSelected(),
        if Parent=Node:tryGetParent() then
            ContextNode=Parent
        else
            ContextNode=Node
        end if,
        !,
        contextCallNode_V:=ContextNode,
        tuple(NodePath,_NodePathObj)=getNodePath(ContextNode,tuple([],[])),
        wsFE_Tasks():createNewGroup(NodePath).
    addGroup().

facts
    addGroupHandler_V:predicate{treeNode_std Object,namedValue* NodeParamList}:=erroneous.
predicates
    addNewGroup:(treeNode_std Object,namedValue* NodeParamList).
clauses
    addNewGroup(NodeObject,NodeParamList):-
        Title=namedValue::getNamed_string(NodeParamList,title_C),
        NewNode = treeNode_std::new(Title),
        NodeImage=getNodeImage(groupNode_C),
        !,
        NewNode:bitmapIdx:=NodeImage,
        NewNode:selectedBitmapIdx:=NodeImage,
        _DummyList=[""||NodeParam=list::getMember_nd(NodeParamList),NewNode:addNamedValue(NodeParam)],
        NodeObject:addChild(NewNode),
        treeControl_P:expand(NewNode),
        addGroupHandler_V:=erroneous,
        treeControl_P:select(NewNode),
        treeControl_P:nodeEdit(NewNode).

clauses
    addNewGroup(NodeParamList):-
        addGroupHandler_V(contextCallNode_V,NodeParamList).

/******** Add Sub Group *************/
predicates
    onMenuAddSubGroup : menuItemListener.
clauses
    onMenuAddSubGroup(_, _) :-
        addSubGroup().
clauses
    addSubGroup(_):-
        addSubGroup().

predicates
    addSubGroup:().
clauses
    addSubGroup():-
        Node = treeControl_P:tryGetSelected(),
        !,
        if NodeType=Node:tryGetValue(nodeID_C),(NodeType=string(groupNode_C) or NodeType=string(workSpace_C)) then
            addGroupHandler_V:={(NodeObj,NodeParamList):-addNewGroup(NodeObj,NodeParamList)},
            contextCallNode_V:=Node,
            tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
            wsFE_Tasks():createNewGroup(NodePath)
        else
            wsFE_Tasks():explain("Sorry, subgroup was not created.\nSubFolder can not be created under folder")
        end if.
    addSubGroup().


/******** Add Folder *************/
predicates
    onMenuAddFolder : menuItemListener.
clauses
    onMenuAddFolder(_, _) :-
        addFolder().
clauses
    addFolder(_):-
        addFolder().

predicates
    addFolder:().
clauses
    addFolder():-
        Node = treeControl_P:tryGetSelected(),
        if Parent=Node:tryGetParent() then
            ContextNode=Parent
        else
            ContextNode=Node
        end if,
        tuple(NodePath,_NodePathObj)=getNodePath(ContextNode,tuple([],[])),
        NewFolderName=wsFE_Tasks():tryDefineNewFolder(treeControl_P),
        contextCallNode_V:=ContextNode,
        !,
        wsFE_Tasks():createNewFolder([namedValue(path_C,string(NewFolderName))|NodePath]).
    addFolder().

clauses
    addNewFolder(NodeParamList):-
        Title=namedValue::getNamed_string(NodeParamList,title_C),
        NewNode = treeNode_std::new(Title),
        NodeImage=getNodeImage(folder_C),
        !,
        NewNode:bitmapIdx:=NodeImage,
        NewNode:selectedBitmapIdx:=NodeImage,
        _DummyList=[""||NodeParam=list::getMember_nd(NodeParamList),NewNode:addNamedValue(NodeParam)],
        treeControl_P:expand(NewNode),
        contextCallNode_V:addChild(NewNode),
        contextCallNode_V:=erroneous,
        treeControl_P:select(NewNode).

/******** Add  Child Folder *************/
predicates
    onMenuAddChildFolder : menuItemListener.
clauses
    onMenuAddchildFolder(_, _) :-
        addSubFolder().
clauses
    addSubFolder(_):-
        addSubFolder().

predicates
    addSubFolder:().
clauses
    addSubFolder():-
        Node = treeControl_P:tryGetSelected(),
        if NodeType=Node:tryGetValue(nodeID_C),(NodeType=string(groupNode_C) or NodeType=string(workSpace_C)) then
            tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
            FolderName=wsFE_Tasks():tryDefineNewFolder(treeControl_P),
            contextCallNode_V:=Node,
            wsFE_Tasks():createNewFolder([namedValue(path_C,string(FolderName))|NodePath])
        else
            wsFE_Tasks():explain("Sorry, folder was not created.\nfolder can not be created under folder")
        end if,
        !.
    addSubFolder().

predicates
    onMenuFolderExplore : menuItemListener.
clauses
    onMenuFolderExplore(_, _):-
        if Node = treeControl_P:tryGetSelected(), string(ShortDirName)==Node:tryGetValue(path_C) then
            wsFE_Tasks():getFullFileName(ShortDirName,
                {
                (DirPath) :-
                    _ = shell_native::shellExecute(nullHandle, "", "explorer", DirPath, "",windowsAPI::sw_show)
                })
        end if.

/****** Delete Node ********/
predicates
    onMenuDeleteNode : menuItemListener.
clauses
    onMenuDeleteNode(_, _) :-
        deleteTreeEntity().

clauses
    deleteTreeEntity(_):-
        deleteTreeEntity().

predicates
    deleteTreeEntity:().
clauses
    deleteTreeEntity():-
        Node = treeControl_P:tryGetSelected(),
        if Parent=Node:tryGetParent() then
            ContextNode=Parent
        else
            wsFE_Tasks():explain(ws_Events():getString(msgRootNodeDeleted)),
            fail
        end if,
        tuple(NodePath,_NodePathObj)=getNodePath(ContextNode,tuple([],[])),
        contextCallNode_V:=Node,
        wsFE_Tasks():tryRemoveNode([namedValue(title_C,Node:tryGetValue(title_C))|NodePath]),
        !.
    deleteTreeEntity():-
        contextCallNode_V:=erroneous.

    removeTreeNode(_Parameters):-
        Parent=contextCallNode_V:tryGetParent(),
        !,
        Parent:deleteChild(contextCallNode_V),
        contextCallNode_V:=erroneous.
    removeTreeNode(_Parameters).

predicates
    onMenuRenameNode : menuItemListener.
clauses
    onMenuRenameNode(_, _) :-
        Node = treeControl_P:tryGetSelected(),
        !,
        treeControl_P:nodeEdit(Node).
    onMenuRenameNode(_, _).

predicates
    onKeyDown : keyDownResponder.
clauses
    onKeyDown(_Source, 318, 0) = defaultKeyDownHandling():-
        deleteTreeEntity(),!.
    onKeyDown(_Source, 301, 2) = defaultKeyDownHandling():-
        Node = treeControl_P:tryGetSelected(),
        tryMove(spbTree::right,Node),
        tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
        !,
        wsFE_Tasks():showNodeContent(NodePath).
    onKeyDown(_Source, 302, 2) = defaultKeyDownHandling():-
        Node = treeControl_P:tryGetSelected(),
        tryMove(spbTree::left,Node),
        tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
        !,
        wsFE_Tasks():showNodeContent(NodePath).
   onKeyDown(_Source, _Key, _ShiftControlAlt) = defaultKeyDownHandling().

clauses
    moveNodeUp(_):-
        Node = treeControl_P:tryGetSelected(),
        tryMove(spbTree::right,Node),
        tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
        !,
        wsFE_Tasks():showNodeContent(NodePath).
    moveNodeUp(_).

clauses
    moveNodeDown(_):-
        Node = treeControl_P:tryGetSelected(),
        tryMove(spbTree::left,Node),
        tuple(NodePath,_NodePathObj)=getNodePath(Node,tuple([],[])),
        !,
        wsFE_Tasks():showNodeContent(NodePath).
    moveNodeDown(_).

predicates
    tryMove:(spbTree::place,treeNode_std Node) determ.
clauses
    tryMove(Direction,Node):-
        Parent=Node:tryGetParent(),
        ChildList=[Child||Child=Parent:getChild_nd()],
        ListLen=list::length(ChildList),
        Position=list::tryGetIndex(Node,ChildList),
        tuple(ToContainer,ToPosition)=tryDefineLocation(Direction,Parent,ChildList,ListLen,Position),
        tuple(SourceNodePath,_DragNodePathObj)=getNodePath(Node,tuple([],[])),
        tuple(TargetNodePath,_DropNodePathObj)=getNodePath(ToContainer,tuple([],[])),
        moveNode(ToPosition,Node,ToContainer,treeControl_P),
        if ToPosition=spbTree::left then
            wsFE_Tasks():moveAbove(SourceNodePath,TargetNodePath)
        elseif ToPosition=spbTree::right then
            wsFE_Tasks():moveBelow(SourceNodePath,TargetNodePath)
        elseif ToPosition=spbTree::leftmost then
            wsFE_Tasks():moveOnBottom(SourceNodePath,TargetNodePath)
        elseif ToPosition=spbTree::rightmost then
            wsFE_Tasks():moveOnTop(SourceNodePath,TargetNodePath)
        end if.

predicates
    tryDefineLocation:(spbTree::place Direction,treeNode_std Parent,treeNode_std* ChildList,integer ListLen,integer Position)->tuple{treeNode_std ToContainer,spbTree::place ToPosition} determ.
clauses
    tryDefineLocation(spbTree::right,Parent,_ChildList,_ListLen,Position)=tuple(ToContainer,ToPosition):- % Move Up
        Position=0,!,
        not(string(workSpace_C)=Parent:tryGetValue(nodeID_C)),
        ToContainer=Parent,ToPosition=spbTree::left.
    tryDefineLocation(spbTree::left,Parent,_ChildList,ListLen,Position)=tuple(ToContainer,ToPosition):- % Move Down
        Position=ListLen-1,!,
        not(string(workSpace_C)=Parent:tryGetValue(nodeID_C)),
        ToContainer=Parent,ToPosition=spbTree::right.
    tryDefineLocation(spbTree::right,_Parent,ChildList,ListLen,Position)=tuple(ToContainer,ToPosition):- % Move Up and Node remains at the subtree
        ListLen>1,Position>=1,!,PosCandidate=Position-1,NodeCandidate=list::nTh(PosCandidate,ChildList),
        if string(folder_C)=NodeCandidate:tryGetValue(nodeID_C) then
            ToContainer=NodeCandidate,ToPosition=spbTree::left
        else
            ToContainer=NodeCandidate,ToPosition=spbTree::rightmost
        end if.
    tryDefineLocation(spbTree::left,_Parent,ChildList,ListLen,Position)=tuple(ToContainer,ToPosition):- % Move Down and Node remains at the subtree
        ListLen>1,Position<ListLen-1,!,PosCandidate=Position+1,NodeCandidate=list::nTh(PosCandidate,ChildList),
        if string(folder_C)=NodeCandidate:tryGetValue(nodeID_C) then
            ToContainer=NodeCandidate,ToPosition=spbTree::right
        else
            ToContainer=NodeCandidate,ToPosition=spbTree::leftmost
        end if.

/******* Drop Menu Operations*********/
predicates
    onDropInside : menuItemListener.
clauses
    onDropInside(_, _) :-
        dragNodes_V=dragParams(DragNode,DragNodePath,DropNode,DropNodePath),
        DragNode:setParent(DropNode),
        wsFE_Tasks():moveOnTop(DragNodePath,DropNodePath),
        dragNodes_V:=erroneous.

predicates
    onDropAbove : menuItemListener.
clauses
    onDropAbove(_, _) :-
        dragNodes_V=dragParams(DragNode,DragNodePath,DropNode,DropNodePath),
        moveNode(spbTree::left,DragNode,DropNode,treeControl_P),
        wsFE_Tasks():moveAbove(DragNodePath,DropNodePath),
        dragNodes_V:=erroneous.

predicates
    onDropBelow : menuItemListener.
clauses
    onDropBelow(_, _) :-
        dragNodes_V=dragParams(DragNode,DragNodePath,DropNode,DropNodePath),
        moveNode(spbTree::right,DragNode,DropNode,treeControl_P),
        wsFE_Tasks():moveBelow(DragNodePath,DropNodePath),
        dragNodes_V:=erroneous.

predicates
    onDropOnTop : menuItemListener.
clauses
    onDropOnTop(_, _) :-
        dragNodes_V=dragParams(DragNode,DragNodePath,DropNode,DropNodePath),
        moveNode(spbTree::leftmost,DragNode,DropNode,treeControl_P),
        wsFE_Tasks():moveOnTop(DragNodePath,DropNodePath),
        dragNodes_V:=erroneous.

predicates
    onDropOnBottom : menuItemListener.
clauses
    onDropOnBottom(_, _) :-
        dragNodes_V=dragParams(DragNode,DragNodePath,DropNode,DropNodePath),
        moveNode(spbTree::rightmost,DragNode,DropNode,treeControl_P),
        wsFE_Tasks():moveOnBottom(DragNodePath,DropNodePath),
        dragNodes_V:=erroneous.

/********  Popup Menu definition and invocation ******/
predicates
    onContextMenu : contextMenuResponder.
clauses
    onContextMenu(_, mouse(Pnt)) = contextMenuHandled :-
        Node = treeControl_P:tryGetNodeAtPosition(Pnt),
        !,
        treeControl_P:select(Node),
        showPopup(Pnt, Node).
    onContextMenu(_, keyboard) = contextMenuHandled :-
        Node = treeControl_P:tryGetSelected(),
        Rct = treeControl_P:tryGetNodeRect(Node, onlyText),
        Rct = rct(X, Y1, _, Y2),
        Pnt = pnt(X, (Y1 + Y2) div 2),
        !,
        showPopup(Pnt, Node).
    onContextMenu(_, _) = defaultContextMenuHandling.

predicates
    addMenuListeners:().
clauses
    addMenuListeners():- % Node Operations Listeners
        treeControl_P:addMenuItemListener(menu_add_group, onMenuAddGroup),
        treeControl_P:addMenuItemListener(menu_add_subgroup, onMenuAddSubGroup),
        treeControl_P:addMenuItemListener(menu_add_folder, onMenuAddFolder),
        treeControl_P:addMenuItemListener(menu_add_childfolder, onMenuAddChildFolder),
        treeControl_P:addMenuItemListener(menu_del_node, onMenuDeleteNode),
        treeControl_P:addMenuItemListener(menu_rename_node, onMenuRenameNode),
        treeControl_P:addMenuItemListener(menu_Explore, onMenuFolderExplore),
        fail.
    addMenuListeners():- % Node DragAndDrop Listeners
        treeControl_P:addMenuItemListener(menu_include, onDropInside),
        treeControl_P:addMenuItemListener(menu_put_above, onDropAbove),
        treeControl_P:addMenuItemListener(menu_put_below, onDropBelow),
        treeControl_P:addMenuItemListener(menu_put_ontop, onDropOnTop),
        treeControl_P:addMenuItemListener(menu_put_onbottom, onDropOnBottom).

constants
    menu_add_group : menuTag = 10000.
    menu_add_folder : menuTag = 10001.
    menu_add_childfolder : menuTag = 10002.
    menu_add_subgroup : menuTag = 10003.
    menu_del_node : menuTag = 10004.
    menu_rename_node : menuTag = 10005.
    menu_Explore : menuTag = 10006.

constants % DropMenuOperation_C
    menu_include : menuTag = 10100.
    menu_put_above : menuTag = 10101.
    menu_put_below : menuTag = 10102.
    menu_put_ontop : menuTag = 10103.
    menu_put_onbottom : menuTag = 10104.

predicates
    dropMenuItem_nd:(dropSolution,menuItem [out]).
clauses
    dropMenuItem_nd(include,txt(menu_include, ws_Events():getString(pmnInsertToGroup), noAccelerator, b_true, mis_none, [])).
    dropMenuItem_nd(putAbove,txt(menu_put_above, ws_Events():getString(pmnAddAboveNode), noAccelerator, b_true, mis_none, [])).
    dropMenuItem_nd(putBelow,txt(menu_put_below, ws_Events():getString(pmnAddBelowNode), noAccelerator, b_true, mis_none, [])).
    dropMenuItem_nd(putAtBeginning,txt(menu_put_ontop, ws_Events():getString(pmnPutOnTop), noAccelerator, b_true, mis_none, [])).
    dropMenuItem_nd(putAtEnd,txt(menu_put_onbottom, ws_Events():getString(pmnPutOnLast), noAccelerator, b_true, mis_none, [])).

predicates
    showPopup : (pnt Pnt,treeNode_std Node).
clauses
    showPopup(PntTree, Node) :-
        if string(folder_C) = Node:tryGetValue(nodeID_C) then
            MenuItemList = %treeFolderOperation_C
                [
                txt(menu_add_group, ws_Events():getString(cmdAddGroup_C), noAccelerator, b_true, mis_none, []),
                txt(menu_add_folder, ws_Events():getString(cmdAddFolder_C), noAccelerator, b_true, mis_none, []),
                txt(menu_del_node, ws_Events():getString(cmdDeleteNode_C), noAccelerator, b_true, mis_none, []),
                txt(menu_rename_node, ws_Events():getString(pmnRename), noAccelerator, b_true, mis_none, []),
                txt(menu_Explore, ws_Events():getString(pmnExplore), noAccelerator, b_true, mis_none, [])
                ]
        else
            MenuItemList = %treeNodeOperation_C
                [
                txt(menu_add_group, ws_Events():getString(cmdAddGroup_C), noAccelerator, b_true, mis_none, []),
                txt(menu_add_subgroup, ws_Events():getString(cmdAddSubGroup_C), noAccelerator, b_true, mis_none, []),
                txt(menu_add_folder, ws_Events():getString(cmdAddFolder_C), noAccelerator, b_true, mis_none, []),
                txt(menu_add_childfolder, ws_Events():getString(cmdAddSubFolder_C), noAccelerator, b_true, mis_none, []),
                txt(menu_del_node, ws_Events():getString(cmdDeleteNode_C), noAccelerator, b_true, mis_none, []),
                txt(menu_rename_node, ws_Events():getString(pmnRename), noAccelerator, b_true, mis_none, [])
                ]
        end if,
        PntForm = treeControl_P:mapPointTo(treeControl_P, PntTree),
        treeControl_P:menuPopup(dynmenu(MenuItemList), PntForm, align_left).

/****** Drag And Drop Support *******/
domains
    dragParams=dragParams(treeNode_std DragNode, namedValue* DragNodePath,treeNode_std DropNode, namedValue* DropNodePath).
facts
    dragNodes_V:dragParams:=erroneous.

class predicates
    dragBeginResponder : dragBeginResponder.
clauses
    dragBeginResponder(_DragNode) = acceptDrag(vpiDomains::cursor_Uparrow, vpiDomains::cursor_Cross).
%    dragBeginResponder(_DragNode) = denyDrag.

class predicates
    dropResponder : dropResponder.
clauses
    dropResponder(_Source, dropEvent(DragNode, _DropTarget)) = dropAvailable :-
        _Parent=DragNode:tryGetParent(),
        !.
    dropResponder(_Source, _Any) = dropUnavailable.

predicates
    endDropListener : endDropListener.
clauses
    endDropListener(dropEvent(DragNode, DropTarget)) :-
        string(NodeType)=DropTarget:tryGetValue(nodeID_C),
        if _Parent=DropTarget:tryGetParent() then
            ParentParam=true
        else
            ParentParam=false
        end if,
        if _Child=DropTarget:getChild_nd(),! then
            HasChildren=true
        else
            HasChildren=false
        end if,
        MenuItemList=[MenuItem||
            DropSituation=dropSituation(NodeType,ParentParam,HasChildren),
            dropMenuItem_nd(DropSituation,MenuItem)
        ],
        Rct = treeControl_P:tryGetNodeRect(DropTarget, all),
        !,
        Rct = rct(X, Y1, _, Y2),
        Pnt = pnt(X, (Y1 + Y2) div 2),
        tuple(DragNodePath,_DragNodePathObj)=getNodePath(DragNode,tuple([],[])),
        tuple(DropNodePath,_DropNodePathObj)=getNodePath(DropTarget,tuple([],[])),
        dragNodes_V:=dragParams(DragNode,DragNodePath,DropTarget,DropNodePath),
        PntForm = treeControl_P:mapPointTo(treeControl_P, Pnt),
        treeControl_P:menuPopup(dynmenu(MenuItemList), PntForm, align_left).
    endDropListener(_DropEvent).

predicates
    expandEndListener : expandEndListener.
clauses
    expandEndListener(_Source, _Node, _NewStatus).

domains
    dropSolution=
 % node is empty group
        include;
 % group contains nodes
        putAtBeginning;
        putAtEnd;
 % node is any type exept root
        putAbove;
        putBelow.

class predicates
    dropSituation:(string NodeID,boolean HasParent,boolean IsEmpty)->dropSolution nondeterm.
clauses
    dropSituation(_AnyType,false,_AnyContent)=putAtBeginning.
    dropSituation(_AnyType,false,_AnyContent)=putAtEnd.
    dropSituation(_AnyType,true,_AnyContent)=putAbove.
    dropSituation(_AnyType,true,_AnyContent)=putBelow.
    dropSituation(groupNode_C,_AnyNode,true)=putAtBeginning.
    dropSituation(groupNode_C,_AnyNode,true)=putAtEnd.
    dropSituation(groupNode_C,_AnyNode,false)=include.

/******* Move Node ********/
class predicates
    moveNode:(spbTree::place Place,treeNode_std NodeIdToMove,treeNode_std BaseNodeId,treeControl{treeNode_std}).
clauses
    moveNode(Place,NodeToMove,BaseNode,TreeControl):-
        (Place=spbTree::leftmost or Place=spbTree::rightmost),
        NodeToMoveParent=NodeToMove:tryGetParent(),
        NodeToMoveParent:deleteChild(NodeToMove),
        ChildList=[Child||Child=BaseNode:getChild_nd(),BaseNode:deleteChild(Child)],
        if Place=spbTree::leftmost then
            NewChildList=[NodeToMove|ChildList]
        elseif Place=spbTree::rightmost then
            NewChildList=list::append(ChildList,[NodeToMove])
        else
            NewChildList=ChildList
        end if,
        _Dummy=[""||NewChild=list::getMember_nd(NewChildList),BaseNode:addChild(NewChild)],
        TreeControl:expand(BaseNode,true),
        TreeControl:select(NodeToMove),
        !.
    moveNode(Place,NodeToMove,BaseNode,TreeControl):-
        (Place=spbTree::left or Place=spbTree::right),
        NodeToMoveParent=NodeToMove:tryGetParent(),
        NodeToMoveParent:deleteChild(NodeToMove),
        Parent=BaseNode:tryGetParent(),
        ChildList=[Child||Child=Parent:getChild_nd(),Parent:deleteChild(Child)],
        ChildListLen=list::length(ChildList),
        Index=list::tryGetIndex(BaseNode,ChildList),
        if Place=spbTree::left, Index>=1 then
            list::split(Index,ChildList,Left,Right),
            NewChildList=list::append(Left,[NodeToMove|Right])
        elseif Place=spbTree::right, Index<ChildListLen then
            list::split(Index+1,ChildList,Left,Right),
            NewChildList=list::append(Left,[NodeToMove|Right])
        elseif Place=spbTree::left, Index=0 then
            NewChildList=[NodeToMove|ChildList]
        elseif Place=spbTree::right, Index=ChildListLen then
            NewChildList=list::append(ChildList,[NodeToMove])
        else
            NewChildList=ChildList
        end if,
        _Dummy=[""||NewChild=list::getMember_nd(NewChildList),Parent:addChild(NewChild)],
        TreeControl:expand(Parent,true),
        TreeControl:select(NodeToMove),
        !.
    moveNode(_Direction,_NodeToMove,_BaseNode,_TreeControl):-
        exception::raise_User("Unexpected Altenative").

/****** Convert xmlTermTree to treeControl term *********/
class predicates
    xmlTermTree2TreeControl : (spbTree::tree{string NodeId,namedValue*}) -> treeNode_std.
clauses
    xmlTermTree2TreeControl(spbTree::tree(_NodeID,NodeParamList,[])) = Node :-
        !,
        Node=createNode(NodeParamList).
    xmlTermTree2TreeControl(spbTree::tree(_NodeID,NodeParamList,TreeList)) = Node :-
        Node=createNode(NodeParamList),
        makeChildList(Node, TreeList).

class predicates
    createNode:(namedValue*)->treeNode_std.
clauses
    createNode(NodeParamList)=Node:-
        NodeTitle=namedValue::getNamed_string(NodeParamList,title_C),
        NodeID=namedValue::getNamed_string(NodeParamList,nodeID_C),
        Node = treeNode_std::new(NodeTitle),
        NodeImage=getNodeImage(NodeID),
        !,
        Node:bitmapIdx:=NodeImage,
        Node:selectedBitmapIdx:=NodeImage,
        _DummyList=[""||NodeParam=list::getMember_nd(NodeParamList),Node:addNamedValue(NodeParam)].

class predicates
    getNodeImage:(string NodeID)->integer multi.
clauses
    getNodeImage(workSpace_C)=0.
    getNodeImage(groupNode_C)=1.
    getNodeImage(folder_C)=2.
    getNodeImage(_Any)=_:-
        exception::raise_User("UnExpected Alternative!").

class predicates
    makeChildList : (treeNode_std,spbTree::tree{string NodeId,namedValue*}*).
clauses
    makeChildList(_,[]).
    makeChildList(Node, [CNodeTerm | CNodeTermList]) :-
        if
            CNodeTerm=spbTree::tree(_NodeId,Data,_TreeList),
            _NodeTitle=namedValue::tryGetNamed_string(Data,title_C),
            CNode = xmlTermTree2TreeControl(CNodeTerm),
            Node:addChild(CNode)
        then
            succeed()
        end if,
        makeChildList(Node, CNodeTermList).

clauses
    tryGetSelectedNodePath()=NodePath:-
        Node = treeControl_P:tryGetSelected(),
        NodePath=getNodePath(Node,tuple([],[])).

/******* Get Tree Control Node Path *******/
clauses
    getNodePath(Node,tuple([],[]))=Result:-
        string(NodeID)=Node:tryGetValue(nodeID_C),
        string(Title)=Node:tryGetValue(title_C),
        !,
        Result=getNodePath(Node,tuple([namedValue(NodeID,string(Title))],[Node])).
    getNodePath(Node,tuple(NodePath,ObjPath))=Result:-
        Parent=Node:tryGetParent(),
        string(ParentID)=Parent:tryGetValue(nodeID_C),
        string(Title)=Parent:tryGetValue(title_C),
        !,
        Result=getNodePath(Parent,tuple([namedValue(ParentID,string(Title))|NodePath],[Parent|ObjPath])).
    getNodePath(_Node,Path)=Path.

end implement wsFE_SourceTree

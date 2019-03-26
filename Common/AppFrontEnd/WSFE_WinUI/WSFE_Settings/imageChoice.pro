%

implement imageChoice
    inherits dialog
    inherits wsFE_Connector
    open core, vpiDomains, listViewControl, ws_EventManager

constants
    imageWidth_C = 16.
    imageHeight_C = 16.

clauses
    display(Parent) = Dialog :-
        Dialog = new(Parent),
        Dialog:show().

clauses
    new(Parent) :-
        wsFE_Connector::new(convert(wsFE_Connector, Parent):wsFE_P),
        dialog::new(Parent),
        generatedInitialize(),
        initListViewControl().

predicates
    initListViewControl : ().
clauses
    initListViewControl():-
        ImageList = imageList::new(imageWidth_C, imageHeight_C),
        listViewControl_ctl:imageList := ImageList,
        addImages(ImageList),
        listViewControl_ctl:setLVType(listViewControl::lvs_report),
        listViewControl_ctl:setStyle([
            listViewControl::lvs_showselalways,
            listViewControl::lvs_autoarrange,
            listViewControl::lvs_noSortHeader
            ]),
        listViewControl_ctl:setLvExStyle(listViewControl::lvs_ex_checkBoxes + listViewControl::lvs_ex_gridLines + listViewControl::lvs_ex_fullRowSelect),
        ColumnList = [
             column("Index", 50, alignleft),
             column(ws_Events():getString(colSourceFile), 200, alignleft),
             column(ws_Events():getString(colPath), 400, alignleft)
                             ],
        listViewControl_ctl:insertColumnList(1, ColumnList).

predicates
    addImages : (imageList ImageList).
clauses
    addImages(ImageList):-
        foreach
            Ext in [".vipprj", ".cmd", ".xlsx"],
            string(ExtValue) = registry::getValue(registry::classesRoot, Ext, ""),
            string(AppValue) = registry::getValue(registry::classesRoot, string::concat(ExtValue, @"\DefaultIcon"), ""),
            string::splitStringBySeparators(AppValue, ",", ExeFile, _, IconIndex),
            Index = tryToTerm(integer, IconIndex),
            FullExeFile = environment::expand(ExeFile)
        do
            Count = shell_native::extractIconEx(FullExeFile, -1, null, null, 0),
            L = arrayM_inline{gui_native::hIcon}::new(1),
            S = arrayM_inline{gui_native::hIcon}::new(1),
            Return = shell_native::extractIconEx(FullExeFile, Index, L:data, S:data, 1),
            stdio::writef("Aarray: %\n", L:valueList),
            stdio::writef("Barray: %\n", S:valueList),
            nothing(Return)
%            Idx = ImageList:addIcon(HIcon)
        end foreach,
        !.

% This code is maintained automatically, do not update it manually.
%  11:29:06-4.7.2018

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    ribbonControl_ctl : ribboncontrol.
    listViewControl_ctl : listviewcontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("imageChoice"),
        setRect(rct(50, 40, 411, 266)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(169, 204),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(233, 204),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(297, 204),
        help_ctl:setSize(56, 16),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        ribbonControl_ctl := ribboncontrol::new(This),
        ribbonControl_ctl:setPosition(4, 4),
        ribbonControl_ctl:setSize(352, 22),
        ribbonControl_ctl:setAnchors([control::left, control::top, control::right]),
        listViewControl_ctl := listviewcontrol::new(This),
        listViewControl_ctl:setPosition(4, 28),
        listViewControl_ctl:setSize(352, 170),
        listViewControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]).
% end of automatic code
end implement imageChoice
%

implement performSourceCommand
    inherits wsFE_Connector
    open core, treeControl{treeNode_std}, ribbonControl, wSFE_Command, ws_eventManager

facts
    invokeCmd_F : (integer Index,command InvokeCmd,command InvokeAllCmd,menuCommand CommandMenu).

    stopRunCmd : command:=erroneous.
    pauseRunCmd : command:=erroneous.
    resetRunCmd : command:=erroneous.
    selResetRunCmd : command:=erroneous.
    resetMenuCmd : menuCommand:=erroneous.

    isPauseRun : boolean := false.

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        wsFE_SourceTree():treeControl_P:addSelectEndListener(onTreeNodeSelected),
        wsFE_SourceTree():treeControl_P:addGetFocusListener(onTreeGetFocus),
        wsFE_SourceList():sourceList_P:addSelectEndListener(onListRowSelected).

clauses
    addChangeListener():-
        ws_Events():changeLanguageEvent:addListener(
            {:-
            resetRunCmd:menuLabel:=ws_Events():getString(cmdResetAll_C),
            resetRunCmd:ribbonLabel := ws_Events():getString(cmdResetAll_C),
            resetRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipResetAll_C)),
            selResetRunCmd:menuLabel:=ws_Events():getString(cmdResetSel_C),
            selResetRunCmd:ribbonLabel := ws_Events():getString(cmdResetSel_C),
            selResetRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipResetSel_C)),
            stopRunCmd:menuLabel:=ws_Events():getString(cmdStop_C),
            stopRunCmd:ribbonLabel := ws_Events():getString(cmdStop_C),
            stopRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipStop_C)),
            pauseRunCmd:menuLabel:=ws_Events():getString(cmdPause_C),
            pauseRunCmd:ribbonLabel := ws_Events():getString(cmdPause_C),
            pauseRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipPause_C))
            }).

clauses
    initWS_PerformReset_Menu(WS_Form)= block([  [cmd(resetMenuCmd:id, imageAndText(vertical))]  ]) :-
        _ResetSourceBlock=initCommandBlock("resetRun",WS_Form,resetAll),
        _SelResetSourceBlock=initCommandBlock("selResetRun",WS_Form,resetSelect),
        resetMenuCmd:=menuCommand::new(WS_Form,"resetSource"),
        resetMenuCmd:style :=menuCommand::toolMenu,
        resetMenuCmd:icon:=core::none,
        resetMenuCmd:layout:=menuCommand::menuStatic(
            [
            menuCommand::cmd(selResetRunCmd),
            menuCommand::cmd(resetRunCmd)
            ]),
        resetMenuCmd:enabled := true.

    initWS_PerformPauseRun_Menu(WS_Form)= block([ [cmd(pauseRunCmd:id,imageAndText(vertical))],
                                                                                    [cmd(stopRunCmd:id,imageAndText(vertical))]
                                                                                    ]):-
        _PauseRunSourceBlock=initCommandBlock("pauseRun",WS_Form,pauseRun),
        _StopRunSourceBlock=initCommandBlock("stopRun",WS_Form,stopRun).

clauses
    initWS_PerformCommand_Menu(WS_Form) =
        [block([[cmd(CommandMenu:id, imageAndText(vertical))]]) ||
            retractAll(invokeCmd_F(_, _, _, _)),
            Index = std::cIterate(commandMax)+1,
                CmdID = string::format("cmd_%", Index),
                Text = string::format("Command #%d", Index),
                InvokeCmd = command::new(WS_Form, CmdID),
                InvokeCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(runIcon16_C),bitmap::createFromBinary(runIcon32_C)])),
                initInvokeCmd(InvokeCmd, Text, {:- invokeCommand(Index, false)}),
                AllCmdID = string::format("cmdAll_%", Index),
                TextAll = string::format("Run All\n command"),
                InvokeAllCmd = command::new(WS_Form, AllCmdID),
                InvokeAllCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(runAllIcon16_C),bitmap::createFromBinary(runAllIcon32_C)])),
                initInvokeCmd(InvokeAllCmd, TextAll, {:- invokeCommand(Index, true)}),
                CommandID = string::format("command_%", Index),
                CommandMenu = menuCommand::new(WS_Form, CommandID),
                CommandMenu:style :=menuCommand::toolMenu,
                CommandMenu:icon:=core::none,
                CommandMenu:layout:=menuCommand::menuStatic(
                    [
                    menuCommand::cmd(InvokeCmd),
                    menuCommand::cmd(InvokeAllCmd)
                    ]),
                CommandMenu:enabled := true,
                assert(invokeCmd_F(Index, InvokeCmd, InvokeAllCmd, CommandMenu))
        ].

class predicates
    initInvokeCmd : (command Command, string Text, predicate{command} Predicate).clauses
    initInvokeCmd(InvokeCmd, Text, Predicate):-
        !,
        InvokeCmd:menuLabel := Text,
        InvokeCmd:ribbonLabel := Text,
        InvokeCmd:tipTitle := tooltip::tip(Text),
        InvokeCmd:run:=Predicate,
        InvokeCmd:enabled := false.

clauses
    initCommandBlock("resetRun",Win,Predicate)=block([  [cmd(resetRunCmd:id,imageAndText(vertical))]  ]):-
        !,
        resetRunCmd := command::new(Win, "Reset"),
        resetRunCmd:menuLabel:=ws_Events():getString(cmdResetAll_C),
        resetRunCmd:ribbonLabel := ws_Events():getString(cmdResetAll_C),
        resetRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipResetAll_C)),
        resetRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(resetIcon16_C),bitmap::createFromBinary(resetIcon32_C)])),
        resetRunCmd:run :=Predicate,
        resetRunCmd:enabled := true.
    initCommandBlock("selResetRun",Win,Predicate)=block([  [cmd(selResetRunCmd:id,imageAndText(vertical))]  ]):-
        !,
        selResetRunCmd := command::new(Win, "SelReset"),
        selResetRunCmd:menuLabel:=ws_Events():getString(cmdResetSel_C),
        selResetRunCmd:ribbonLabel := ws_Events():getString(cmdResetSel_C),
        selResetRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipResetSel_C)),
        selResetRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(selResetIcon16_C),bitmap::createFromBinary(selResetIcon32_C)])),
        selResetRunCmd:run :=Predicate,
        selResetRunCmd:enabled := true.
    initCommandBlock("stopRun",Win,Predicate)=block([  [cmd(stopRunCmd:id,imageAndText(vertical))]  ]):-
        !,
        stopRunCmd := command::new(Win, "Stop"),
        stopRunCmd:menuLabel:=ws_Events():getString(cmdStop_C),
        stopRunCmd:ribbonLabel := ws_Events():getString(cmdStop_C),
        stopRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipStop_C)),
        stopRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(stopIcon16_C),bitmap::createFromBinary(stopIcon32_C)])),
        stopRunCmd:run :=Predicate,
        stopRunCmd:enabled := false.
    initCommandBlock("pauseRun",Win,Predicate)=block([  [cmd(pauseRunCmd:id,imageAndText(vertical))]  ]):-
        !,
        pauseRunCmd := command::new(Win, "Pause"),
        pauseRunCmd:menuLabel:=ws_Events():getString(cmdPause_C),
        pauseRunCmd:ribbonLabel := ws_Events():getString(cmdPause_C),
        pauseRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipPause_C)),
        pauseRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(pauseIcon16_C),bitmap::createFromBinary(pauseIcon32_C)])),
        pauseRunCmd:run :=Predicate,
        pauseRunCmd:enabled := false.

    initCommandBlock(_Any,_Win,_Predicate)=_:-
        exception::raise_User("Unexpected Alternative!").

predicates
    onTreeNodeSelected:selectEndListener.
clauses
    onTreeNodeSelected(_TreeControl, _SlectionOld,_SlectionNew):-
        setMenuAblity().

predicates
    onListRowSelected: listViewControl::selectEndListener.
clauses
    onListRowSelected(_Source, _ItemId, _Select) :-
        setMenuAblity().

predicates
    setMenuAblity:().
clauses
    setMenuAblity():-
        Selected = toBoolean( [] <> wSFE_SourceList():sourceList_P:getSel() ),
        ExistsItems = toBoolean( 0 < wSFE_SourceList():sourceList_P:getItemCount() ),
        if Selected=true, wsFE_Tasks():sourceIsRunning_P=true then
            foreach invokeCmd_F(_Index, InvokeCmd, InvokeAllCmd, MenuCommand) do
                InvokeCmd:enabled := false,
                InvokeAllCmd:enabled := false,
                MenuCommand:enabled := false
            end foreach
        elseif wsFE_Tasks():sourceIsRunning_P=false then
            foreach invokeCmd_F(_Index, InvokeCmd, InvokeAllCmd, _MenuCommand) do
                InvokeCmd:enabled := Selected,
                InvokeAllCmd:enabled := ExistsItems
            end foreach
        end if.

clauses
    setEnabledExecuteCmd(_Value):-
        setMenuAblity().

clauses
    updateCommandRibbon(Index, CmdName, Enabled):-
        if invokeCmd_F(Index, InvokeCmd, InvokeAllCmd, MenuCommand) then
            InvokeCmd:menuLabel := CmdName,
            InvokeCmd:ribbonLabel := CmdName,
            InvokeCmd:tipTitle := tooltip::tip(CmdName),
            InvokeAllCmd:menuLabel := string::concat(CmdName, " All"),
            InvokeAllCmd:ribbonLabel := string::concat(CmdName, " All"),
            InvokeAllCmd:tipTitle := tooltip::tip(string::concat(CmdName, " All")),
            MenuCommand:enabled := Enabled
        end if.

predicates
    onTreeGetFocus : window::getFocusListener.
clauses
    onTreeGetFocus(_):-
        foreach invokeCmd_F(Index, InvokeCmd, InvokeAllCmd, MenuCommand) do
            CmdText = string::format("%s #%d", ws_Events():getString(stCommand), Index),
            InvokeCmd:menuLabel := CmdText,
            InvokeCmd:ribbonLabel := CmdText,
            InvokeCmd:tipTitle := tooltip::tip(CmdText),
            InvokeAllCmd:menuLabel := string::concat(CmdText, " All"),
            InvokeAllCmd:ribbonLabel := string::concat(CmdText, " All"),
            InvokeAllCmd:tipTitle := tooltip::tip(string::concat(CmdText, " All")),
            MenuCommand:enabled := true
        end foreach,
        wsFE_Form():prevSelectExt := "",
        wsFE_Form():ribbonControl_P:invalidate().

clauses
    restoreResetState(IsSelect):-
        if IsSelect = true then
            resetMenuCmd:activeTool := some(resetRunCmd)
        else
            resetMenuCmd:activeTool := some(selResetRunCmd)
        end if.

predicates
    invokeCommand : (integer Index, boolean TrueIfAll).
clauses
    invokeCommand(Index, TrueIfAll):-
        stopRunCmd:enabled := true,
        wsFE_Tasks():sourceIsRunning_P := true,
        if false = TrueIfAll then
            ItemsIDToRun = wSFE_SourceList():sourceList_P:getSel()
        else
            ItemsIDToRun = wSFE_SourceList():sourceList_P:getAll()
        end if,
        if [_,_|_] = ItemsIDToRun then
            pauseRunCmd:enabled := true
        end if,
        wsFE_Tasks():invoke(Index, TrueIfAll, [toString(ItemID)||ItemID=list::getMember_nd(ItemsIDToRun)]),
        if false = isPauseRun then
            disabledRibbonPauseBlock(),
            stopRunCmd:enabled := false,
            wsFE_Tasks():sourceIsRunning_P := false
        end if.

clauses
    disabledRibbonPauseBlock():-
        foreach invokeCmd_F(_Index, InvokeCmd, InvokeAllCmd, MenuCommand) do
            InvokeCmd:enabled := true,
            InvokeAllCmd:enabled := true,
            MenuCommand:enabled := true
        end foreach,
        pauseRunCmd:enabled := false.

predicates
    stopRun : (command).
clauses
    stopRun(_) :-
        if true = isPauseRun then
            wsFE_Tasks():pauseRun(false),
            isPauseRun := false,
            pauseRunCmd:ribbonLabel := ws_Events():getString(cmdPause_C),
            pauseRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(pauseIcon16_C),bitmap::createFromBinary(pauseIcon32_C)])),
            if tuple(SourceNodePath,_ObjPAth)=wsFE_SourceTree():tryGetSelectedNodePath() then
                wsFE_Tasks():updateNodeContent(SourceNodePath)
            end if
        end if,
        wsFE_Tasks():stopRun(),
        _ = vpi::processEvents(),
        disabledRibbonPauseBlock(),
        stopRunCmd:enabled := false,
        wsFE_Tasks():sourceIsRunning_P := false.

predicates
    resetAll : (command).
clauses
    resetAll(_) :-
        wsFE_Tasks():resetStatus(true).

predicates
    resetSelect : (command).
clauses
    resetSelect(_) :-
        wsFE_Tasks():resetStatus(false).

predicates
    pauseRun : (command).
clauses
    pauseRun(_):-
        if pauseRunCmd:ribbonLabel = ws_Events():getString(cmdPause_C) then
            wsFE_Tasks():pauseRun(true),
            isPauseRun := true,
            _ = vpi::processEvents(),
            pauseRunCmd:ribbonLabel := ws_Events():getString(cmdRun_C),
            pauseRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(contRunIcon16_C),bitmap::createFromBinary(contRunIcon32_C)]))
        else
            wsFE_Tasks():pauseRun(false),
            isPauseRun := false,
            pauseRunCmd:ribbonLabel := ws_Events():getString(cmdPause_C),
            pauseRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(pauseIcon16_C),bitmap::createFromBinary(pauseIcon32_C)])),
            wsFE_Tasks():continueRun(),
            if false = isPauseRun then
                disabledRibbonPauseBlock(),
                stopRunCmd:enabled := false,
                wsFE_Tasks():sourceIsRunning_P := false
            end if
       end if.

end implement performSourceCommand
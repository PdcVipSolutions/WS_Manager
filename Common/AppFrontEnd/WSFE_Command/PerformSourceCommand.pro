%

implement performSourceCommand
    inherits wsFE_Connector
    open core, stdio, vpiDomains,treeControl{treeNode_std}, ribbonControl, wSFE_Command, ws_eventManager

facts
    runCmd : command:=erroneous.
    reRunCmd : command:=erroneous.
    runSelectCmd : menucommand:=erroneous.

    runAllCmd : command:=erroneous.
    reRunAllCmd : command:=erroneous.
    runSelectAllCmd : menucommand:=erroneous.

    stopRunCmd : command:=erroneous.
    pauseRunCmd : command:=erroneous.
    execRunCmd : command:=erroneous.
    resetRunCmd : command:=erroneous.
    selResetRunCmd : command:=erroneous.
    resetMenuCmd : menuCommand:=erroneous.

    isPauseRun : boolean := false.

    execCmdOn : (string ExtName, string* ExtList, boolean Enabled).

clauses
    new(FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        wsFE_SourceTree():treeControl_P:addSelectEndListener(onTreeNodeSelected),
        wsFE_SourceList():sourceList_P:addSelectEndListener(onListRowSelected).

clauses
    addChangeListener():-
        ws_Events():changeLanguageEvent:addListener(
            {:-
            runCmd:menuLabel:=ws_Events():getString(cmdRun_C),
            runCmd:ribbonLabel := ws_Events():getString(cmdRun_C),
            runCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipRun_C)),
            reRunCmd:menuLabel:=ws_Events():getString(cmdReRun_C),
            reRunCmd:ribbonLabel := ws_Events():getString(cmdReRun_C),
            reRunCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipReRun_C)),
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
            pauseRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipPause_C)),
            execRunCmd:menuLabel:=ws_Events():getString(cmdExec_C),
            execRunCmd:ribbonLabel := ws_Events():getString(cmdExec_C),
            execRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipExec_C)),
            runAllCmd:menuLabel:=ws_Events():getString(cmdRunAll_C),
            runAllCmd:ribbonLabel := ws_Events():getString(cmdRunAll_C),
            runAllCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipRunAll_C)),
            reRunAllCmd:menuLabel:=ws_Events():getString(cmdReRunAll_C),
            reRunAllCmd:ribbonLabel := ws_Events():getString(cmdReRunAll_C),
            reRunAllCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipReRunAll_C))
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
                                                                                    [cmd(stopRunCmd:id,imageAndText(vertical))],
                                                                                    [cmd(execRunCmd:id,imageAndText(vertical))] ]):-
        _PauseRunSourceBlock=initCommandBlock("pauseRun",WS_Form,pauseRun),
        _StopRunSourceBlock=initCommandBlock("stopRun",WS_Form,stopRun),
        _ExecRunSourceBlock=initCommandBlock("execRun",WS_Form,execRun).

    initWS_PerformSource_Menu(WS_Form)=RunBlockMenu:-
        _ReRunSourceBlock=initCommandBlock("reRunSource",WS_Form,{:-performRun(runCmd,true)}),
        _RunSourceBlock=initCommandBlock("runSource",WS_Form,{:-performRun(runCmd,false)}),
        RunBlockMenu=initCommandBlock("runMenu",WS_Form,dummyRun).

    initWS_PerformAllSource_Menu(WS_Form)=RunAllBlockMenu:-
        _ReRunAllSourceBlock=initCommandBlock("reRunAllSource",WS_Form,{:-performRun(runAllCmd,true)}),
        _RunAllSourceBlock=initCommandBlock("runAllSource",WS_Form,{:-performRun(runAllCmd,false)}),
        RunAllBlockMenu=initCommandBlock("runAllMenu",WS_Form,dummyRun).

clauses
    initCommandBlock("runSource",Win,Predicate)=block([  [cmd(runCmd:id, textOnly)]  ]):-
        !,
        runCmd := command::new(Win,"run"),
        runCmd:menuLabel:=ws_Events():getString(cmdRun_C),
        runCmd:ribbonLabel := ws_Events():getString(cmdRun_C),
        runCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipRun_C)),
        runCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(runIcon16_C),bitmap::createFromBinary(runIcon32_C)])),
        runCmd:run:=Predicate,
        runCmd:enabled := false.
    initCommandBlock("reRunSource",Win,Predicate)=block([  [cmd(reRunCmd:id, textOnly)]  ]):-
        !,
        reRunCmd := command::new(Win,"reRun"),
        reRunCmd:menuLabel:=ws_Events():getString(cmdReRun_C),
        reRunCmd:ribbonLabel := ws_Events():getString(cmdReRun_C),
        reRunCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipReRun_C)),
        reRunCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(reRunIcon16_C),bitmap::createFromBinary(reRunIcon32_C)])),
        reRunCmd:run:=Predicate,
        reRunCmd:enabled := false.
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
    initCommandBlock("execRun",Win,Predicate)=block([  [cmd(execRunCmd:id,imageAndText(vertical))]  ]):-
        !,
        execRunCmd := command::new(Win, "Exec"),
        execRunCmd:menuLabel:=ws_Events():getString(cmdExec_C),
        execRunCmd:ribbonLabel := ws_Events():getString(cmdExec_C),
        execRunCmd:tipTitle:=toolTip::tip(ws_Events():getString(tipExec_C)),
        execRunCmd:icon := some(icon::createFromImages([bitmap::createFromBinary(execIcon16_C),bitmap::createFromBinary(execIcon32_C)])),
        execRunCmd:run :=Predicate,
        execRunCmd:enabled := false.
    initCommandBlock("runMenu",Win,_Predicate)=block([  [cmd(runSelectCmd:id, imageAndText(vertical))]]):-
        !,
        runSelectCmd:=menuCommand::new(Win,"run/reRun"),
%        runSelectCmd:menuLabel:="Run",
%        runSelectCmd:tipTitle:=toolTip::tip("Run"),
%        runSelectCmd:tipBody:=toolTip::tip("Win kind of menu button toggles tools in and out. Whenever a new tool is selected, it becomes the default tool shown on menu button."),
        runSelectCmd:style :=menuCommand::toolMenu,
        runSelectCmd:icon:=core::none,
        runSelectCmd:layout:=menuCommand::menuStatic(
            [
            menuCommand::cmd(runCmd),
            menuCommand::cmd(reRunCmd)
            ]),
        runSelectCmd:enabled := false.
    initCommandBlock("runAllSource",Win,Predicate)=block([  [cmd(runAllCmd:id, imageAndText(vertical))]  ]):-
        !,
        runAllCmd:=command::new(Win,"run all"),
        runAllCmd:menuLabel:=ws_Events():getString(cmdRunAll_C),
        runAllCmd:ribbonLabel := ws_Events():getString(cmdRunAll_C),
        runAllCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipRunAll_C)),
        runAllCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(runAllIcon16_C),bitmap::createFromBinary(runAllIcon32_C)])),
        runAllCmd:run:=Predicate,
        runAllCmd:enabled := false.
    initCommandBlock("reRunAllSource",Win,Predicate)=block([  [cmd(reRunAllCmd:id, imageAndText(vertical))]  ]):-
        !,
        reRunAllCmd :=command::new(Win,"reRun all"),
        reRunAllCmd:menuLabel:=ws_Events():getString(cmdReRunAll_C),
        reRunAllCmd:ribbonLabel := ws_Events():getString(cmdReRunAll_C),
        reRunAllCmd:tipTitle:=tooltip::tip(ws_Events():getString(tipReRunAll_C)),
        reRunAllCmd:icon:=some(icon::createFromImages([bitmap::createFromBinary(reRunAllIcon16_C),bitmap::createFromBinary(reRunAllIcon32_C)])),
        reRunAllCmd:run:=Predicate,
        reRunAllCmd:enabled := false.
    initCommandBlock("runAllMenu",Win,_Predicate)=block([  [cmd(runSelectAllCmd:id, imageAndText(vertical))]  ]):-
        !,
        runSelectAllCmd:=menuCommand::new(Win,"run/reRunAll"),
%        runSelectAllCmd:menuLabel:="Run All",
%        runSelectAllCmd:tipTitle:=toolTip::tip("Run All"),
%        runSelectAllCmd:tipBody:=toolTip::tip("Win kind of menu button toggles tools in and out. Whenever a new tool is selected, it becomes the default tool shown on menu button."),
        runSelectAllCmd:style :=menuCommand::toolMenu,
        runSelectAllCmd:icon:=core::none,
        runSelectAllCmd:layout:=menuCommand::menuStatic(
            [
            menuCommand::cmd(runAllCmd),
            menuCommand::cmd(reRunAllCmd)
            ]),
        runSelectAllCmd:enabled := false.

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
            runCmd:enabled := false,
            runAllCmd:enabled := false,
            reRunCmd:enabled := false,
            reRunAllCmd:enabled := false,
            runSelectCmd:enabled := false,
            runSelectAllCmd:enabled := false,
            execRunCmd:enabled := false
        elseif wsFE_Tasks():sourceIsRunning_P=false then
            runCmd:enabled := Selected,
            runAllCmd:enabled := ExistsItems,
            reRunCmd:enabled := Selected,
            reRunAllCmd:enabled := ExistsItems,
            runSelectCmd:enabled := Selected,
            runSelectAllCmd:enabled := ExistsItems,
            if
                FileName = wSFE_SourceList():tryGetSelectSourceFileName(),
                Ext = filename::getExtension(FileName),
                execCmdOn(_ExtName,  ExtList, Enabled),
                string::toLowerCase(Ext) in ExtList
            then
                execRunCmd:enabled := Enabled
            else
                execRunCmd:enabled := false
            end if
        end if.

domains
    enabledList = tuple{string, string, string}*.
clauses
    setEnabledExecuteCmd(Value):-
        EnabledList = toTerm(enabledList, Value),
        foreach tuple(ExtName, ExtListStr, EnabledStr) in EnabledList do
            retractAll(execCmdOn(ExtName, _, _)),
            assert(execCmdOn(ExtName,  string::split_delimiter(ExtListStr, ","), toBoolean("true" = EnabledStr)))
        end foreach,
        setMenuAblity().

predicates
    dummyRun : (command).
clauses
    dummyRun(_).

clauses
    restoreResetState(IsSelect):-
        if IsSelect = true then
            resetMenuCmd:activeTool := some(resetRunCmd)
        else
            resetMenuCmd:activeTool := some(selResetRunCmd)
        end if.

predicates
    performRun:(command CommandId, boolean TrueIfReRun).
clauses
    performRun(Command,TrueIfReRun):-
        stopRunCmd:enabled := true,
        wsFE_Tasks():sourceIsRunning_P := true,
        if Command=runCmd then
            runSelectCmd:activeTool := some(Command),
            runSelectCmd:enabled := false,
            if [_,_|_] = wSFE_SourceList():sourceList_P:getSel() then
                pauseRunCmd:enabled := true
            end if,
            wSFE_SourceList():run(false,TrueIfReRun)
        else % Command=runAllCmd
            runSelectAllCmd:activeTool := some(Command),
            runSelectAllCmd:enabled := false,
            pauseRunCmd:enabled := true,
            wSFE_SourceList():run(true,TrueIfReRun)
        end if,
        if false = isPauseRun then
            disabledRibbonPauseBlock(),
            stopRunCmd:enabled := false,
            wsFE_Tasks():sourceIsRunning_P := false
        end if.

clauses
    disabledRibbonPauseBlock():-
        runSelectCmd:enabled := true,
        runSelectAllCmd:enabled := true,
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
    execRun : (command).
clauses
    execRun(_):-
        wSFE_SourceList():execRun().

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
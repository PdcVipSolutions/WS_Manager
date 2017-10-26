%

implement wSFE_Command
    inherits wsFE_Connector

    open core, ribbonControl, ws_eventManager

clauses
    new(WS_Form,FrontEnd):-
        wsFE_Connector::new(FrontEnd),
        initCommands(WS_Form).

facts - command_FB
    workSpaceHanling_V:workSpaceCommand.
    sourceContentHandling_V:sourceContentCommand.
    moveRemoveContentHandling_V:moveRemoveCommand.
    performSourceHandling_V:performSourceCommand.

    buildBlockMenu_V:ribbonControl::block.
    buildAllBlockMenu_V:ribbonControl::block.
    pauseRunBlockMenu_V:ribbonControl::block.
    performSourceSection_V:ribbonControl::section.

    localOptionsShowCmd : checkCommand:=erroneous.

    showFilterCmd : menucommand:=erroneous.
    ws_Form_V : window.

predicates
    initCommands:(window WS_Form).
clauses
    initCommands(WS_Form):-
        ws_Form_V := WS_Form,
 % Commands
        workSpaceHanling_V:=workSpaceCommand::new(wsFE_P),
        WorkSpaceMenu=workSpaceHanling_V:initWS_Menu(WS_Form),
        TreeMenu=workSpaceHanling_V:initWSTree_Menu(WS_Form),
        workSpaceHanling_V:addChangeListener(),

        moveRemoveContentHandling_V:=moveRemoveCommand::new(wsFE_P),
        MoveRemoveBlockMenu=moveRemoveContentHandling_V:initMoveRemoveNode_Menu(WS_Form),
%        moveRemoveContentHandling_V:addChangeListener(),

        sourceContentHandling_V:=sourceContentCommand::new(wsFE_P),
        AddEntityBlockMenu=sourceContentHandling_V:initWS_AddRemoveSource_Menu(WS_Form),

        performSourceHandling_V:=performSourceCommand::new(wsFE_P),
        buildBlockMenu_V:=performSourceHandling_V:initWS_PerformSource_Menu(WS_Form),
        buildAllBlockMenu_V:=performSourceHandling_V:initWS_PerformAllSource_Menu(WS_Form),
        ResetRunBlockMenu=performSourceHandling_V:initWS_PerformReset_Menu(WS_Form),
        pauseRunBlockMenu_V:=performSourceHandling_V:initWS_PerformPauseRun_Menu(WS_Form),
        performSourceHandling_V:addChangeListener(),

        AboutCmd = command::new(WS_Form,"about"),
        AboutCmd:menuLabel := ws_Events():getString(cmdAbout_C),
        AboutCmd:ribbonLabel := ws_Events():getString(cmdAbout_C),
        AboutCmd:tipTitle := toolTip::tip(ws_Events():getString(tipAbout_C)),
        AboutCmd:icon := some(icon::createFromImages(
            [bitmap::createFromBinary(helpIcon16_C),
            bitmap::createFromBinary(helpIcon32_C)
            ])),
        AboutCmd:run := about,
        AboutCmd:enabled := true,
        HelpAndAbout = block([[cmd(AboutCmd:id, imageAndText(vertical))]]),

        DesignCmd = command::new(WS_Form, "ribbon.design"),
        DesignCmd:menuLabel := ws_Events():getString(cmdDesign_C),
        DesignCmd:ribbonLabel := ws_Events():getString(cmdDesign_C),
        DesignCmd:tipTitle := toolTip::tip(ws_Events():getString(tipDesign_C)),
        DesignCmd:icon :=some(icon::createFromImages(
                [
                    bitmap::createFromBinary(designIcon16_C),
                    bitmap::createFromBinary(designIcon32_C)
                ])),
        DesignCmd:run := onDesign,
        Design =  block([[cmd(DesignCmd:id, imageAndText(vertical))]]),
        SettingsDlgCmd = command::new(WS_Form,"settingsdlg"),
        SettingsDlgCmd:menuLabel := ws_Events():getString(cmdOptions_C),
        SettingsDlgCmd:ribbonLabel := ws_Events():getString(cmdOptions_C),
        SettingsDlgCmd:tipTitle := toolTip::tip(ws_Events():getString(tipOptions_C)),
        SettingsDlgCmd:icon := some(icon::createFromImages(
            [bitmap::createFromBinary(extOptions_C),
            bitmap::createFromBinary(extOptions_C)
            ])),
        SettingsDlgCmd:run := onSettingsDlg,
        SettingsDlgCmd:enabled := true,
        SettingsDlg = block([[cmd(SettingsDlgCmd:id, imageAndText(vertical))]]),
        localOptionsShowCmd := checkCommand::new(WS_Form, "showlocaloptions"),
        localOptionsShowCmd:menuLabel := ws_Events():getString(cmdLocalOptions_C),
        localOptionsShowCmd:ribbonLabel := ws_Events():getString(cmdLocalOptions_C),
        localOptionsShowCmd:tipTitle := toolTip::tip(ws_Events():getString(tipLocalOptions_C)),
        localOptionsShowCmd:icon :=
            some(
                icon::createFromImages(
                    [bitmap::createFromBinary(showLocalOptions16_C),
                    bitmap::createFromBinary(showLocalOptions32_C)
                    ])),
        localOptionsShowCmd:checked := true,
        localOptionsShowCmd:checkChangeEvent:addListener({:- wsFE_Form():showLocalOptionsPanel(localOptionsShowCmd:checked)}),
        LocalOptionsShow = block([[cmd(localOptionsShowCmd:id, imageAndText(vertical))]]),

        initFilterCommand(WS_Form),
        FilterCommand = block([[cmd(showFilterCmd:id, imageAndText(vertical))]]),

        WorkSpaceSection = section("workSpace", ws_Events():getString(sctWorkspace), toolTip::noTip,core::none,[WorkSpaceMenu, TreeMenu]),
        MoveRemoveSection = section("manipulate", ws_Events():getString(sctManipulate), toolTip::noTip,core::none,[MoveRemoveBlockMenu]),
        ProjectSection = section("sourceSpace", ws_Events():getString(sctSrcEditing), toolTip::noTip,core::none,[AddEntityBlockMenu]),
        ws_Events():changeLanguageEvent:addListener(
            {:-
            AboutCmd:ribbonLabel := ws_Events():getString(cmdAbout_C),
            AboutCmd:tipTitle := toolTip::tip(ws_Events():getString(tipAbout_C)),
            DesignCmd:ribbonLabel := ws_Events():getString(cmdDesign_C),
            DesignCmd:tipTitle := toolTip::tip(ws_Events():getString(tipDesign_C)),
            SettingsDlgCmd:ribbonLabel := ws_Events():getString(cmdOptions_C),
            SettingsDlgCmd:tipTitle := toolTip::tip(ws_Events():getString(tipOptions_C)),
            showFilterCmd:ribbonLabel := ws_Events():getString(cmdFilter_C),
            showFilterCmd:tipTitle := toolTip::tip(ws_Events():getString(tipFilter_C)),
            localOptionsShowCmd:ribbonLabel := ws_Events():getString(cmdLocalOptions_C),
            localOptionsShowCmd:tipTitle := toolTip::tip(ws_Events():getString(tipLocalOptions_C))
            }),
        performSourceSection_V := section("actions",ws_Events():getString(sctSrcActions),toolTip::noTip,core::none,
                                                              [buildBlockMenu_V, buildAllBlockMenu_V, pauseRunBlockMenu_V, ResetRunBlockMenu]),
        AboutCmdSection = section("about",ws_Events():getString(sctHelpAbout),toolTip::noTip,core::none,[FilterCommand,SettingsDlg,LocalOptionsShow,Design,HelpAndAbout]),
        predefinedLayout_V:=[WorkSpaceSection, MoveRemoveSection,ProjectSection,performSourceSection_V,AboutCmdSection],
        convert(wsFE_Form,WS_Form):ribbonControl_ctl:layout := predefinedLayout_V.

predicates
    initFilterCommand : (window WS_Form).
clauses
    initFilterCommand(WS_Form):-
        showFilterCmd:=menuCommand::new(WS_Form, "showFilter"),
        showFilterCmd:menuLabel := ws_Events():getString(cmdFilter_C),
        showFilterCmd:ribbonLabel := ws_Events():getString(cmdFilter_C),
        showFilterCmd:tipTitle := toolTip::tip(ws_Events():getString(tipFilter_C)),
        showFilterCmd:style :=menuCommand::popupMenu,
        showFilterCmd:icon:=
                some(
                icon::createFromImages(
                    [bitmap::createFromBinary(showAll_C),
                    bitmap::createFromBinary(showAll_C)
                    ])),
        showFilterCmd:layout:=menuCommand::menuRender(getFilterList).

predicates
    getFilterList : function{menuCommand::menuItem*}.
clauses
    getFilterList() =
        convert(wsFE_Form,ws_Form_V):getSourceFilterList().

predicates
    about:(command).
clauses
    about(_):-
        wsFE_Tasks():about().
facts
    predefinedLayout_V : layout:=erroneous.

predicates
    onDesign : (command).
clauses
    onDesign(_) :-
        DesignerDlg = ribbonDesignerDlg::new(wsFE_Form()),
        DesignerDlg:cmdHost := wsFE_Form(),
        DesignerDlg:designLayout := wsFE_Form():ribbonControl_ctl:layout,
        DesignerDlg:predefinedSections := predefinedLayout_V,
        DesignerDlg:show(),
        if DesignerDlg:isOk() then
            wsFE_Form():ribbonControl_ctl:layout := DesignerDlg:designLayout
        end if.

predicates
    onSettingsDlg : (command).
clauses
    onSettingsDlg(_) :-
        notify(methodRequest,ws_EventManager::setupSettings_C, []).

clauses
    disabledRibbonPauseBlock():-
        performSourceHandling_V:disabledRibbonPauseBlock().

clauses
    setEnabledExecuteCmd(Value):-
        performSourceHandling_V:setEnabledExecuteCmd(Value).

clauses
    restoreRibbonState(ValueList):-
        foreach namedValue(Name, boolean(Value)) in ValueList do
            if Name = "reset" then
                performSourceHandling_V:restoreResetState(Value)
            elseif Name = "local" then
                localOptionsShowCmd:checked := Value
            end if
        end foreach.

end implement wSFE_Command
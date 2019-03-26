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

%    buildBlockMenu_V:ribbonControl::block.
%    buildAllBlockMenu_V:ribbonControl::block.
    pauseRunBlockMenu_V:ribbonControl::block.
    performSourceSection_V:ribbonControl::section.

    localOptionsShowCmd : command:=erroneous.

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

        sourceContentHandling_V:=sourceContentCommand::new(wsFE_P),
        AddEntityBlockMenu=sourceContentHandling_V:initWS_AddRemoveSource_Menu(WS_Form),

        performSourceHandling_V:=performSourceCommand::new(wsFE_P),

        PerformCmdBlock = performSourceHandling_V:initWS_PerformCommand_Menu(WS_Form),
        PerformCmdBlockRO = reOrderPerformBlock(PerformCmdBlock),

        ResetRunBlockMenu=performSourceHandling_V:initWS_PerformReset_Menu(WS_Form),
        pauseRunBlockMenu_V:=performSourceHandling_V:initWS_PerformPauseRun_Menu(WS_Form),
        performSourceHandling_V:addChangeListener(),

        AboutCmd = command::new(WS_Form,"about"),
        AboutCmd:menuLabel := ws_Events():getString(cmdAbout_C),
        AboutCmd:ribbonLabel := ws_Events():getString(cmdAbout_C),
        AboutCmd:tipTitle := toolTip::tip(ws_Events():getString(tipAbout_C)),
        AboutCmd:icon := some(icon::createFromImages(
            [bitmap::createFromBinary(aboutIcon16_C),
            bitmap::createFromBinary(aboutIcon32_C)
            ])),
        AboutCmd:run := about,
        AboutCmd:enabled := true,
        About = block([[cmd(AboutCmd:id, imageAndText(vertical))]]),

        HelpCmd = command::new(WS_Form,"help"),
        HelpCmd:menuLabel := ws_Events():getString(cmdHelp_C),
        HelpCmd:ribbonLabel := ws_Events():getString(cmdHelp_C),
        HelpCmd:tipTitle := toolTip::tip(ws_Events():getString(tipHelp_C)),
        HelpCmd:icon := some(icon::createFromImages(
            [bitmap::createFromBinary(helpIcon16_C),
            bitmap::createFromBinary(helpIcon32_C)
            ])),
        HelpCmd:run := help,
        HelpCmd:enabled := true,
        Help = block([[cmd(HelpCmd:id, imageAndText(vertical))]]),

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
        localOptionsShowCmd := command::new(WS_Form, "showlocaloptions"),
        localOptionsShowCmd:menuLabel := ws_Events():getString(cmdLocalOptions_C),
        localOptionsShowCmd:ribbonLabel := ws_Events():getString(cmdLocalOptions_C),
        localOptionsShowCmd:tipTitle := toolTip::tip(ws_Events():getString(tipLocalOptions_C)),
        localOptionsShowCmd:icon :=
            some(
                icon::createFromImages(
                    [bitmap::createFromBinary(showLocalOptions16_C),
                    bitmap::createFromBinary(showLocalOptions32_C)
                    ])),
%        localOptionsShowCmd:checked := true,
        localOptionsShowCmd:run := onLocalOptionsDlg,
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
            HelpCmd:ribbonLabel := ws_Events():getString(cmdHelp_C),
            HelpCmd:tipTitle := toolTip::tip(ws_Events():getString(tipHelp_C)),
            DesignCmd:ribbonLabel := ws_Events():getString(cmdDesign_C),
            DesignCmd:tipTitle := toolTip::tip(ws_Events():getString(tipDesign_C)),
            SettingsDlgCmd:ribbonLabel := ws_Events():getString(cmdOptions_C),
            SettingsDlgCmd:tipTitle := toolTip::tip(ws_Events():getString(tipOptions_C)),
            showFilterCmd:ribbonLabel := ws_Events():getString(cmdFilter_C),
            showFilterCmd:tipTitle := toolTip::tip(ws_Events():getString(tipFilter_C)),
            localOptionsShowCmd:ribbonLabel := ws_Events():getString(cmdLocalOptions_C),
            localOptionsShowCmd:tipTitle := toolTip::tip(ws_Events():getString(tipLocalOptions_C)),
            updateSectionLabels(convert(wsFE_Form,WS_Form))
            }),
        performSourceSection_V := section("actions",ws_Events():getString(sctSrcActions),toolTip::noTip,core::none,
                                                        list::append(PerformCmdBlockRO,[pauseRunBlockMenu_V, ResetRunBlockMenu])),
        OptionsSections = section("options",ws_Events():getString(sctOptions),toolTip::noTip,core::none,[FilterCommand,SettingsDlg,LocalOptionsShow,Design]),
        AboutCmdSection = section("about",ws_Events():getString(sctHelpAbout),toolTip::noTip,core::none,[Help,About]),
        predefinedLayout_V:=[WorkSpaceSection, MoveRemoveSection,ProjectSection,performSourceSection_V,OptionsSections,AboutCmdSection],
        convert(wsFE_Form,WS_Form):ribbonControl_P:layout := predefinedLayout_V.
%        convert(wsFE_Form,WS_Form):ribbonControl_ctl:colorBackgroundGradientFrom := ribbonCommand::vpi(vpiDomains::color_WhiteSmoke).

class predicates
    reOrderPerformBlock : (block* BlockList) -> block* BlockList.clauses
    reOrderPerformBlock([block([Cmd1]),block([Cmd2])|RestBlock]) = Result :-
        !,
        Result = [block([Cmd1, Cmd2]) | reOrderPerformBlock(RestBlock)].
    reOrderPerformBlock(Block) = Block.

predicates
    updateSectionLabels : (wsFE_Form WS_Form).
clauses
    updateSectionLabels(WS_Form):-
        foreach tuple(Index, StrIndex) in [tuple(0, sctWorkspace), tuple(1, sctManipulate), tuple(2, sctSrcEditing), tuple(3, sctSrcActions), tuple(4, sctOptions), tuple(5, sctHelpAbout)] do
            if section(section(Id, _, TipText, Icon, Blocks)) = WS_Form:ribbonControl_P:layoutTryGetElement(predefinedLayout_V, [Index]) then
                predefinedLayout_V:=WS_Form:ribbonControl_P:layoutUpdateElement(predefinedLayout_V, [Index], section(section(Id, ws_Events():getString(StrIndex), TipText, Icon, Blocks)))
            end if
        end foreach,
        WS_Form:ribbonControl_P:layout := predefinedLayout_V.

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

predicates
    help:(command).
clauses
    help(_):-
        wsFE_Tasks():help().

facts
    predefinedLayout_V : layout:=erroneous.

predicates
    onDesign : (command).
clauses
    onDesign(_) :-
        DesignerDlg = ribbonDesignerDlg::new(wsFE_Form()),
        DesignerDlg:cmdHost := wsFE_Form(),
        DesignerDlg:designLayout := wsFE_Form():ribbonControl_P:layout,
        DesignerDlg:predefinedSections := predefinedLayout_V,
        DesignerDlg:show(),
        if DesignerDlg:isOk() then
            wsFE_Form():ribbonControl_P:layout := DesignerDlg:designLayout,
            wsFE_Form():wsFE_Command_P:predefinedLayout_V := wsFE_Form():ribbonControl_P:layout
        end if.

predicates
    onSettingsDlg : (command).
clauses
    onSettingsDlg(_) :-
        notify(methodRequest,ws_EventManager::setupSettings_C, []).

predicates
    onLocalOptionsDlg : (command).
clauses
    onLocalOptionsDlg(_) :-
        wsFE_Form():showLocalOptionsDialog().

clauses
    disabledRibbonPauseBlock():-
        performSourceHandling_V:disabledRibbonPauseBlock().

clauses
    setEnabledExecuteCmd(Value):-
        performSourceHandling_V:setEnabledExecuteCmd(Value).

clauses
    updateCommandRibbon(Index, CmdName, Enabled):-
        performSourceHandling_V:updateCommandRibbon(Index, CmdName, Enabled).
%        wsFE_Form():ribbonControl_ctl:invalidate().

clauses
    restoreRibbonState(ValueList):-
        foreach namedValue(Name, boolean(Value)) in ValueList do
            if Name = "reset" then
                performSourceHandling_V:restoreResetState(Value)
%            elseif Name = "local" then
%                localOptionsShowCmd:checked := Value
            end if
        end foreach.

end implement wSFE_Command
%

class wSFE_Command : wSFE_Command
    open core

constants
    newWSIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\WsNew_16.png").
    newWSIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\WsNew_32.png").
    openWSIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\WsOpen_16.png").
    openWSIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\WsOpen_32.png").

    addGroupIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddGroup_16.png").
    addGroupIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddGroup_32.png").

    addSubGroupIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSubGroup_16.png").
    addSubGroupIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSubGroup_32.png").

    addFolderIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddFolder_16.png").
    addFolderIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddFolder_32.png").

    addSubFolderIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSubFolder_16.png").
    addSubFolderIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSubFolder_32.png").

    addSourceIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSource_16.png").
    addSourceIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSource_32.png").

    addSourcesFromFolderIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSourceFromFolder_16.png").
    addSourcesFromFolderIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\AddSourceFromFolder_32.png").

    checkFilesIcon_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\CheckFiles.png").

    runIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Run.png").
    runIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Run.png").
    runAllIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\RunAll.png").
    runAllIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\RunAll.png").
    contRunIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Run_16.png").
    contRunIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Run_32.png").
    pauseIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Pause_16.png").
    pauseIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Pause_32.png").
    stopIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Stop_16.png").
    stopIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Stop_32.png").
    extOptions_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\gear_2-512.png").
    showLocalOptions16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\localOptions.png").
    showLocalOptions32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\localOptions.png").
    showAll_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\icons8-filter-50.png").

    removeIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Remove.png").
    removeIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Remove.png").
    moveUpIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ArrowUp_16.png").
    moveUpIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ArrowUp_32.png").
    moveDownIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ArrowDown_16.png").
    moveDownIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ArrowDown_32.png").
    editIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Edit_16.png").
    editIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Edit_32.png").

    resetIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Reset_16.png").
    resetIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Reset_32.png").
    selResetIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ResetS_16.png").
    selResetIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\ResetS_32.png").

    helpIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Question.png").
    helpIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Question.png").

    aboutIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Info.png").
    aboutIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\Info.png").

    designIcon16_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\if_tools-70px_510859.png").
    designIcon32_C:binary=#bininclude(@"WS_manager\Common\AppFrontEnd\FE_Icons\if_tools-70px_510859.png").


constructors
    new : (window WS_Form,ws_FrontEnd FrontEnd).

end class wSFE_Command
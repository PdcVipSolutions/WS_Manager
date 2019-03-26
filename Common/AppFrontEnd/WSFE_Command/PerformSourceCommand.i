%

interface performSourceCommand
    supports commandBlock
    open core

predicates
    initWS_PerformReset_Menu:(window Win)->ribbonControl::block.
    initWS_PerformPauseRun_Menu:(window Win)->ribbonControl::block.
    addChangeListener:().

predicates
    initWS_PerformCommand_Menu:(window Win) -> ribbonControl::block*.

predicates
    disabledRibbonPauseBlock : ().

predicates
    setEnabledExecuteCmd : (string Value).

predicates
    updateCommandRibbon : (integer Index,string CmdName,boolean Enabled).

predicates
    restoreResetState : (boolean IsSelect).

end interface performSourceCommand
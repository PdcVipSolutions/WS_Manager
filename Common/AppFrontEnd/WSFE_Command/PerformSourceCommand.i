%

interface performSourceCommand
    supports commandBlock
    open core

predicates
    initWS_PerformSource_Menu:(window Win)->ribbonControl::block.
    initWS_PerformAllSource_Menu:(window Win)->ribbonControl::block.
    initWS_PerformReset_Menu:(window Win)->ribbonControl::block.
    initWS_PerformPauseRun_Menu:(window Win)->ribbonControl::block.
%    initWS_PerformSettings_Menu:(window Win)->ribbonControl::block.
    addChangeListener:().

predicates
    disabledRibbonPauseBlock : ().

predicates
    setEnabledExecuteCmd : (string Value).

predicates
    restoreResetState : (boolean IsSelect).

end interface performSourceCommand
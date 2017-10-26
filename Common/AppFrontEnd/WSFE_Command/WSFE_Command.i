%

interface wSFE_Command

predicates
    disabledRibbonPauseBlock:().

predicates
    setEnabledExecuteCmd : (string EnabledStr).

predicates
    restoreRibbonState : (core::namedValue_list).

end interface wSFE_Command
%

interface wSFE_Command

predicates
    disabledRibbonPauseBlock:().

predicates
    setEnabledExecuteCmd : (string EnabledStr).

predicates
    updateCommandRibbon : (integer Index,string CmdName,boolean Enabled).

predicates
    restoreRibbonState : (core::namedValue_list).

properties
    predefinedLayout_V : ribbonControl::layout.

end interface wSFE_Command
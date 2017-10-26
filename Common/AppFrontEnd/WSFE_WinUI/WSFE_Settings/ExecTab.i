%

interface execTab supports control
    open core

properties
    editCmdString_P : editControl (o).
    editArgString_P : editControl (o).
    resultCommandLine_P : textControl (o).
    getMacroSymbols_P : button (o).
    cbExecutePossible_P : checkButton (o).
    txtCommandLine_P : textControl (o).
    gbResultStr_P : groupBox (o).
    txtArguments_P : textControl (o).
    txtFormatCommand_P : textControl (o).
    cbFEMode_P : checkButton (o).

end interface execTab
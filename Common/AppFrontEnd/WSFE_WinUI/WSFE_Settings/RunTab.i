%

interface runTab supports control
    open core

properties
    editorFileName_P : editControl (o).
    browseEditor_P : button (o).
    editSuffix_P : editControl (o).
    resultCommandLine_P : textControl (o).
    editArgStringForRun_P : editControl (o).
    editArgStringForReRun_P : editControl (o).
    lbSteramMode_P : listButton (o).
    lbCodePage_P : listButton (o).
    browseSuffix_P : button (o).
    txtFormatCommand_P : textControl (o).
    txtRunFile_P : textControl (o).
    txtSuffix_P : textControl (o).
    gbResultStr_P : groupBox (o).
    gbArguments_P : groupBox (o).
    txtRunMode_P : textControl (o).
    txtReRunMode_P : textControl (o).
    txtStreamMode_P : textControl (o).
    txtCodePage_P : textControl (o).
    cbFEMode_P : checkButton (o).

end interface runTab
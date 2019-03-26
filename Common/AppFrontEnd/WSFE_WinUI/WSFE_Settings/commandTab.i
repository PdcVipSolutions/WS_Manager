%

interface commandTab supports control
    open core

properties
    txtArguments_P : textControl (o).
    editArguments_P : editControl (o).
    cbCallAssociations_P : checkButton (o).
    txtFormatCommand_P : textControl (o).
    editFormatCommand_P : editControl (o).
    txtApplicationFile_P : textControl (o).
    applicationFileName_P : editControl (o).
    browseEditor_P : button (o).
    editSuffix_P : editControl (o).
    browseSuffix_P : button (o).
    txtSuffix_P : textControl (o).
    cbFEMode_P : checkButton (o).
    cbDefCommand_P : checkButton (o).
    cbCheckStatus_P : checkButton (o).
    lbCodePage_P : listButton (o).
    cbStreamMode_P : checkButton (o).
    txtCodePage_P : textControl (o).
    getMacroSymbols_P : button (o).
    resultCommandLine_P : editControl (o).
    gbResultStr_P : groupBox (o).
    cbPossibleAll_P : checkButton (o).
    stInputStream_P : textControl (o).
    edInputStreamFile_P : editControl (o).
    browseInputFile_P : button (o).
    gbInputStream_P : groupBox (o).
    lbInputEncoding_P : listButton (o).
    stInputEncoding_P : textControl (o).
    gbOutputStream_P : groupBox (o).
    pbSetWords_P : button (o).

    codePageList_P : namedValue*.

predicates
    setControlsID : ().

predicates
    addControlsText : (namedValue* TitleDialogList).

predicates
    setStreamModeValues : ().

predicates
    updateResultString : ().

end interface commandTab
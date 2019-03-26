%

class commandTab : commandTab
    open core

constructors
    new : ().

constructors
    new : (containerWindow Parent).

constants
    editorFileName_id = 1000.
    browseEditor_id = editorFileName_id + 1.
    editArgString_id = browseEditor_id + 1.
    runFileName_id = editArgString_id + 1.
    browseRun_id = runFileName_id + 1.
    editSuffix_id = browseRun_id + 1.
    browseSuffix_id = editSuffix_id + 1.
    editFormatCommand_id = browseSuffix_id + 1.
    editCommandName_id = editFormatCommand_id + 1.
    execCmdString_id = editCommandName_id + 1.
    execArgString_id = execCmdString_id + 1.
    editComponentName_id = execArgString_id + 1.
    lbSteramMode_id = editComponentName_id + 1.
    lbCodePage_id = lbSteramMode_id + 1.
    getMacroSymbols_id = lbCodePage_id + 1.
    performOpen_id = getMacroSymbols_id + 1.
    cbCallAssociations_id = performOpen_id + 1.
    defCommand_id = cbCallAssociations_id + 1.
    cbStreamModeOn_id = defCommand_id + 1.
    edInputStreamFile_id = cbStreamModeOn_id + 1.
    browseInputFile_id = edInputStreamFile_id + 1.
    lbInputEncoding_id = browseInputFile_id + 1.
    cbPossibleAll_id = lbInputEncoding_id + 1.

end class commandTab
%

class performExtCtl : performExtCtl
    open core

constants
    editorFileName_id = 1000.
    browseEditor_id = editorFileName_id + 1.
    editArgString_id = browseEditor_id + 1.
    runFileName_id = editArgString_id + 1.
    browseRun_id = runFileName_id + 1.
    editSuffix_id = browseRun_id + 1.
    browseSuffix_id = editSuffix_id + 1.
    editArgStringForRun_id = browseSuffix_id + 1.
    editArgStringForReRun_id = editArgStringForRun_id + 1.
    execCmdString_id = editArgStringForReRun_id + 1.
    execArgString_id = execCmdString_id + 1.
    editComponentName_id = execArgString_id + 1.
    lbSteramMode_id = editComponentName_id + 1.
    lbCodePage_id = lbSteramMode_id + 1.
    getMacroSymbols_id = lbCodePage_id + 1.
    cbExecutePossible_id = getMacroSymbols_id + 1.
    performOpen_id = cbExecutePossible_id + 1.
    performRun_id = performOpen_id + 1.
    performExec_id = performRun_id + 1.

constructors
    new : (wsfe_Settings,containerWindow Parent,string SelectSourceType,namedValue* ExtOptionsList).

end class performExtCtl
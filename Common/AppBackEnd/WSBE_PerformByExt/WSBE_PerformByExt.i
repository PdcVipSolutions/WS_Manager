%

interface wSBE_PerformByExt
    open core

properties
    useExe_P:useExe.

    sourceObj_P:xmlElement.

    input_P:inputStream.
    currentBinDir_P:string.

    openEnabled_P : boolean.
    runEnabled_P : boolean.
    execEnabled_P : boolean.

    streamModeIndex_P:positive.
    codePage_P:codePage.

    performOpen_P:boolean.
    performRun_P:boolean.
    performExec_P:boolean.

properties
    sourcePerformer_P:string.
    sourceEditor_P:string.

predicates
    startRun:(string RunMode) -> string FullCommandLine.

predicates
    stopRun:().

predicates
    invokeOpenSource:() -> string FullCommandLine.

predicates
    executeSource:() -> string FullCommandLine.

predicates
    tryParseError : (string StringInLowerCase)->tuple{symbol ErrorOrWarning,wsBE_Perform::errorNumber ErrorNumber, string FileName} determ.

end interface wSBE_PerformByExt
%

interface wsBE_Performer
    open core

properties
    stopRunFlag_P:boolean.

    runDone_V:integer.
    runFailed_V:integer.
    runNoPath_V:integer.
    runNoRule_V:integer.
    runTotalCount_V:integer.

    wsBE_Options_P : wsBE_Options (o).
    wsBE_XmlDocument_P : xmlDocument (o).

predicates
    runSource : (xmlElement SourceObj,string RunMode, string FullFileName,object TaskQueueObj).
    stopRun:(object TaskQueueObj).
    execSource : (xmlElement SourceObj, string FullFileName,object TaskQueueObj).

predicates
    openSource:(xmlElement SourceObj, string SourceFileName,object TaskQueueObj).

predicates
    handleStreamFrontEnd:(boolean EndRunSource,string Line,object TaskQueueObj).

end interface wsBE_Performer
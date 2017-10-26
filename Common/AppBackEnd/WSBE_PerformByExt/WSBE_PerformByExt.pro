%

implement wSBE_PerformByExt
    open core, ws_EventManager, xmlNavigate, pfc\log

facts
    useExe_P:useExe:=erroneous.
    input_P:inputStream:=erroneous.
    source_P:string.
    sourceObj_P:xmlElement.
    currentBinDir_P:string := directory::getCurrentDirectory().
    parent_P : wsBE_Performer.

    openEnabled_P : boolean := false.
    runEnabled_P : boolean := false.
    execEnabled_P : boolean := false.

clauses
    new(Parent, FullSourceFileName, SourceObj):-
        source_P := FullSourceFileName,
        parent_P := Parent,
        sourceObj_P := SourceObj,
        initializeByExt(Parent, string::toLowerCase(filename::getExtension(FullSourceFileName))),
        createSupportComponent().

predicates
    createSupportComponent : ().
clauses
    createSupportComponent():-
        if "" = componentName_P then
        elseif string::equalIgnoreCase("wsBE_PerformVIPPRJ", componentName_P) then
            componentObject_P := wsBE_PerformVIPPRJ::new()
        elseif string::equalIgnoreCase("wsBE_PerformCMD", componentName_P) then
            componentObject_P := wsBE_PerformCMD::new()
        end if.

facts
    componentName_P:string:="".
    componentObject_P:wsBE_Perform := erroneous.

    sourcePerformer_P:string:="".
    argPerformer_P:string:="".
    reargPerformer_P:string:="".
    suffixPerformer_P:string:="".
    streamModeIndex_P:positive:=0.
    codePage_P:codePage:=utf8.

    sourceEditor_P:string:="".
    argEditor_P:string:="".

    performOpen_P:boolean := false.
    performRun_P:boolean := false.
    performExec_P:boolean := false.

    cmdExec_P:string:="".
    argExec_P:string:="".

    macroSymbols : (string MacroName, string Value).

predicates
    initializeByExt : (wsBE_Performer Parent,string Ext).
clauses
    initializeByExt(Parent, Ext):-
        OptionsValues = Parent:wsBE_Options_P:loadOptionsByExt(Ext),
        foreach tuple(NodeName, AttributesList) in OptionsValues do
            if NodeName = sourceEditor_C then
                sourceEditor_P := if E = namedValue::tryGetNamed_string(AttributesList, fileName_C) then Parent:wsBE_Options_P:getFullFileName(E) else "" end if,
                if file::existExactFile(sourceEditor_P) then
                    openEnabled_P := true
                end if,
                argEditor_P := if AE = namedValue::tryGetNamed_string(AttributesList, argEditor_C) then AE else "" end if,
                performOpen_P := toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, feMode_C))
            elseif NodeName = sourcePerformer_C then
                componentName_P := if C = namedValue::tryGetNamed_string(AttributesList, componentName_C) then C else "" end if,
                sourcePerformer_P := if S = namedValue::tryGetNamed_string(AttributesList, fileName_C) then Parent:wsBE_Options_P:getFullFileName(S) else "" end if,
                if file::existExactFile(sourcePerformer_P) then
                    runEnabled_P := true
                end if,
                argPerformer_P := if A = namedValue::tryGetNamed_string(AttributesList, argPerformer_C) then A else "" end if,
                reArgPerformer_P := if RA = namedValue::tryGetNamed_string(AttributesList, reargPerformer_C) then RA else "" end if,
                suffixPerformer_P := if SP = namedValue::tryGetNamed_string(AttributesList, suffixPerformer_C)
                                                then changeVirtualDir(Parent:wsBE_Options_P, SP)
                                                else "" end if,
                streamModeIndex_P := if SMI = toTerm(positive, namedValue::tryGetNamed_string(AttributesList, streamMode_C)) then SMI else 0 end if,
                codePage_P := if CP = namedValue::tryGetNamed_string(AttributesList, codePage_C) then toTerm(codePage, CP) else utf8 end if,
                performRun_P := toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, feMode_C))
            elseif NodeName = sourceExecute_C then
                cmdExec_P := if CEx = namedValue::tryGetNamed_string(AttributesList, cmdExecute_C) then CEx else "" end if,
                argExec_P := if AEx = namedValue::tryGetNamed_string(AttributesList, argExecute_C) then AEx else "" end if,
                performExec_P := toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, feMode_C))
            end if
        end foreach.

predicates
    changeVirtualDir : (wsBE_Options, string InputStr) -> string.
clauses
    changeVirtualDir(WSBE_Options, InputStr) = string::concat(HeadStr, ShortDir, OutStr) :-
        string::splitStringBySeparators(InputStr, "$", HeadStr, _, RestStr),
        string::splitStringBySeparators(RestStr, ")", VirtName, _, Rest),
        ShortDir = fileSystem_api::getShortPathName(WSBE_Options:getFullFileName(string::concat("$", VirtName, ")"))),
        !,
        OutStr = changeVirtualDir(WSBE_Options, Rest).
    changeVirtualDir(_, Rest) = Rest.

clauses
    invokeOpenSource() = string::format(@["%s" %s "%s"],sourceEditor_P,FullArgumentLine,source_P) :-
        Path=fileName::getPath(source_P),
        directory::setCurrentDirectory(Path),
        if
            FEArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(argEditor_C, { (_) })]),
            FEMode = FEArgNode:attribute(feMode_C)
        then
            performOpen_P := toBoolean("true" = FEMode)
        end if,
        if
            EditArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(argEditor_C, { (_) })]),
            AddMode = EditArgNode:attribute(addMode_C),
            LocalValue = EditArgNode:attribute(value_C)
        then
            FullArgumentLine = if AddMode = "true" then string::concat(argEditor_P, " ", LocalValue) else LocalValue end if
        else
            FullArgumentLine = argEditor_P
        end if.

%        useExe_P:=useExe::new(string::format(@["%s" %s "%s"],sourceEditor_P,FullArgumentLine,source_P)),
%        useExe_P:run().

clauses
    executeSource() = CommandLine :-
        Path=fileName::getPath(source_P),
        directory::setCurrentDirectory(Path),
        if
            FEArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(argExecute_C, { (_) })]),
            FEMode = FEArgNode:attribute(feMode_C)
        then
            performExec_P := toBoolean("true" = FEMode)
        end if,
        if
            ExecArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(argExecute_C, { (_) })]),
            AddMode = ExecArgNode:attribute(addMode_C),
            LocalValue = ExecArgNode:attribute(value_C)
        then
            FullArgumentLine = if AddMode = "true" then string::concat(argExec_P, " ", LocalValue) else LocalValue end if
        else
            FullArgumentLine = argExec_P
        end if,
        if isErroneous(componentObject_P) then
            tuple(CommandLine, ExeDir) = getExecuteCommandLine(parent_P, source_P, cmdExec_P, FullArgumentLine)
        else
            tuple(CommandLine, ExeDir) = componentObject_P:getExecuteCommandLine(parent_P, source_P, cmdExec_P, FullArgumentLine)
        end if,
        if ExeDir <> "" then
            directory::setCurrentDirectory(ExeDir)
        end if.
%        useExe_P:=useExe::new(CommandLine),
%        useExe_P:run().

predicates
    getExecuteCommandLine : (wsBE_Performer Parent, string SourceFile, string CommandLine, string ArgumentStr) -> tuple{string,string}.
clauses
    getExecuteCommandLine(Parent, SourceFile, CommandLine, ArgumentStr) = tuple(FullCommandLine, FullExeDir) :-
        retractAll(macroSymbols(_, _)),
        assert(macroSymbols("$(SourceFile)", SourceFile)),
        assert(macroSymbols("$(SourceName)", filename::getNameWithExtension(SourceFile))),
        FullCommandLine = parseCommandLine(Parent:wsBE_Options_P, string::concat(CommandLine, " ", ArgumentStr)),
        if macroSymbols("$(SourceExeDir)", FullExeDir) then
        else
            FullExeDir = ""
        end if.

predicates
    parseCommandLine : (wsBE_Options, string InputStr) -> string.
clauses
    parseCommandLine(WSBE_Options, InputStr) = string::concat(HeadStr, ParseStr, OutStr) :-
        string::splitStringBySeparators(InputStr, "$", HeadStr, _, RestStr),
        string::splitStringBySeparators(RestStr, ")", VirtName, _, Rest),
        ParseStr = changeMacroSymbol(WSBE_Options, string::concat("$", VirtName, ")")),
        !,
        OutStr = parseCommandLine(WSBE_Options, Rest).
    parseCommandLine(_, Rest) = Rest.

predicates
    changeMacroSymbol : (wsBE_Options, string MacroSymbol) -> string ParseStr.
clauses
    changeMacroSymbol(_WSBE_Options, MacroName) = MacroValue :-
        macroSymbols(MacroName, MacroValue),
        !.
    changeMacroSymbol(_WSBE_Options, "$(SourceExeDir)") = FullExeDir :-
        macroSymbols("$(SourceFile)", SourceFile),
        FullExeDir = filename::getPath(SourceFile),
        assert(macroSymbols("$(SourceExeDir)", FullExeDir)),
        !.
    changeMacroSymbol(WSBE_Options, MacroSymbol) = ParseStr :-
        ParseStr = WSBE_Options:getFullFileName(MacroSymbol),
        not(string::equalIgnoreCase(ParseStr, MacroSymbol)),
        !.
    changeMacroSymbol(_WSBE_Options, String) = String.

clauses
    tryParseError(Line)=
        if isErroneous(componentObject_P) then
            tuple("", 0, "")
        else
            componentObject_P:tryParseError(Line)
        end if.

clauses
    startRun(RunMode) = string::format(@["%s" %  "%s" %s],sourcePerformer_P,FullArgumentLine,source_P,SuffixStr) :-
        Path=fileName::getPath(source_P),
        if run_C=RunMode then
            ArgumentsLine = argPerformer_P,
            LocalName = argPerformer_C
        else
            ArgumentsLine = reargPerformer_P,
            LocalName = reargPerformer_C
        end if,
        if
            RunArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(LocalName, { (_) })]),
            AddMode = RunArgNode:attribute(addMode_C),
            LocalValue = RunArgNode:attribute(value_C)
        then
            FullArgumentLine = if AddMode = "true" then string::concat(ArgumentsLine, " ", LocalValue) else LocalValue end if
        else
            FullArgumentLine = ArgumentsLine
        end if,
        if
            FEArgNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(LocalName, { (_) })]),
            FEMode = FEArgNode:attribute(feMode_C)
        then
            performRun_P := toBoolean("true" = FEMode)
        end if,
        if
            SuffixNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(suffixPerformer_C, { (_) })]),
            AddSMode = SuffixNode:attribute(addMode_C),
            LocalSValue = SuffixNode:attribute(value_C)
        then
            SuffixStr = if AddSMode = "true" then string::concat(suffixPerformer_P, " ", LocalSValue) else LocalSValue end if
        else
            SuffixStr = suffixPerformer_P
        end if,
        directory::setCurrentDirectory(Path).
%        useExe_P:=useExe::new(string::format(@["%s" %  "%s" %s],sourcePerformer_P,FullArgumentLine,source_P,SuffixStr)),
%        OutputStreamMode = if streamModeIndex_P = 0 then stream::unicode elseif streamModeIndex_P = 1 then stream::ansi(codePage_P) else stream::binary end if,
%        input_P:=useExe_P:getFromProcessStream(OutputStreamMode).

clauses
    stopRun():-
        try
            useExe_P:terminate(777)
        catch _TraceId do
            succeed()
        end try.

end implement wSBE_PerformByExt
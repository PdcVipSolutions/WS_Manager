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
%    feMode_P : string := "".

clauses
    new(Parent, BuildType, FullSourceFileName, SourceObj):-
%        feMode_P := FEMode,
        source_P := FullSourceFileName,
        parent_P := Parent,
        sourceObj_P := SourceObj,
        initializeByExt(Parent, BuildType, string::toLowerCase(filename::getExtension(FullSourceFileName))),
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

    winAssExecute_P : boolean := false.
    formatCmd_P : string := "".
    sourcePerformer_P : string := "".
    argPerformer_P : string := "".
    suffixPerformer_P : string := "".
    streamModeOn_P : boolean := false.
    streamModeIndex_P : positive := 0.
    codePage_P : codePage := utf8.
    checkStatus_P : boolean := false.
    inputStreamFile_P : string := "".

    sourceEditor_P:string := "".
    argEditor_P:string := "".

    performOpen_P:boolean := false.
    performRun_P:boolean := false.
    performExec_P:boolean := false.

    cmdExec_P:string := "".
    argExec_P:string := "".

    keyWords_P : namedValue* := [].

    macroSymbols : (string MacroName, string Value).

predicates
    initializeByExt : (wsBE_Performer Parent,string BuildType, string Ext).
clauses
    initializeByExt(Parent, BuildType, Ext):-
        OptionsValues = Parent:wsBE_Options_P:loadOptionsByExt(Ext),
        if
            tuple("common", СAttributesList) in OptionsValues,
            componentName_P := namedValue::tryGetNamed_string(СAttributesList, componentName_C)
        then end if,
        if
            tuple(command_C, AttributesList) in OptionsValues,
            BuildType = namedValue::tryGetNamed_string(AttributesList, index_C)
        then
            if "true" = namedValue::tryGetNamed_string(AttributesList, winAss_C) then
                winAssExecute_P := true,
                runEnabled_P := true
            end if,
            formatCmd_P := if F = namedValue::tryGetNamed_string(AttributesList, formatCmd_C) then F else "" end if,
            if S = namedValue::tryGetNamed_string(AttributesList, fileName_C) then
                FullS = Parent:wsBE_Options_P:getFullFileName(S),
                sourcePerformer_P := if file::existExactFile(FullS) then FullS else S end if
            else sourcePerformer_P := "" end if,
            if file::existExactFile(sourcePerformer_P) then
                runEnabled_P := true
            end if,
            argPerformer_P := if A = namedValue::tryGetNamed_string(AttributesList, argument_C)
                                            then changeVirtualDir(Parent:wsBE_Options_P, A)
                                            else "" end if,
            suffixPerformer_P := if SP = namedValue::tryGetNamed_string(AttributesList, suffix_C)
                                            then changeVirtualDir(Parent:wsBE_Options_P, SP)
                                            else "" end if,
            streamModeOn_P  := toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, streamModeOn_C)),
            streamModeIndex_P := if SMI = toTerm(positive, namedValue::tryGetNamed_string(AttributesList, streamMode_C)) then SMI else 0 end if,
            codePage_P := if CP = namedValue::tryGetNamed_string(AttributesList, codePage_C) then toTerm(codePage, CP) else utf8 end if,
            checkStatus_P := toBoolean("true" = namedValue::tryGetNamed_string(AttributesList, checkStatus_C)),
            inputStreamFile_P := if ISF = namedValue::tryGetNamed_string(AttributesList, inputFile_C)
                                            then changeVirtualDir(Parent:wsBE_Options_P, ISF)
                                            else "" end if,
            keyWords_P := toTerm(namedValue::tryGetNamed_string(AttributesList, keyWords_C)) otherwise []
        end if.

class predicates
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
            tuple(CommandLine, _FullApplicationFile, ExeDir) = getExecuteCommandLine(parent_P, source_P, "", cmdExec_P, FullArgumentLine)
        else
            tuple(CommandLine, _FullApplicationFile, ExeDir) = componentObject_P:getExecuteCommandLine(parent_P, source_P, "", cmdExec_P, FullArgumentLine)
        end if,
        if ExeDir <> "" then
            directory::setCurrentDirectory(ExeDir)
        end if.

predicates
    getExecuteCommandLine : (wsBE_Performer Parent, string SourceFile, string ApplicationFile, string CommandLine, string ArgumentStr) -> tuple{string,string,string}.
clauses
    getExecuteCommandLine(Parent, SourceFile, ApplicationFile, CommandLine, ArgumentStr) = tuple(FullCommandLine, FullApplicationFile, FullExeDir) :-
        retractAll(macroSymbols(_, _)),
        assert(macroSymbols("$(SourceFile)", SourceFile)),
        assert(macroSymbols("$(SourceName)", filename::getNameWithExtension(SourceFile))),
        FullCommandLine = parseCommandLine(Parent:wsBE_Options_P, string::concat(CommandLine, " ", ArgumentStr)),
        FullApplicationFile = parseCommandLine(Parent:wsBE_Options_P, ApplicationFile),
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
    tryParseError(Line) =
        if  ComponentObject = notErroneous(componentObject_P) then
            ComponentObject:tryParseError(Line, keyWords_P)
        elseif [] <> keyWords_P then
            tryDefaultParseError(Line, keyWords_P)
        else
            tuple("", 0, "")
        end if.

predicates
    tryDefaultParseError : (string Line,namedValue* KeyWords) -> tuple{symbol ErrorOrWarning,wsBE_Perform::errorNumber ErrorNumber, string FileName} determ.
clauses
    tryDefaultParseError(Line, KeyWords) = Result :-
        namedValue(KeyName, string(Words)) in KeyWords,
            ErrorList = toTerm(Words),
            Error in ErrorList,
        _Pos = string::search(Line, Error, string::caseInsensitive),
        !,
        if KeyName = "Warning" then
            Result = tuple(wsBE_Perform::performWarning_C, 1, "")
        else
            Result = tuple(wsBE_Perform::performError_C, 1, "")
        end if.

clauses
    startRun(BuildType) = Result :-
        if winAssExecute_P = true then
            Result = source_P
        else
            if BuildNode = parent_P:wsBE_XmlDocument_P:getNode_nd([current(sourceObj_P), child(commandL_C, { (C) :- C:attribute(index_C) = BuildType})]) then
                FC = BuildNode:attribute(formatCmd_C) otherwise formatCmd_P,
                Application = BuildNode:attribute(fileName_C) otherwise sourcePerformer_P,
                ArgumentsLine = BuildNode:attribute(argument_C) otherwise argPerformer_P,
                SuffixStr = BuildNode:attribute(suffix_C) otherwise suffixPerformer_P,
                StreamModeOn = toBoolean("true" = BuildNode:attribute(streamModeOn_C) otherwise toString(streamModeOn_P)),
                inputStreamFile_P := if ISF = BuildNode:attribute(inputFile_C) then changeVirtualDir(parent_P:wsBE_Options_P, ISF) else inputStreamFile_P end if,
                keyWords_P := toTerm(BuildNode:attribute(keyWords_C)) otherwise keyWords_P
            else
                FC = formatCmd_P,
                Application = sourcePerformer_P,
                ArgumentsLine = argPerformer_P,
                SuffixStr = suffixPerformer_P,
                StreamModeOn = streamModeOn_P
            end if,
            FC1 = string::replaceAll(FC, "[Application]", string::concat(@{"},Application,@{"})),
            FC2 = string::replaceAll(FC1, "[Arguments]", ArgumentsLine),
            FC3 = string::replaceAll(FC2, "[Suffix]", SuffixStr),
            Path = fileName::getPath(source_P),
            directory::setCurrentDirectory(Path),
            if isErroneous(componentObject_P) then
                tuple(CommandLine, ApplicationFile, ExeDir) = getExecuteCommandLine(parent_P, source_P, Application, FC3, "")
            else
                tuple(CommandLine, ApplicationFile, ExeDir) = componentObject_P:getExecuteCommandLine(parent_P, source_P, Application, FC3, "")
            end if,
            streamModeOn_P  :=
                if file::existExactFile(ApplicationFile) and StreamModeOn = true then
                    exe_api::isOSConsoleApplication(ApplicationFile)
                else
                    false
                end if,
            if ExeDir <> "" then
                directory::setCurrentDirectory(ExeDir)
            end if,
            Result = CommandLine
        end if.

clauses
    stopRun():-
        try
            useExe_P:terminate(777)
        catch _TraceId do
            succeed()
        end try.

end implement wSBE_PerformByExt
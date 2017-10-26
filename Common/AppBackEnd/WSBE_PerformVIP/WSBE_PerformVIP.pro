%
implement wsBE_PerformVIPPRJ
    open core, ws_eventManager

class facts
    vipPath_V:string:=getVipPath().

facts
    macroSymbols : (string MacroName, string Value).

class predicates
    getVipPath:()->string VipPath.
clauses
    getVipPath()=VipPath:-
        string(VipPath)=registry::tryGetValue(registry::localMachine,@"SOFTWARE\Prolog Development Center\Visual Prolog","VipPath"),
        !.
    getVipPath()=_:-
        exception::raise_User("Vip is not installed or it is installed incorreectly").

constants
    error_C : symbol = "error".
    warning_C : symbol = "warning".
    fatalError_C : symbol = "fatal error".
    registerCompError_C : symbol = "RegistryError".
    callFailed_C : symbol ="call failed".

clauses
    tryParseError(Line)=tuple(ParseResult, ErrorNumber, FileName) :-
        ErrorOrWarning in [error_C, fatalError_C, warning_C,registerCompError_C,callFailed_C],
            Pos = string::search(Line, ErrorOrWarning),
            string::front(Line, Pos, FileName, Rest1),
            string::hasPrefix(Rest1, ErrorOrWarning, RestString),
            string::fronttoken(RestString, Token, _Message),
            string::frontChar(Token, First, Last),
            Token2 =
                if First = 'c' then
                    Last
                else
                    Token
                end if,
            ErrorNumber = tryToTerm(Token2),
            if ErrorOrWarning=warning_C then ParseResult=wSBE_Perform::performWarning_C
            else ParseResult=wSBE_Perform::performError_C end if,
        !.
    tryParseError(StringInLowerCase)=tuple(wsBE_Perform::performError_C,ErrorNumber,""):-
        string::hasPrefix(StringInLowerCase, "error ", RestString),
        string::fronttoken(RestString, Token, _),
        string::hasDecimalDigits(Token),
        ErrorNumber = tryToTerm(Token),
        !.
    tryParseError(StringInLowerCase)=tuple(wsBE_Perform::performError_C, 1,"") :-
            _Pos = string::search(StringInLowerCase, callFailed_C).

clauses
    getExecuteCommandLine(Parent, SourceFile, CommandLine, ArgumentStr) = tuple(string::concat(FullCommandLine, " ", ArgumentStr), FullExeDir) :-
        assert(macroSymbols("$(SourceFile)", SourceFile)),
        assert(macroSymbols("$(SourceName)", filename::getNameWithExtension(SourceFile))),
        assert(macroSymbols("$(ExeName)", fileName::setExtension(filename::getName(SourceFile),"exe"))),
        FullCommandLine = parseCommandLine(Parent:wsBE_Options_P, CommandLine),
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
    changeMacroSymbol(WSBE_Options, "$(SourceExeDir)") = FullExeDir :-
        macroSymbols("$(SourceFile)", SourceFile),
        VipProjectXML=xmlDocument::new("vipprj"),
        VipProjectXML:codePage_P:=utf8,
        VipProjectXML:indent_P:=true,
        VipProjectXML:xmlStandalone_P:=xmlLite::yes,
        VipFile = inputStream_file::openFile(SourceFile, stream::binary),
        spbXmlLigntSupport::read(VipFile, VipProjectXML),
        VipFile:close(),
        ShortDir = VipProjectXML:root_P:attribute("final_directory"),
        FullExeDir = WSBE_Options:getFullFileName(ShortDir),
        assert(macroSymbols("$(SourceExeDir)", FullExeDir)),
        !.
    changeMacroSymbol(WSBE_Options, MacroSymbol) = ParseStr :-
        ParseStr = WSBE_Options:getFullFileName(MacroSymbol),
        not(string::equalIgnoreCase(ParseStr, MacroSymbol)),
        !.
    changeMacroSymbol(_WSBE_Options, String) = String.


end implement wsBE_PerformVIPPRJ
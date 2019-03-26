%

implement wsBE_PerformCMD
    open core

clauses
    getExecuteCommandLine(_Parent, _SourceFile, ApplicationFile, CommandLine, ArgumentStr) = tuple(string::concat(CommandLine," ",ArgumentStr), ApplicationFile, "").

clauses
    tryParseError("", _)=tuple("", 0, "").

end implement wsBE_PerformCMD
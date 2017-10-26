%

implement wsBE_PerformCMD
    open core

clauses
    getExecuteCommandLine(_Parent, _SourceFile, CommandLine, ArgumentStr) = tuple(string::concat(CommandLine," ",ArgumentStr),"").

clauses
    tryParseError("")=tuple("", 0, "").

end implement wsBE_PerformCMD
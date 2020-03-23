%

interface wsBE_Perform
    open core

constants
    performError_C : symbol = "error".
    performWarning_C : symbol = "warning".

domains
    errorNumber = unsigned.

predicates
    tryParseError : (string StringInLowerCase,namedValue* KeyWords)->tuple{symbol ErrorOrWarning,errorNumber ErrorNumber, string FileName} determ.

predicates
%    tryGetTargetFile : (string SourceFile) -> string TargetFile determ.
    getExecuteCommandLine : (wsBE_Performer Parent, string SourceFile, string ApplicationFile, string CommandLine, string ArgumentStr) -> tuple{string,string,string}.

end interface wsBE_Perform
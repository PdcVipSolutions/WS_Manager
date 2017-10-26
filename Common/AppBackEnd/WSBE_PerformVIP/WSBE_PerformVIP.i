%

interface wsBE_PerformVIPPRJ
    supports wsBE_Perform
    open core

%predicates
%    tryParseError : (string StringInLowerCase)->tuple{symbol ErrorOrWarning,wsBE_Perform::errorNumber ErrorNumber, string FileName} determ.

end interface wsBE_PerformVIPPRJ
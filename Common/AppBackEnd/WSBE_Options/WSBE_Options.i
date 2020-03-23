%

interface wSBE_Options
    open core

% VirtualDir supports
predicates
    getShortFileName : (string FullFileName) -> string ShortFileName.
    getFullFileName : (string ShortFileName) -> string FullFileName.
    getVirtualDir_nd : (string Name [out],string DirValue [out],boolean FlagVip [out],string IdeVarsFile [out]) nondeterm.
    existVirtualName : (string Name) determ.
    insertVirtualDir : (string Name,string NewDirValue).
    updateVirtualDir : (string Name,string NewDirValue).
    deleteVirtualDir : (string Name).
    getSourceColor_nd : (string Name [out],integer FGColor [out],integer BGColor [out]) nondeterm.
    setVipVirtualDir : (namedValue* VipVirtualDir).
    setWSVariableFile : (string NewWSVFile).
properties
    wsv_file : string.

% Perform's options supports
predicates
    loadOptionsByExt : (string Ext) -> tuple{string, namedValue*}*.
    getExtOptions_nd : (string Name [out],string* Ext [out]) nondeterm.
    updateExtOptionsList : (namedValue* ExtOptionsList).
    updateOptionsByExt : (string Ext,string OptionsValuesStr,object TaskQueueObj).
    splitExtListStr : (string ExtListStr) -> string* ExtList.
    loadOptionsBySource : (string SourceNodeID) -> tuple{string, namedValue*}*.
    updateSourceLocalOptions : (namedValue* ExtOptionsList).
    getSelectSourceType : () -> string SelectSourceType.
    updateSelectSourceType : (namedValue* SelectSourceType).
    getCompRun_nd : (string CompName [out],string Synname [out]) nondeterm.

predicates
    getFrontEndOptionsList : () -> namedValue*.
    setFrontEndOptions : (namedValue* FrontEndOptions).
    updateSourceColors : (namedValue* SourceColorsList).
    updateUILanguage : (namedValue* FrontEndOptions).

predicates
    updateOptionsNotifyFE : (object TaskQueueObj).

end interface wSBE_Options
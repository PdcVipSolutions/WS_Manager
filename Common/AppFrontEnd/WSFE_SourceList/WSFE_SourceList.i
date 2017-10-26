%

interface wSFE_SourceList
    open core

properties
    sourceList_P:listViewControl.
    vip_P : integer.
    vip_bw_P : integer.
    txt_P : integer.
    txt_light_P : integer.
    pzl_bk_P : integer.
    pzl_wt_P : integer.

predicates
    addSource:(namedValue* Parameters).

predicates
    setAllInQueue:(string* NodeIdList) -> string* QueueNodeIdList.

predicates
    getAllInQueue:() -> string* QueueNodeIdList.

predicates
    run:(boolean TrueIfAll,boolean TrueIfReRun).

predicates
    execRun : ().

predicates
    showPerformStatus:(string SourceID,string Status,string Errors,string Warnings,string DateTime).

predicates
    sourceUp:(command).

predicates
    sourceDown:(command).

predicates
    tryGetSelectSourceFileName : () -> string FileName determ.

predicates
    setSourceItemGroup : (listViewControl::itemId, tuple{string, string, string}).
    tryGetSourceItemGroup : (listViewControl::itemId) -> tuple{string, string, string} determ.
    clearSourceItemGroup : ().

predicates
    getItemStatus : (listViewControl::itemId) -> string LocalStatus.
    clearLocalStatus : ().

predicates
    setSourceColors:(namedValue* PerformParams).

end interface wSFE_SourceList
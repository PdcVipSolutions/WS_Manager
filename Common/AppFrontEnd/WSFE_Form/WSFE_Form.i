/************************************************************************

                        Copyright ©

************************************************************************/

interface wsFE_Form supports formWindow
    supports splitForm{string}
    open core

predicates
    display : (ws_FrontEnd FrontEnd).

properties
    progress : core::predicate{integer,string}.
    ribbonControl_ctl:ribboncontrol.
    wsFE_Command_P:wsFE_Command.
    selectNode_V:namedValue*.
    localOptionsPanel_P :groupBox.

predicates
    progressBar_activate : (positive Count).
    progressBar_remove : ().
    progressBar_progress : (positive Progress).

predicates
    setStatusBar:().
    setTitle : (string WS_FileName,boolean TrueIfReadOnly).
    showLocalOptionsPanel : ().
    showLocalOptionsPanel : (boolean IsShown).

predicates
    onSelectedProject : ().

predicates
    setExtOptionsList:(namedValue* PerformParams).
    showSourceLocalOptions : ().
    setLocalExtOptionsList:(namedValue* PerformParams).

predicates
    getSourceFilterList : () -> menuCommand::menuItem*.
    setCheckedFilter : (string*).
%    showAll : (boolean IsShowAll).
%    setFilter : (string* ExtList,boolean SetFilter).

end interface wsFE_Form
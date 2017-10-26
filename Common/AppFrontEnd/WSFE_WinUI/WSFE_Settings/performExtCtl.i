%

interface performExtCtl supports control
    open core

domains
    extOptions = tuple{string, namedValue*}*.

properties
    extOptionsList_P : namedValue*.
    newExtOptionsList : namedValue*.

    openTab_P : openTab.
    runTab_P : runTab.
    execTab_P : execTab.
    extLBox_ctl : listBox.

    tabControl_ctl : tabcontrol.

predicates
    updateResultString : ().
predicates
    updateAttribute : (editControl Source).
predicates
    createNewExtOptionsList : ().
    getExtOptionsValues : (string ExtName,string ExtOptionsStr) ->  extOptions OptionsValues.

end interface performExtCtl
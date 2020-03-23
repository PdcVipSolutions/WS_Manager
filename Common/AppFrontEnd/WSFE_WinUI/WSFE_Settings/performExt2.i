%

interface performExt2 supports control
    open core

domains
    extOptions = tuple{string, namedValue*}*.

properties
    extOptionsList_P : namedValue*.
    newExtOptionsList_P : namedValue*.
    extLBox_P : listBox (o).
    commandTab_P : commandtab (o).
    pbReOrder_P : button (o).
    lbCommandIndex_P : listButton (o).
    stCommand_P : textControl (o).

predicates
    updateAttribute : (editControl Source).
predicates
    createNewExtOptionsList : ().
    getExtOptionsValues : (string ExtName,string ExtOptionsStr) ->  extOptions OptionsValues.

predicates
    tryCreatePopUp : (vpiDomains::menuTag*) -> vpiDomains::menuItem* determ.
    reOrderCommands : (vpiDomains::menuTag MenuTag).

end interface performExt2
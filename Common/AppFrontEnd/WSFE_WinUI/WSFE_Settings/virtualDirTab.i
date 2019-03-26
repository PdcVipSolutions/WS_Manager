%

interface virtualDirTab supports control
    open core

properties
    vip_P : integer.
    sm_P : integer.

    virtualDirListView_P : listviewcontrol (o).
    new_P : button (o).
    edit_P : button (o).
    delete_P : button (o).
    stWSVariable_P : textControl (o).
    edWSVariableFile_P : editControl (o).
    pbBrowse_P : button (o).

predicates
    setVirtualDirList : (namedValue_list PerformResultParams).

predicates
    getNextId : () -> listViewControl::itemId.

predicates
    existName : (string Name) determ.

end interface virtualDirTab
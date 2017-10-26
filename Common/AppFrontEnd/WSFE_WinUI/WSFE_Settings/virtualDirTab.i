%

interface virtualDirTab supports control
    open core

properties
    vip_P : integer.
    sm_P : integer.

    virtualDirListView_ctl : listviewcontrol.
    new_ctl : button.
    edit_ctl : button.
    delete_ctl : button.

predicates
    setVirtualDirList : (namedValue_list PerformResultParams).

predicates
    getNextId : () -> listViewControl::itemId.

predicates
    existName : (string Name) determ.

end interface virtualDirTab
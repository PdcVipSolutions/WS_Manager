%

interface generalTab supports control
    open core

properties
    listViewControl_P : listviewcontrol (o).
    pbFontColor_P : button (o).
    staticText_P : textControl (o).
    pbBGColor_P : button (o).
    stCurrentLang_P : textControl (o).
    languageList_P : listBox (o).

predicates
    getNewSourceColors : () -> namedValue*.

predicates
    tryGetNewLanguage : () -> string NewLanguage determ.

end interface generalTab
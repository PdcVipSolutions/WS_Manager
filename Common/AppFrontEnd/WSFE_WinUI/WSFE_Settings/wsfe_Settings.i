%

interface wsfe_Settings supports dialog
    open core

predicates
    getDialogTitleList : (string Key) -> namedValue*.

predicates
    onGetMacroSymbolsClick : button::clickResponder.

predicates
    onBrowseEditorClick : button::clickResponder.

predicates
    onPbReOrderClick : button::clickResponder.

predicates
    onBrowseWSVClick : button::clickResponder.

end interface wsfe_Settings
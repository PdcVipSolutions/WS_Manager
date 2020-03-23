%

interface workSpaceCommand
    supports commandBlock
    open core

predicates
    initWS_Menu:(window Win)->ribbonControl::block.
    initWSTree_Menu:(window Win)->ribbonControl::block.
    addChangeListener:().

end interface workSpaceCommand
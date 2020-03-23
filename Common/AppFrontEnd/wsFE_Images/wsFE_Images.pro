%

implement wsFE_Images
    inherits imageList
    inherits wsFE_Connector
    open core

constants
    imageWidth_C = 16.
    imageHeight_C = 16.

facts
    hashImages : mapM{string, integer} := mapM_redBlack::new().

clauses
    new(WS_FrontEnd):-
        wsFE_Connector::new(WS_FrontEnd),
        imageList::new(imageWidth_C, imageHeight_C, imageList::pixelFormat_32bpp).

clauses
    getSourceImageIdx(SourceFile) = ImageIdx :-
        Key = string::toLowerCase(filename::getExtension(SourceFile)),
        ImageIdx = getImageIdx(Key, SourceFile).

predicates
    getImageIdx : (string Key,string FilePath) -> integer ImageIdx.
clauses
    getImageIdx(Key, FilePath) = ImageIdx :-
        if ImageIdx = hashImages:tryGet(Key) then
        else
            HIcon = shell_api::extractAssociatedIcon(FilePath, _, _),
            ImageIdx = addIcon(Hicon),
            hashImages:set(Key, ImageIdx)
        end if.

end implement wsFE_Images
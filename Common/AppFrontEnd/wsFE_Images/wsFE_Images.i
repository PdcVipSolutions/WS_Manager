%

interface wsFE_Images supports imageList
    open core

predicates
    getSourceImageIdx : (string SourceFile) -> integer ImageIdx.

end interface wsFE_Images
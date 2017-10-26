%

implement wsBE_PerformEXC
    open core

facts
    useExe_P:useExe:=erroneous.
    input_P:inputStream:=erroneous.

facts
    sourcePerformer_P:string:="".
    sourceEditor_P:string:=@"c:\windows\system32\notepad.exe".

clauses
    new(FullProjectFileName):-
        Path=fileName::getPath(FullProjectFileName),
        directory::setCurrentDirectory(Path),
        useExe_P:=useExe::new(string::format(@["%"],FullProjectFileName)),
        useExe_P:setApplicationName(sourceEditor_P).

clauses
    new(_RunMode,FileName):-
        Path=fileName::getPath(FileName),
        directory::setCurrentDirectory(Path),
        Task=file::readString(FileName),
        Pos=string::search(Task,"exe "),
        string::front(Task,Pos+3,ApplicationName,AppParameters),
        !,
        useExe_P:= useExe::new(AppParameters),
        useExe_P:setApplicationName(ApplicationName),
        input_P:=useExe_P:getFromProcessStream(stream::ansi(866)).
    new(_RunMode,FileName):-
        exception::raise_User(string::format("Not found %s\n",FileName)).

clauses
    tryParseError("")=tuple("", 0, "").

clauses
    invokeOpenSource():-
        useExe_P:run().

clauses
    stopRun().


end implement wsBE_PerformEXC
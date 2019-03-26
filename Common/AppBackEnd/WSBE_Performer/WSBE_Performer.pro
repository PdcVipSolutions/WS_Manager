%

implement wsBE_Performer
    inherits wsBE_Connector
    open core, ws_EventManager, xmlNavigate, pfc\log\

clauses
    new(BackEnd):-
        wsBE_Connector::new(BackEnd).

facts
    stopRunFlag_P:boolean:=false.
    state_V:integer:=erroneous.
    errors_V:integer := 0.
    warnings_V:integer := 0.
    currentlyExecuting_V:wSBE_PerformByExt:=erroneous.

    runTotalCount_V:integer := 0.
    runDone_V:integer := 0.
    runFailed_V:integer := 0.
    runNoPath_V:integer := 0.
    runNoRule_V:integer := 0.

clauses
    wsBE_Options_P() = wsBE_Options().
    wsBE_XmlDocument_P() = wsBE_XmlDocument().

predicates
    getFullFileName : (string ShortFileName) -> string FullFileName.
clauses
    getFullFileName(Short) = Full :-
        Full = wsBE_Options():getFullFileName(Short),
        if not(file::existExactFile(Full)) then
            state_V := noPathStatus_C
        end if.

clauses
    execSource(_SourceObj, _SourceFileName, _TaskQueueObj).

clauses
    openSource(SourceObj, SourceFileName,TaskQueueObj):-
        Dir=directory::getCurrentDirectory(),
        state_V := performingStatus_C,
        FullFileName = getFullFileName(SourceFileName),
        Options = wSBE_Options():loadOptionsByExt(fileName::getExtension(FullFileName, _Name)),
        if BuildType = tryGetBuildType(Options) then
            stopRunFlag_P := false,
            runSource(SourceObj,BuildType,SourceFileName,TaskQueueObj)
        end if,
        directory::setCurrentDirectory(Dir).

class predicates
    tryGetBuildType : (tuple{string, namedValue*}* OptionsList) -> string BuildType determ.
clauses
    tryGetBuildType(OptionsList) = BuildType :-
        tuple(command_C, Options) in OptionsList,
        "true" = namedValue::tryGetNamed_string(Options, defCommand_C),
        BuildType = namedValue::tryGetNamed_string(Options, index_C),
        !.

clauses
    checkFile(SourceObj, SourceFileName, TaskQueueObj) :-
        Full = wsBE_Options():getFullFileName(SourceFileName),
        if not(file::existExactFile(Full)) then
            if _ = SourceObj:tryGetAttribute(status_C) then
                SourceObj:modifyAttribute(status_C, toString(noPathStatus_C))
            else
                SourceObj:addAttribute(status_C, toString(noPathStatus_C))
            end if,
            wsBE_Tasks():saveWorkSpace(),
            notify(wsBE_UpdateSourceStatus_C,
                [
                namedValue("sourceID",string(toString(SourceObj))),
                namedValue("status",string(toString(noPathStatus_C))),
                namedValue("errors",string("0")),
                namedValue("warnings",string("0")),
                namedValue("datetime",string(""))
                ],TaskQueueObj)
        elseif toString(noPathStatus_C) = SourceObj:tryGetAttribute(status_C) then
            SourceObj:removeAttribute(status_C),
            wsBE_Tasks():saveWorkSpace(),
            notify(wsBE_UpdateSourceStatus_C,
                [
                namedValue("sourceID",string(toString(SourceObj))),
                namedValue("status",string("")),
                namedValue("errors",string("0")),
                namedValue("warnings",string("0")),
                namedValue("datetime",string(""))
                ],TaskQueueObj)
        end if.

clauses
    runSource(_SourceObj,_BuildType,_SourceFileName,_TaskQueueObj) :-
        stopRunFlag_P=true,
        !.
    runSource(SourceObj,BuildType,SourceFileName,TaskQueueObj) :-
        % call the runner
        Dir=directory::getCurrentDirectory(),
        state_V := performingStatus_C,
        stopRunFlag_P := false,
        Ext=fileName::getExtension(SourceFileName),
        FullFileName = getFullFileName(SourceFileName),
        if state_V <> noPathStatus_C then
            Performer = perform(SourceObj,string::toLowerCase(Ext),Dir,namedValue(BuildType,string(FullFileName)),FullCommandLine),
            currentlyExecuting_V := Performer,
            errors_V := 0,
            warnings_V := 0,
            updateFEStatus(SourceObj, SourceFileName, ws_Events():getString(state_V), TaskQueueObj),
            try
                CurrentStatus = if CS = tryToTerm(integer, (SourceObj:tryGetAttribute(status_C))) then CS else state_V end if,
                if Performer:winAssExecute_P = true then
                    shell_api::shellOpen(FullFileName),
                    if CurrentStatus <> state_V then
                        state_V := CurrentStatus
                    end if
                else
                    OutputStreamMode = if Performer:streamModeIndex_P = 0 then stream::unicode
                                                        elseif Performer:streamModeIndex_P = 1 then stream::ansi(Performer:codePage_P)
                                                        else stream::binary end if,
                    if Performer:performRun_P = false then
                        if Performer:streamModeOn_P = true then
                            Performer:useExe_P := useExe::new(FullCommandLine),
                            Performer:input_P := Performer:useExe_P:getFromProcessStream(OutputStreamMode),
                            Performer:useExe_P:setShowWindow(false),
                            Performer:useExe_P:setNativeCreationFlags(multiThread_native::create_new_process_group),
                            if file::existExactFile(Performer:inputStreamFile_P) then
                                FileHandle = fileSystem_api::createFile(Performer:inputStreamFile_P, fileSystem_native::generic_read, fileSystem_api::permitRead),
                                Performer:useExe_P:setToProcessHandle(FileHandle, true)
                            end if,
                            Performer:useExe_P:run(),
                            handleStream(SourceObj,Performer, 0,TaskQueueObj),
                            state_V := if true = stopRunFlag_P then stoppedStatus_C else doneStatus_C end if,
                            _ = Performer:useExe_P:wait(),
                            Performer:useExe_P:close()
                        else
                            Performer:useExe_P := useExe::new(FullCommandLine),
                            if file::existExactFile(Performer:inputStreamFile_P) then
                                FileHandle = fileSystem_api::createFile(Performer:inputStreamFile_P, fileSystem_native::generic_read, fileSystem_api::permitRead),
                                Performer:useExe_P:setToProcessHandle(FileHandle, true)
                            end if,
                            Performer:useExe_P:run(),
                            state_V := doneStatus_C
                        end if
                    else
                        directory::setCurrentDirectory(Dir),
                        notify(weBE_RunSource_C,
                            [
                            namedValue(sourcePerformer_C, string(FullCommandLine)),
                            namedValue(streamMode_C,string(toString(OutputStreamMode)))
                            ],TaskQueueObj)
                    end if
                end if
            catch _TraceId do
                notify(weBE_WriteMessage_C,[namedValue(emptyString_C, string(string::format(ws_Events():getString(runImpossible), SourceFileName)))],TaskQueueObj),
                state_V := exceptedStatus_C
            finally
                if Performer:performRun_P = false then
                    if errors_V>0 then state_V:=failedStatus_C end if,
                    if state_V = doneStatus_C then
                        runDone_V := runDone_V + 1
                    elseif state_V = failedStatus_C then
                        runFailed_V := runFailed_V + 1
                    elseif state_V = noPathStatus_C then
                        runNoPath_V := runNoPath_V + 1
                    elseif state_V = noRuleStatus_C then
                        runNoRule_V := runNoRule_V + 1
                    end if,
                    NewStatus = if state_V = performingStatus_C then
                        ""
                    else
                        ws_Events():getString(state_V)
                    end if,
                    updateFEStatus(SourceObj, SourceFileName, NewStatus, TaskQueueObj),
                    directory::setCurrentDirectory(Dir),
                    currentlyExecuting_V:=erroneous,
                    wsBE_Tasks():doRunSource(TaskQueueObj)
                end if
            end try
        else
            updateFEStatus(SourceObj, SourceFileName, ws_Events():getString(state_V),TaskQueueObj),
            directory::setCurrentDirectory(Dir),
            currentlyExecuting_V:=erroneous,
            wsBE_Tasks():doRunSource(TaskQueueObj)
        end if.

clauses
    stopRun(_TaskQueueObj):-
        isErroneous(currentlyExecuting_V),
        !.
    stopRun(TaskQueueObj):-
        if currentlyExecuting_V:performRun_P = false then
            currentlyExecuting_V:stopRun()
        else
            notify(weBE_StopRunSource_C, [],TaskQueueObj)
        end if,
        stopRunFlag_P:=true.

predicates
    updateFEStatus:(xmlElement SourceObj,string SourceFileName, string Status,object TaskQueueObj).
clauses
    updateFEStatus(_SourceObj, _SourceFileName, _Status, _TaskQueueObj):-
        CurrentlyExecuting = notErroneous(currentlyExecuting_V),
        CurrentlyExecuting:checkStatus_P = false,
        !.
    updateFEStatus(SourceObj, SourceFileName, Status, TaskQueueObj):-
        if _ = SourceObj:tryGetAttribute(errors_C) then
            if errors_V > 0 then
                SourceObj:modifyAttribute(errors_C, toString(errors_V))
            else
                SourceObj:removeAttribute(errors_C)
            end if
        elseif errors_V > 0 then
            SourceObj:addAttribute(errors_C, toString(errors_V))
        end if,
        if _ = SourceObj:tryGetAttribute(warnings_C) then
            if warnings_V > 0 then
                SourceObj:modifyAttribute(warnings_C, toString(warnings_V))
            else
                SourceObj:removeAttribute(warnings_C)
            end if
        elseif warnings_V > 0 then
            SourceObj:addAttribute(warnings_C, toString(warnings_V))
        end if,
        if Status <> "" and Status <> ws_Events():getString(performingStatus_C) then
            DateTime = getDateTime()
        else
            DateTime = ""
        end if,
        if DateTime <> "" and not(SourceObj:tryModifyAttribute(datetime_C,  DateTime)) then
            SourceObj:addAttribute(datetime_C,  DateTime)
        elseif DateTime = "" and _ = SourceObj:tryGetAttribute(datetime_C) then
            SourceObj:removeAttribute(datetime_C)
        end if,
        if state_V <> performingStatus_C and not(SourceObj:tryModifyAttribute(status_C, toString(state_V))) then
            SourceObj:addAttribute(status_C, toString(state_V))
        end if,
        NVStatus = if Status = "" then "" else toString(state_V) end if,
        notify(wsBE_UpdateSourceStatus_C,
            [
            namedValue("sourceID",string(toString(SourceObj))),
            namedValue("status",string(NVStatus)),
            namedValue("errors",string(toString(errors_V))),
            namedValue("warnings",string(toString(warnings_V))),
            namedValue("datetime",string(DateTime))
            ],TaskQueueObj),
        notify(wsBE_UpdateRunStatus_C,
            [
            namedValue(statusPrefix_C,string(ws_Events():getString(totalReady_C))),
            namedValue(runSourceFile_C,string(SourceFileName)),
            namedValue(totalCount_C,integer(runTotalCount_V)),
            namedValue(runDone_C,integer(runDone_V)),
            namedValue(runFailed_C,integer(runFailed_V)),
            namedValue(runNoPath_C,integer(runNoPath_V)),
            namedValue(runNoRule_C,integer(runNoRule_V))
            ],TaskQueueObj).

class predicates
    getDateTime : () -> string.
clauses
    getDateTime() = Date_Time :-
        Time = time::now(),
        Time:getDateAndTime(Year, Month, Day, Hour, Minute, Second),
        Date_Time = string::format("%.%02.% %02:%02:%02", Day, Month, Year, Hour, Minute, Second).

predicates
    perform:(xmlElement SourceObj,string Ext,string CurrentBinDir,namedValue Param,string FullCommandLine [out])->wSBE_PerformByExt.
clauses
    perform(SourceObj,_Ext,_CurrentBinDir,namedValue(BuildType,string(SourceFileName)),FullCommandLine) = Performer:-
        !,
        Performer = wsBE_PerformByExt::new(This, BuildType, SourceFileName, SourceObj),
        FullCommandLine = Performer:startRun(BuildType).
    perform(_SourceObj,_Ext,_CurrentBinDir,_CmdStr, _) = _ :-
        exception::raise_User("Unexpected Alternative!").

constants
    maxRowsToProcess = 10000.

predicates
    handleStream : (xmlElement SourceObj,wSBE_PerformByExt, positive LineNumber,object TaskQueueObj).
clauses
    handleStream(SourceObj,_Performer, LineNumber,TaskQueueObj) :-
        updateFEStatus(SourceObj, SourceObj:attribute(fileName_C), ws_Events():getString(state_V),TaskQueueObj),
        maxRowsToProcess < LineNumber,
        !.
    handleStream(SourceObj, Performer, LineNumber,TaskQueueObj) :-
        if
            not(Performer:input_P:endOfStream())
        then
            Line = Performer:input_P:readLine(),
            notify(weBE_WriteMessage_C,[namedValue(emptyString_C, string(string::format("%\n",Line)))],TaskQueueObj),
            checkErrorsWarnings(Performer, Line,TaskQueueObj),
            handleStream(SourceObj, Performer, LineNumber + 1,TaskQueueObj)
        else
            EventCode = Performer:useExe_P:wait(50),
            if EventCode = multiThread_native::wait_timeout then
                handleStream(SourceObj, Performer, LineNumber,TaskQueueObj)
            end if
        end if.

clauses
    handleStreamFrontEnd(EndRunSource, Line, TaskQueueObj):-
        if EndRunSource = false then
            notify(weBE_WriteMessage_C,[namedValue(emptyString_C, string(string::format("%\n",Line)))],TaskQueueObj),
            checkErrorsWarnings(currentlyExecuting_V, Line,TaskQueueObj)
        else
            state_V := if true = stopRunFlag_P then stoppedStatus_C else doneStatus_C end if,
            if errors_V>0 then state_V:=failedStatus_C end if,
            if state_V = doneStatus_C then
                runDone_V := runDone_V + 1
            elseif state_V = failedStatus_C then
                runFailed_V := runFailed_V + 1
            elseif state_V = noPathStatus_C then
                runNoPath_V := runNoPath_V + 1
            elseif state_V = noRuleStatus_C then
                runNoRule_V := runNoRule_V + 1
            end if,
            if
                SourceObj = currentlyExecuting_V:sourceObj_P,
                FileName = SourceObj:attribute(fileName_C)
            then
                updateFEStatus(SourceObj, FileName, ws_Events():getString(state_V), TaskQueueObj),
                currentlyExecuting_V:=erroneous
            end if
        end if.

predicates
    checkErrorsWarnings : (wSBE_PerformByExt,string,object TaskQueueObj).
clauses
    checkErrorsWarnings(Performer,String,TaskQueueObj):-
        if
            tuple(ErrorType, _ErrorNumber, _FileName) = Performer:tryParseError(string::toLowerCase(String)),
            SourceObj = Performer:sourceObj_P,
            FileName = SourceObj:attribute(fileName_C)
        then
            if wSBE_Perform::performWarning_C = ErrorType then
                warnings_V := warnings_V + 1
            elseif wSBE_Perform::performError_C = ErrorType then
                errors_V := errors_V + 1
            end if,
            updateFEStatus(SourceObj, FileName, ws_Events():getString(state_V),TaskQueueObj)
        end if.

end implement wsBE_Performer
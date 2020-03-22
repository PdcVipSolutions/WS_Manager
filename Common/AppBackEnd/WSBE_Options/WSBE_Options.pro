%

implement wSBE_Options
    inherits wsBE_Connector
    open core, ws_EventManager, xmlNavigate

domains
    extOptions = tuple{string, namedValue*}*.

facts
    xml_Options : xmlDocument.
    xml_ExtOptions : (string ExtName, string_list ExtList, xmlElement).

facts - macroDir_FB
    wsv_file : string := "".
    virtualDir : (string Name,string DirValue,boolean FlagVip,string IdeVarsFile).

clauses
    new(BackEnd):-
        wsBE_Connector::new(BackEnd),
        xml_Options:=xmlDocument::new("wsm_options"),
        xml_Options:codePage_P:=utf8,
        xml_Options:indent_P:=true,
        xml_Options:xmlStandalone_P:=xmlLite::yes,
        loadOptionsWSM().

constants
    vpDirToolsKey = @"Software\Prolog Development Center\Visual Prolog6\settings\toolsDirList".
%    virtualDirFileFB_C = @"WorkSpaceBE.opt".
%    optionsBEFile_C = @"OptionsBE.xml".
    optionsWSMFile_C = @"wsmAppData\OptionsWSM.xml".

predicates
    loadOptionsWSM : ().
clauses
    loadOptionsWSM():-
        if file::existExactFile(optionsWSMFile_C) then
            XmlOptions = inputStream_file::openFile(optionsWSMFile_C, stream::binary),
            spbXmlLigntSupport::read(XmlOptions, xml_Options),
            XmlOptions:close(),
%            upgradeBackEndOptions(),
            loadVirtualDir(),
            loadPerformGroup(),
            loadPerformOptions(),
            loadColorOptions(),
            loadCurrentLanguage()
        else
            setDefaultOptionsWSM()
        end if.

predicates
    loadPerformGroup : ().
clauses
    loadPerformGroup():-
        BEOptions = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)})]),
        !,
        if _ = xml_Options:getNode_nd([current(BEOptions), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "CompRun" })]) then
        else
            CRGroup = xmlElement::new("", groupNode_C, BEOptions),
            CRGroup:addAttribute(title_C, "CompRun"),
            BEOptions:addNode(CRGroup),
%            foreach tuple(Name, Synname) in [tuple("wsBE_PerformVIPPRJ", "Visual Prolog"),tuple("wsBE_PerformCMD","Command File")] do
                tuple(Name, Synname) = tuple("wsBE_PerformVIPPRJ", "Visual Prolog"),
                CRItem = xmlElement::new("", componentName_C, CRGroup),
                CRItem:addAttribute(name_C, Name),
                CRItem:addAttribute(synname_C, Synname),
                CRGroup:addNode(CRItem),
%            end foreach,
            saveWSMOptions()
        end if.
    loadPerformGroup().

predicates
    loadCurrentLanguage : ().
clauses
    loadCurrentLanguage():-
        if LngNode = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}),
                                child(groupNode_C, { (O) :- O:attribute(title_C) = wsFE_Form_C }), child("lang", {(_)})]),
            CurrentLng = LngNode:attribute("cur")
        then
            ws_Events():setCurrentLng(CurrentLng)
        end if.

predicates
    saveWSMOptions : ().
clauses
    saveWSMOptions():-
        OutputStream = outputStream_file::create(optionsWSMFile_C, stream::binary),
        xml_Options:saveXml(OutputStream).

predicates
    loadVirtualDir : ().
clauses
    loadVirtualDir():-
        foreach
            Node = xml_Options:getNode_nd(
            [root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(_)})]),
            VirtualName = Node:attribute(virtDirName_C), VirtualDir = Node:attribute(path_C)
        do
            assert(virtualDir(VirtualName, VirtualDir, false, ""))
        end foreach,
        loadVirtualDirVIP().

predicates
    loadVirtualDirVIP : ().
clauses
    loadVirtualDirVIP():-
        try
            DirToolsList = registry::getAllValues(registry::currentUser, vpDirToolsKey)
        catch _ do
            DirToolsList = []
        end try,
        foreach
            namedValue(Name, string(VirtualDir)) in DirToolsList,
                Name <> ""
        do
            VirtualName = string::format("$(%s)", Name),
            asserta(virtualDir(VirtualName, VirtualDir, true, ""))
        end foreach.

clauses
    setVipVirtualDir(VipVirtualDir):-
        foreach
            namedValue(Name, string(VirtualDir)) in VipVirtualDir,
                Name <> ""
        do
            VirtualName = string::format("$(%s)", Name),
            asserta(virtualDir(VirtualName, VirtualDir, true, ""))
        end foreach.

clauses
    setWSVariableFile(NewWSVFile):-
        if file::existExactFile(NewWSVFile) then
            retractAll(virtualDir(_, _, _, _)),
            retractFactDb(ideVars),
            loadVirtualDirFromFile(NewWSVFile)
        elseif NewWSVFile = "" then
            wsv_file := "",
            retractAll(virtualDir(_, _, _, _)),
            loadVirtualDir()
        else
            createWSVariableFile(NewWSVFile)
        end if.

facts - ideVars
    td : (string MacroName, string MacroDir) nondeterm.
    nextFile : (string File) determ.

predicates
    createWSVariableFile : (string NewWSVFile).
clauses
    createWSVariableFile(NewWSVFile):-
        foreach
            virtualDir(MacroName, MacroDir, _, _),
            string::hasPrefix(MacroName, "$(", Rest),
            string::hasSuffix(Rest, ")", Name)
        do
            assert(td(Name, MacroDir))
        end foreach,
        file::saveUtf8(NewWSVFile, ideVars),
        retractFactDb(ideVars),
        loadVirtualDirFromFile(NewWSVFile).

predicates
    loadVirtualDirFromFile : (string NewWSVFile).
clauses
    loadVirtualDirFromFile(NewWSVFile):-
        wsv_file := NewWSVFile,
        readTSourceFileName(NewWSVFile, setM_redBlack::new()).

predicates
    readTSourceFileName : (string StartFilename, setM{string} Fence).
clauses
    readTSourceFileName(StartFilename, Fence) :-
        file::consult(StartFilename, ideVars),
        foreach retract(td(Name, MacroDir)) do
            MacroName = string::format("$(%s)", Name),
            assert(virtualDir(MacroName, MacroDir, false, StartFilename))
        end foreach,
        if retract(nextFile(NewFilename)) and not(NewFilename in Fence) and file::existExactFile(NewFilename) then
            Fence:insert(NewFilename),
            readTSourceFileName(NewFilename, Fence)
        end if.

predicates
    loadPerformOptions : ().
clauses
    loadPerformOptions():-
        foreach Node = xml_Options:getNode_nd(
            [root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "PerformExt" }), child(source_C, {(_)})]),
            ExtList = splitExtListStr(Node:attribute(extName_C)), ExtName = Node:attribute(name_C) do
                assert(xml_ExtOptions(ExtName, ExtList, Node))
        end foreach.

clauses
    splitExtListStr(ExtListStr) =
        list::filteredMap(string::split(ExtListStr, separatorsExt), {(S) = R :- R = string::trim(S), R <> ""}).

predicates
    setDefaultOptionsWSM : ().
clauses
    setDefaultOptionsWSM():-
        loadVirtualDirVIP(),
% Create <be_options> Node
        BEOptions = xmlElement::new("", backEndOptions_C, xml_Options:root_P),
        xml_Options:root_P:addNode(BEOptions),
% Create <group title="VirtualDir"> Node
        VirtDirGroup = xmlElement::new("", groupNode_C, BEOptions),
        VirtDirGroup:addAttribute(title_C, "VirtualDir"),
        BEOptions:addNode(VirtDirGroup),
% Create <group title="CompRun"> Node
        loadPerformGroup(),
% Create <group title="PerformExt"> Node
        ExtGroup = xmlElement::new("", groupNode_C, BEOptions),
        ExtGroup:addAttribute(title_C, "PerformExt"),
        BEOptions:addNode(ExtGroup),
% Create <source ext="vipprj" name="Vip Project"> Node
        VipGroup = xmlElement::new("", source_C, ExtGroup),
        VipGroup:addAttribute(extName_C, "vipprj"),
        VipGroup:addAttribute(name_C, "Vip Project"),
        VipGroup:addAttribute(componentName_C, "wsBE_PerformVIPPRJ"),
        ExtGroup:addNode(VipGroup),
        createCommandNodes(VipGroup,
            [
                tuple(1, [namedValue(name_C, string("Edit")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true")),
                            namedValue(formatCmd_C, string(@{[Application] "$(SourceFile)"})),namedValue(fileName_C, string("$(VipDir)bin\\vip.exe"))]),
                tuple(2, [namedValue(name_C, string("Build")), namedValue(formatCmd_C, string(@{[Application] [Arguments] "$(SourceFile)"})),
                            namedValue(fileName_C, string("$(VipDir)bin\\vipBuilder.exe")), namedValue(argument_C, string("/build /saveproject")),
                            namedValue(streamModeOn_C, string("true")), namedValue(streamMode_C, string("1")), namedValue(codePage_C, string("65001")),
                            namedValue(checkStatus_C, string("true")),namedValue(allPossible_C, string("true"))]),
                tuple(3, [namedValue(name_C, string("Rebuild")), namedValue(formatCmd_C, string(@{[Application] [Arguments] "$(SourceFile)"})),
                            namedValue(fileName_C, string("$(VipDir)bin\\vipBuilder.exe")), namedValue(argument_C, string("/rebuild /saveproject")),
                            namedValue(streamModeOn_C, string("true")), namedValue(streamMode_C, string("1")), namedValue(codePage_C, string("65001")),
                            namedValue(checkStatus_C, string("true")),namedValue(allPossible_C, string("true"))]),
                tuple(4, [namedValue(name_C, string("RunExe")), namedValue(formatCmd_C, string("[Application]")),
                            namedValue(fileName_C, string("$(SourceExeDir)\\$(ExeName)"))])
            ]),
% Create <source ext="cmd" name="Command File"> Node
        CmdGroup = xmlElement::new("", source_C, ExtGroup),
        CmdGroup:addAttribute(extName_C, "cmd"),
        CmdGroup:addAttribute(name_C, "Command File"),
        ExtGroup:addNode(CmdGroup),
        createCommandNodes(CmdGroup,
            [
                tuple(1, [namedValue(name_C, string("Edit")), namedValue(formatCmd_C, string(@{[Application] "$(SourceFile)"})),
                            namedValue(fileName_C, string("c:\\Windows\\notepad.exe")), namedValue(defCommand_C, string("true"))]),
                tuple(2, [namedValue(name_C, string("Perform")), namedValue(formatCmd_C, string(@{[Application] [Arguments] "$(SourceFile)" [Suffix]})),
                            namedValue(fileName_C, string("C:\\WINDOWS\\System32\\cmd.exe")), namedValue(allPossible_C, string("true")),
                            namedValue(checkStatus_C, string("true")), namedValue(argument_C, string("/u /c")), namedValue(suffix_C, string("<$(SpbVipTools)\\bin\\yes.txt"))]),
                tuple(3, [namedValue(name_C, string("Perform2")), namedValue(formatCmd_C, string(@{[Application] [Arguments] "$(SourceFile)" [Suffix]})),
                            namedValue(fileName_C, string("C:\\WINDOWS\\System32\\cmd.exe")), namedValue(allPossible_C, string("true")),
                            namedValue(checkStatus_C, string("true")), namedValue(argument_C, string("/u /c")), namedValue(suffix_C, string("<$(SpbVipTools)\\bin\\yes.txt"))])
            ]),
% Create <source ext="txt" name="PlainText"> Node
        TxtGroup = xmlElement::new("", source_C, ExtGroup),
        TxtGroup:addAttribute(extName_C, "txt"),
        TxtGroup:addAttribute(name_C, "Plain Text"),
        ExtGroup:addNode(TxtGroup),
        createCommandNodes(TxtGroup,[tuple(1, [namedValue(name_C, string("Edit")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])]),
% Create <source ext="doc" name="Word"> Node
        DocGroup = xmlElement::new("", source_C, ExtGroup),
        DocGroup:addAttribute(extName_C, "doc, docx"),
        DocGroup:addAttribute(name_C, "Word Document"),
        ExtGroup:addNode(DocGroup),
        createCommandNodes(DocGroup,[tuple(1, [namedValue(name_C, string("Open")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])]),
% Create <source ext="xls" name="Excel Spread Sheet"> Node
        XlsGroup = xmlElement::new("", source_C, ExtGroup),
        XlsGroup:addAttribute(extName_C, "xls, xlsx"),
        XlsGroup:addAttribute(name_C, "Excel Spread Sheet"),
        ExtGroup:addNode(XlsGroup),
        createCommandNodes(XlsGroup,[tuple(1, [namedValue(name_C, string("Open")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])]),
% Create <source ext="hhp" name="Help WorkShop"> Node
        HhpGroup = xmlElement::new("", source_C, ExtGroup),
        HhpGroup:addAttribute(extName_C, "hhp"),
        HhpGroup:addAttribute(name_C, "Help WorkShop"),
        ExtGroup:addNode(HhpGroup),
        createCommandNodes(HhpGroup,[tuple(1, [namedValue(name_C, string("Open")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])]),
% Create <source ext="htm" name="Web Page"> Node
        WebGroup = xmlElement::new("", source_C, ExtGroup),
        WebGroup:addAttribute(extName_C, "htm, html"),
        WebGroup:addAttribute(name_C, "Web Page"),
        ExtGroup:addNode(WebGroup),
        createCommandNodes(WebGroup,[tuple(1, [namedValue(name_C, string("Open")), namedValue(winAss_C, string("true")), namedValue(defCommand_C, string("true"))])]),
        loadPerformOptions(),
% Create <fe_options> Node
        FEOptions = xmlElement::new("", frontEndOptions_C, xml_Options:root_P),
        xml_Options:root_P:addNode(FEOptions),
% Create <group title="wsFE_Tasks"> Node
        TaskGroup = xmlElement::new("", groupNode_C, FEOptions),
        TaskGroup:addAttribute(title_C, "wsFE_Tasks"),
        FEOptions:addNode(TaskGroup),
% Create <group title="wsFE_Form"> Node
        FormGroup = xmlElement::new("", groupNode_C, FEOptions),
        FormGroup:addAttribute(title_C, "wsFE_Form"),
        FEOptions:addNode(FormGroup),
% Create <group title="wSFE_SourceList_C"> Node
        SrcListGroup = xmlElement::new("", groupNode_C, FEOptions),
        SrcListGroup:addAttribute(title_C, wSFE_SourceList_C),
        FEOptions:addNode(SrcListGroup),
        ColorList = [namedValue(Name, string(toString(tuple(FGColor, BGColor)))) || getSourceColor_nd(Name, FGColor, BGColor)],
        ColorItem = xmlElement::new("", "color", SrcListGroup),
        ColorItem:addAttribute(textColor_C, toString(ColorList)),
        SrcListGroup:addNode(ColorItem),
        saveWSMOptions().

predicates
    loadColorOptions : ().
clauses
    loadColorOptions():-
        if FEOptions = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)})]) then
        else
            FEOptions = xmlElement::new("", frontEndOptions_C, xml_Options:root_P),
            xml_Options:root_P:addNode(FEOptions)
        end if,
        if SrcListGroup = xml_Options:getNode_nd([current(FEOptions), child(groupNode_C, { (O) :- wSFE_SourceList_C = O:attribute(title_C) })]) then
        else
            SrcListGroup = xmlElement::new("", groupNode_C, FEOptions),
            SrcListGroup:addAttribute(title_C, wSFE_SourceList_C),
            FEOptions:addNode(SrcListGroup)
        end if,
        if ColorNode = xml_Options:getNode_nd([current(SrcListGroup), child("color", {(_)})]) then
        else
            ColorNode = xmlElement::new("", "color", SrcListGroup),
            SrcListGroup:addNode(ColorNode)
        end if,
        if
            ColorValueStr = ColorNode:attribute(textColor_C),
            ColorList = toTerm(namedValue_list, ColorValueStr),
            3 = list::length(ColorList)
        then
        else
            ColorList = [namedValue(Name, string(toString(tuple(FGColor, BGColor)))) || getSourceColor_nd(Name, FGColor, BGColor)],
            ColorNode:addAttribute(textColor_C, toString(ColorList)),
            saveWSMOptions()
        end if.


class predicates
    createCommandNodes : (xmlElement ExtGroup, tuple{integer Index,namedValue* Attributes}*).
clauses
    createCommandNodes(ExtGroup, CommandList):-
        foreach tuple(Index, Attributes) in CommandList do
            CmdNode = xmlElement::new("", command_C, ExtGroup),
            CmdNode:addAttribute(index_C, toString(Index)),
            foreach namedValue(Name, string(Value)) in Attributes do
                CmdNode:addAttribute(Name, Value)
            end foreach,
            ExtGroup:addNode(CmdNode)
        end foreach.

% Virtual Directory Options
clauses
    getVirtualDir_nd(Name, DirValue, FlagVip, IdeVarsFile):-
        virtualDir(Name, DirValue, FlagVip, IdeVarsFile).

clauses
    getShortFileName(FullFileName) = ShortFileName :-
        fileName::getPathAndName(FullFileName, Path, Name),
        isVirtualDir(Path, VirtualName, FullVirtualDir),
        !,
        FullName = fileName::createPath(Path, Name),
        RedName = fileName::reduce(FullName, FullVirtualDir),
        ShortFileName = string::concat(VirtualName, RedName).
    getShortFileName(FullFileName) = FullFileName.

predicates
    isVirtualDir : (string Path,string VirtualName [out],string FullVirtualDir [out]) determ.
clauses
    isVirtualDir(Path, VirtualName, FullVirtualDir) :-
        tuple(VirtualName, VirtualDir) = list::getMember_nd
            (
                list::sortBy({(tuple(_,X), tuple(_,Y)) = string::compareCaseNeutral(X, Y)},
                    [tuple(VN, VD)||virtualDir(VN, VD, _IsVipDir, _IdeVarsFile)], descending)
            ),
        VirtualDir <> "",
        FullVirtualDir = fileName::createPath(directory::getCurrentDirectory(), VirtualDir),
        string::hasPrefixIgnoreCase(Path, FullVirtualDir, _Rest),
        !.

clauses
    getFullFileName(Short) = Full :-
        Full = fileName::createPath(directory::getCurrentDirectory(),
            if memory::pChar('$') = uncheckedConvert(memory::pChar, Short) then
                changeMacro(Short)
            else
                Short
            end if).

predicates
    changeMacro : (string Full) -> string FullUp.
clauses
    changeMacro(Full) = FullUp :-
        virtualDir(VirtualName, Dir, _, _),
        string::hasPrefixIgnoreCase(Full, VirtualName, Rest),
        !,
        RestPath = if string::frontChar(Rest, First, Last) and First = '\\' then Last else Rest end if,
        CurrentDirectory = directory::getCurrentDirectory(),
        FullDir = fileName::createPath(CurrentDirectory, Dir),
        FullUp = fileName::createPath(FullDir, RestPath).
    changeMacro(Full) = Full.

clauses
    existVirtualName(Name):-
        virtualDir(VirtName, _, _, _),
        equal = string::compareIgnoreCase(Name, VirtName),
        !.

clauses
    insertVirtualDir(Name, NewDirValue):-
        VirtualName = string::format("$(%s)", Name),
        if virtualDir(VirtualName, _, _, _) then
            deleteVirtualDir(Name)
        end if,
        if
            wsv_file = "",
            VirtDirGroup = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "VirtualDir" })]), !
        then
            assert(virtualDir(VirtualName, NewDirValue, false, "")),
            insertNewVirtualDir(VirtDirGroup, VirtualName, NewDirValue),
            saveWSMOptions()
        else
            assert(virtualDir(VirtualName, NewDirValue, false, wsv_file)),
            updateWSVFile(wsv_file)
        end if.

predicates
    updateWSVFile : (string WSVFile).
clauses
    updateWSVFile(WSVFile):-
        file::consult(WSVFile, ideVars),
        retractAll(td(_,_)),
        foreach
            virtualDir(MacroName, MacroDir, _, WSVFile),
            string::hasPrefix(MacroName, "$(", Rest),
            string::hasSuffix(Rest, ")", Name)
        do
            assert(td(Name, MacroDir))
        end foreach,
        file::saveUtf8(WSVFile, ideVars),
        retractFactDb(ideVars).

class predicates
    insertNewVirtualDir : (xmlElement VirtDirGroup,string VirtualName,string VirtualDir).
clauses
    insertNewVirtualDir(VirtDirGroup, VirtualName, VirtualDir):-
        VirtDir = xmlElement::new("",virtDir_C, VirtDirGroup),
        VirtDir:addAttribute(virtDirName_C, VirtualName),
        VirtDir:addAttribute(path_C, VirtualDir),
        VirtDirGroup:addNode(VirtDir).

clauses
    updateVirtualDir(Name, NewDirValue):-
        VirtualName = string::format("$(%s)", Name),
        retract(virtualDir(VirtualName, _, FlagVip, IdeVarsFile)),
        assert(virtualDir(VirtualName, NewDirValue, FlagVip, IdeVarsFile)),
        !,
        if
            IdeVarsFile = "",
            VirtDir = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(OG) :- OG:attribute(virtDirName_C) = VirtualName})]),!
        then
            VirtDir:modifyAttribute(path_C, NewDirValue),
            saveWSMOptions()
        else
            updateWSVFile(IdeVarsFile)
        end if.
    updateVirtualDir(Name, _):-
        exception::raise_User(string::format("Unknown Name: %s", Name)).

clauses
    deleteVirtualDir(Name):-
        VirtualName = string::format("$(%s)", Name),
        retract(virtualDir(VirtualName, _, _, IdeVarsFile)),
        !,
        if
            IdeVarsFile = "",
            VirtDir = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(OG) :- OG:attribute(virtDirName_C) = VirtualName})]),!
        then
            convert(xmlElement,VirtDir:parent_P):removeNode(VirtDir),
            saveWSMOptions()
        else
            updateWSVFile(IdeVarsFile)
        end if.
    deleteVirtualDir(Name):-
        exception::raise_User(string::format("Unknown Name: %s", Name)).

% Perform Options
clauses
    getExtOptions_nd(ExtName, ExtList):-
        xml_ExtOptions(ExtName, ExtList, _).

clauses
    getSelectSourceType() = SelectSourceType :-
        Node = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "PerformExt" })]),
        SelectSourceType = Node:attribute(selectSourceType_C),
        !.
    getSelectSourceType() = "".

clauses
    updateSelectSourceType([namedValue("select",string(SelectSourceType))]):-
        Node = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "PerformExt" })]),
        !,
        if _ = Node:attribute(selectSourceType_C) then
            Node:modifyAttribute(selectSourceType_C, SelectSourceType)
        else
            Node:addAttribute(selectSourceType_C, SelectSourceType)
        end if.
    updateSelectSourceType(_).

clauses
    getCompRun_nd(CompName, Synname):-
        Node = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "CompRun" }),
            child(componentName_C, {(_)})]),
        CompName = Node:attribute(name_C),
        Synname = Node:attribute(synname_C).

clauses
    loadOptionsByExt(Ext) = OptionsValues :-
        if
            xml_ExtOptions(ExtName, ExtList, ExtItem),
            list::isMemberEq(string::equalIgnoreCase, Ext, ExtList),
            !,
            ExtListAsStr = ExtItem:attribute(extName_C),
            ComponentName = if CN = ExtItem:attribute(componentName_C) then CN else "" end if
        then
            OptionsValues =
                [tuple("common", [namedValue("ext", string(ExtListAsStr)),namedValue(componentName_C, string(ComponentName))])|
                [tuple(Node:name_P, FullAttributesList)||
                Node = xml_Options:getNode_nd([current(ExtItem), child("*", { (_) })]),
                AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()],
                if _ = Node:attribute(keyWords_C) then
                    FullAttributesList = AttributesList
                elseif ExtName = "Vip Project" and Node:attribute(name_C) in ["Build","Rebuild"] then
                    KeyWordsList = [namedValue("Error", string(toString([error_C, fatalError_C, registerCompError_C]))),
                                            namedValue("Warning", string(toString([warning_C]))),
                                            namedValue("LinkError", string(toString([callFailed_C, notBuilt_C])))],
                    FullAttributesList = list::append(AttributesList,[namedValue(keyWords_C, string(toString(KeyWordsList)))])
                else
                    KeyWordsList = [namedValue("Error", string(toString([]))),
                                            namedValue("Warning", string(toString([])))],
                    FullAttributesList = list::append(AttributesList,[namedValue(keyWords_C, string(toString(KeyWordsList)))])
                end if
                ]]
        else
            OptionsValues = []
        end if.

clauses
    loadOptionsBySource(NodeId) = FileOptionsList :-
        SourceNode = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = NodeId })]),
        removeOldLocalOptions(SourceNode),
        !,
        FileOptionsList = [tuple(Node:attribute(index_C), AttributesList)||
            Node = wsBE_XmlDocument():getNode_nd([current(SourceNode), child(commandL_C, { (_) })]),
            AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]
            ].
    loadOptionsBySource(_NodeId) = [].

predicates
    removeOldLocalOptions : (xmlElement SourceNode).
clauses
    removeOldLocalOptions(SourceNode):-
        foreach Node = wsBE_XmlDocument():getNode_nd([current(SourceNode), child(command_C, { (_) })]) do
            SourceNode:removeNode(Node),
            wSBE_Tasks():saveWorkSpace()
        end foreach.

clauses
    updateSourceLocalOptions([namedValue(SourceID, string(LocalOptionsStr))]):-
        ExtOptionsList = toTerm(extOptions, LocalOptionsStr),
        SourceNode = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = SourceID })]),
        !,
        foreach tuple(Index, AttributesList) in ExtOptionsList do
            if ArgNode = wsBE_XmlDocument():getNode_nd([current(SourceNode), child(commandL_C, { (CN) :- Index = CN:attribute(index_C) })]) then
            else
                ArgNode = xmlElement::new("", commandL_C, SourceNode),
                ArgNode:addAttribute(index_C, Index),
                SourceNode:addNode(ArgNode)
            end if,
            foreach namedValue(AttributeName, string(NewValue)) in AttributesList do
                if _ = ArgNode:attribute(AttributeName) then
                    ArgNode:modifyAttribute(AttributeName, NewValue)
                else
                    ArgNode:addAttribute(AttributeName, NewValue)
                end if
            end foreach,
            foreach
                tuple(_, AttrNameD, _) = ArgNode:getAttribute_nd(),
                AttrNameD <> index_C,
                not(namedValue(AttrNameD, _) in AttributesList)
            do
                ArgNode:removeAttribute(AttrNameD)
            end foreach
        end foreach,
        wSBE_Tasks():saveWorkSpace().
    updateSourceLocalOptions(_).

clauses
    updateOptionsByExt(NameExt, ExtOptionsStr, _TaskQueueObj):-
        ExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        if xml_ExtOptions(NameExt, _, ExtGroup) then
            foreach
                CmdNode = xml_Options:getNode_nd([current(ExtGroup), child(command_C, {(_)})])
            do
                ExtGroup:removeNode(CmdNode)
            end foreach,
            foreach
                tuple(NodeName, AttributesList) in ExtOptionsList
            do
                updateOptions(ExtGroup, NodeName, AttributesList)
            end foreach,
%            updateOptionsNotifyFE(TaskQueueObj), BB! TODO
            saveWSMOptions()
        end if.

class predicates
    updateOptions : (xmlElement ExtGroup,string NodeName,namedValue* AttributesList).
clauses
    updateOptions(ExtGroup, "common", AttributesList):-
        !,
        if ComponentName = namedValue::tryGetNamed_string(AttributesList, componentName_C) then
            if _ = ExtGroup:attribute(componentName_C) then
                if ComponentName <> "" then
                    ExtGroup:modifyAttribute(componentName_C, ComponentName)
                else
                    ExtGroup:removeAttribute(componentName_C)
                end if
            elseif ComponentName <> "" then
                ExtGroup:addAttribute(componentName_C, ComponentName)
            end if
        end if.
    updateOptions(ExtGroup, NodeName, AttributesList):-
        CmdIndex = namedValue::tryGetNamed_string(AttributesList, index_C),
        !,
        CmdNode = xmlElement::new("", NodeName, ExtGroup),
        CmdNode:addAttribute(index_C, CmdIndex),
        ExtGroup:addNode(CmdNode),
        foreach namedValue(AttributeName, string(NewValue)) in AttributesList do
            if _ = CmdNode:attribute(AttributeName) then
                if NewValue <> "" then
                    CmdNode:modifyAttribute(AttributeName, NewValue)
                else
                    CmdNode:removeAttribute(AttributeName)
                end if
            elseif NewValue <> "" then
                CmdNode:addAttribute(AttributeName, NewValue)
            end if
        end foreach.
    updateOptions(_ExtGroup, _NodeName, _AttributesList).

clauses
    updateOptionsNotifyFE(TaskQueueObj):-
        NameValues = [NameValue||NameValue = getUpdateOptions_nd()],
        wSBE_Tasks():updateOptionsNotifyFE(NameValues,TaskQueueObj).

predicates
    getUpdateOptions_nd : () -> namedValue multi.
clauses
    getUpdateOptions_nd() = namedValue(execOn_C, string(ExecOnValue)) :-
        ExecOnValue = toString(
            [tuple(ExtName, ExtList, Enabled) ||
                xml_ExtOptions(ExtName, _, ExtItem),
                ExtList = ExtItem:attribute(extName_C),
                if ExtItem1 = xml_Options:getNode_nd([current(ExtItem),child(sourceExecute_C, {(_)})]) then % вот тут что надо переделать!!!
                    Enabled = ExtItem1:attribute(execOn_C)
                else
                    Enabled = "false"
                end if
           ]
        ).
    getUpdateOptions_nd() = namedValue("", string("")).

clauses
    updateExtOptionsList(NewExtOptionsList):-
        if ExtGroup = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = "PerformExt" })]) then
            OldExtOptionsList = [ namedValue(Name, string(ExtListAsStr)) || xml_ExtOptions(Name, _ExtList, ExtItem), ExtListAsStr = ExtItem:attribute(extName_C)],
            foreach namedValue(NewExtName, string(NewExtListAsStr)) in list::differenceEq(existExtName, NewExtOptionsList, OldExtOptionsList) do
                ExtItem = xmlElement::new("", source_C,ExtGroup),
                ExtItem:addAttribute(name_C, NewExtName),
                ExtItem:addAttribute(extName_C, NewExtListAsStr),
                ExtList = splitExtListStr(NewExtListAsStr),
                assert(xml_ExtOptions(NewExtName, ExtList, ExtItem)),
                ExtGroup:addNode(ExtItem)
            end foreach,
            foreach namedValue(DelName, string(_)) in list::differenceEq(existExtName, OldExtOptionsList, NewExtOptionsList),
                        xml_ExtOptions(DelName, _, DelItem) do
                ExtGroup:removeNode(DelItem),
                retractAll(xml_ExtOptions(DelName, _, DelItem))
            end foreach,
            foreach namedValue(Name, string(ExtListAsStr)) in NewExtOptionsList do
                if xml_ExtOptions(Name, _, ModifyItem) then
                    ModifyItem:modifyAttribute(extName_C, ExtListAsStr),
                    MExtList = splitExtListStr(ExtListAsStr),
                    retractall(xml_ExtOptions(Name, _, ModifyItem)),
                    assert(xml_ExtOptions(Name, MExtList, ModifyItem))
                end if
            end foreach,
            saveWSMOptions()
        end if.

class predicates
    existExtName : predicate_dt{namedValue, namedValue}.
clauses
    existExtName(namedValue(Ext, _), namedValue(Ext, _)).

% FrontEnd Options
clauses
    getFrontEndOptionsList() =
        [namedValue(Node:name_P, string(Node:attribute(path_C)))
        ||
        Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) in [wsFE_Tasks_C,wsFE_Form_C] }), child("*", {(_)})])
        ].

clauses
    setFrontEndOptions(FrontEndOptions):-
        foreach
            namedValue(Name, string(Value)) in FrontEndOptions
        do
            if
                Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) in [wsFE_Tasks_C,wsFE_Form_C] }), child(Name, {(_)})])
                then
                    Node:modifyAttribute(path_C, Value)
            elseif
                Name in [nodeIDList_C,ribbonLayout_C,checkedFilter_C],
                FormNode = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = wsFE_Form_C })])
                then
                    ChItem = xmlElement::new("", Name,FormNode),
                    ChItem:addAttribute(path_C, Value),
                    FormNode:addNode(ChItem)
            elseif
                TaskNode = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = wsFE_Tasks_C })])
                then
                    ChItem = xmlElement::new("", Name,TaskNode),
                    ChItem:addAttribute(path_C, Value),
                    TaskNode:addNode(ChItem)
            end if
        end foreach,
        saveWSMOptions().

clauses
    getSourceColor_nd(Name, FGColor, BGColor):-
        if
            Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- wSFE_SourceList_C = O:attribute(title_C) }), child("color", {(_)})]),
            ColorValueStr = Node:attribute(textColor_C),
            ColorList = toTerm(namedValue_list, ColorValueStr),
            3 = list::length(ColorList)
        then
            Name = list::getMember_nd([groupSource_C, folderSource_C, excludedSource_C]),
                namedValue(Name, string(ColorsStr)) = list::tryGetMemberEq(colorByName, Name, ColorList),
                tuple(FGColor, BGColor) = toTerm(tuple{integer, integer}, ColorsStr)
        else
            tuple(Name, FGColor, BGColor) in
                [
                tuple(groupSource_C, vpiDomains::color_Black, vpiDomains::color_White),
                tuple(folderSource_C, vpiDomains::color_Gray60, vpiDomains::color_White),
                tuple(excludedSource_C, vpiDomains::color_GrayC0, vpiDomains::color_White)
                ]
        end if.

class predicates
    colorByName : predicate_dt{string, namedValue}.
clauses
    colorByName(Name, namedValue(Name, _)).

clauses
    updateSourceColors(SourceColorsList):-
        if
            Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- wSFE_SourceList_C = O:attribute(title_C) }), child("color", {(_)})])
        then
            Node:modifyAttribute(textColor_C, toString(SourceColorsList)),
            saveWSMOptions()
        end if.

clauses
    updateUILanguage(NewUILanguage):-
        NewUILanguage = [namedValue("lang", string(NewUICode))],
        if
            Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- wsFE_Form_C = O:attribute(title_C) }), child("lang", {(_)})])
        then
            Node:modifyAttribute("cur", NewUICode)
        else
            FormNode = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = wsFE_Form_C })]),
            ChItem = xmlElement::new("","lang", FormNode),
            ChItem:addAttribute("cur", NewUICode),
            FormNode:addNode(ChItem)
        end if,
        ws_Events():setCurrentLng(NewUICode),
        !,
        saveWSMOptions().
    updateUILanguage(_).

end implement wSBE_Options
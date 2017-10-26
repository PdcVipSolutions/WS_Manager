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
    virtualDir : (string Name,string DirValue,boolean FlagVip).

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
    optionsWSMFile_C = @"OptionsWSM.xml".

predicates
    loadOptionsWSM : ().
clauses
    loadOptionsWSM():-
        if file::existExactFile(optionsWSMFile_C) then
            XmlOptions = inputStream_file::openFile(optionsWSMFile_C, stream::binary),
            spbXmlLigntSupport::read(XmlOptions, xml_Options),
            XmlOptions:close(),
            loadVirtualDir(),
            loadPerformOptions(),
            loadCurrentLanguage()
        else
            setDefaultOptionsWSM()
        end if.

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
        foreach Node = xml_Options:getNode_nd(
            [root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(_)})]),
            VirtualName = Node:attribute(virtDirName_C), VirtualDir = Node:attribute(path_C)
        do
            assert(virtualDir(VirtualName, VirtualDir, false))
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
            asserta(virtualDir(VirtualName, VirtualDir, true))
        end foreach.

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
    splitExtListStr(ExtListStr) = [string::trim(Ext)|RestList] :-
        string::splitStringBySeparators(ExtListStr, ",;", Ext, _, RestStr),
        !,
        RestList = splitExtListStr(RestStr).
    splitExtListStr(ExtListStr) = Rest :-
        if "" = string::trim(ExtListStr) then
            Rest = []
        else
            Rest = [string::trim(ExtListStr)]
        end if.

predicates
    setDefaultOptionsWSM : ().
clauses
    setDefaultOptionsWSM():-
        loadVirtualDirVIP(),
% Create <be_options> Node
        BEOptions = xmlElement::new("", backEndOptions_C,xml_Options:root_P),
%        BEOptions:name_P := backEndOptions_C,
        xml_Options:root_P:addNode(BEOptions),
% Create <group title="VirtualDir"> Node
        VirtDirGroup = xmlElement::new("", groupNode_C,BEOptions),
%        VirtDirGroup:name_P := groupNode_C,
        VirtDirGroup:addAttribute(title_C, "VirtualDir"),
        BEOptions:addNode(VirtDirGroup),
% Create <group title="PerformExt"> Node
        ExtGroup = xmlElement::new("", groupNode_C,BEOptions),
%        ExtGroup:name_P := groupNode_C,
        ExtGroup:addAttribute(title_C, "PerformExt"),
        BEOptions:addNode(ExtGroup),
% Create <source ext="vipprj" name="Vip Project"> Node
        VipGroup = xmlElement::new("",source_C, ExtGroup),
%        VipGroup:name_P := source_C,
        VipGroup:addAttribute(extName_C, "vipprj"),
        VipGroup:addAttribute(name_C, "Vip Project"),
        ExtGroup:addNode(VipGroup),
        createCommandNodes(VipGroup),
% Create <source ext="cmd" name="Command File"> Node
        CmdGroup = xmlElement::new("",source_C, ExtGroup),
%        CmdGroup:name_P := source_C,
        CmdGroup:addAttribute(extName_C, "cmd"),
        CmdGroup:addAttribute(name_C, "Command File"),
        ExtGroup:addNode(CmdGroup),
        createCommandNodes(CmdGroup),
        loadPerformOptions(),
% Create <fe_options> Node
        FEOptions = xmlElement::new("", frontEndOptions_C,xml_Options:root_P),
%        FEOptions:name_P := frontEndOptions_C,
        xml_Options:root_P:addNode(FEOptions),
% Create <group title="wsFE_Tasks"> Node
        TaskGroup = xmlElement::new("", groupNode_C,FEOptions),
%        TaskGroup:name_P := groupNode_C,
        TaskGroup:addAttribute(title_C, "wsFE_Tasks"),
        FEOptions:addNode(TaskGroup),
% Create <group title="wsFE_Form"> Node
        FormGroup = xmlElement::new("", groupNode_C,FEOptions),
%        FormGroup:name_P := groupNode_C,
        FormGroup:addAttribute(title_C, "wsFE_Form"),
        FEOptions:addNode(FormGroup),
% Create <group title="wSFE_SourceList_C"> Node
        SrcListGroup = xmlElement::new("", groupNode_C,FEOptions),
%        SrcListGroup:name_P := groupNode_C,
        SrcListGroup:addAttribute(title_C, wSFE_SourceList_C),
        FEOptions:addNode(SrcListGroup),
        ColorList = [namedValue(Name, string(toString(tuple(FGColor, BGColor)))) || getSourceColor_nd(Name, FGColor, BGColor)],
        ColorItem = xmlElement::new("", "color",SrcListGroup),
%        ColorItem:name_P := "color",
        ColorItem:addAttribute(textColor_C, toString(ColorList)),
        SrcListGroup:addNode(ColorItem),
        saveWSMOptions().

predicates
    createCommandNodes : (xmlElement ExtGroup).
clauses
    createCommandNodes(ExtGroup):-
        OpenNode = xmlElement::new("", sourceEditor_C,ExtGroup),
%        OpenNode:name_P := sourceEditor_C,
        ExtGroup:addNode(OpenNode),
        RunNode = xmlElement::new("", sourcePerformer_C,ExtGroup),
%        RunNode:name_P := sourcePerformer_C,
        ExtGroup:addNode(RunNode),
        ExecNode = xmlElement::new("", sourceExecute_C,ExtGroup),
%        ExecNode:name_P := sourceExecute_C,
        ExtGroup:addNode(ExecNode).

% Virtual Directory Options
clauses
    getVirtualDir_nd(Name, DirValue, FlagVip):-
        virtualDir(Name, DirValue, FlagVip).

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
                    [tuple(VN, VD)||virtualDir(VN, VD, _IsVipDir)], descending)
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
        virtualDir(VirtualName, Dir, _),
        string::hasPrefixIgnoreCase(Full, VirtualName, Rest),
        !,
        RestPath = if string::frontChar(Rest, First, Last) and First = '\\' then Last else Rest end if,
        CurrentDirectory = directory::getCurrentDirectory(),
        FullDir = fileName::createPath(CurrentDirectory, Dir),
        FullUp = fileName::createPath(FullDir, RestPath).
    changeMacro(Full) = Full.

clauses
    existVirtualName(Name):-
        virtualDir(VirtName, _, _),
        equal = string::compareIgnoreCase(Name, VirtName),
        !.

clauses
    insertVirtualDir(Name, NewDirValue):-
        if VirtDirGroup = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (OG) :- OG:attribute(title_C) = "VirtualDir" })]), ! then
            VirtualName = string::format("$(%s)", Name),
            assert(virtualDir(VirtualName, NewDirValue, false)),
            insertNewVirtualDir(VirtDirGroup, VirtualName, NewDirValue),
            saveWSMOptions()
        end if.

predicates
    insertNewVirtualDir : (xmlElement VirtDirGroup,string VirtualName,string VirtualDir).
clauses
    insertNewVirtualDir(VirtDirGroup, VirtualName, VirtualDir):-
        VirtDir = xmlElement::new("",virtDir_C, VirtDirGroup),
%        VirtDir:name_P := virtDir_C,
        VirtDir:addAttribute(virtDirName_C, VirtualName),
        VirtDir:addAttribute(path_C, VirtualDir),
        VirtDirGroup:addNode(VirtDir).

clauses
    updateVirtualDir(Name, NewDirValue):-
        VirtualName = string::format("$(%s)", Name),
        retract(virtualDir(VirtualName, _, FlagVip)),
        VirtDir = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(OG) :- OG:attribute(virtDirName_C) = VirtualName})]),
        !,
        assert(virtualDir(VirtualName, NewDirValue, FlagVip)),
        VirtDir:modifyAttribute(path_C, NewDirValue),
        saveWSMOptions().
    updateVirtualDir(Name, _):-
        exception::raise_User(string::format("Unknown Name: %s", Name)).

clauses
    deleteVirtualDir(Name):-
        VirtualName = string::format("$(%s)", Name),
        VirtDir = xml_Options:getNode_nd([root(), child(backEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = "VirtualDir" }), child(virtDir_C, {(OG) :- OG:attribute(virtDirName_C) = VirtualName})]),
        !,
        retractAll(virtualDir(VirtualName, _, _)),
        convert(xmlElement,VirtDir:parent_P):removeNode(VirtDir),
        saveWSMOptions().
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
    loadOptionsByExt(Ext) = OptionsValues :-
        if
            xml_ExtOptions(_ExtName, ExtList, ExtItem),
            list::isMemberEq(string::equalIgnoreCase, Ext, ExtList),
            !,
            ExtListAsStr = ExtItem:attribute(extName_C)
        then
            OptionsValues =
                [tuple("common", [namedValue("ext", string(ExtListAsStr))])|
                [tuple(Node:name_P, AttributesList)||
                Node = xml_Options:getNode_nd([current(ExtItem), child("*", { (_) })]),
                AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]]]
        else
            OptionsValues = []
        end if.

clauses
    loadOptionsBySource(NodeId) = FileOptionsList :-
        SourceNode = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = NodeId })]),
%        FileName = SourceNode:tryGetAttribute(fileName_C),
        !,
        FileOptionsList = [tuple(Node:name_P, AttributesList)||
            Node = wsBE_XmlDocument():getNode_nd([current(SourceNode), child("*", { (_) })]),
            AttributesList = [namedValue(AttrName, string(AttrValue))||tuple(_AttrPrefix, AttrName, AttrValue) = Node:getAttribute_nd()]].
    loadOptionsBySource(_NodeId) = [].

clauses
    updateSourceLocalOptions([namedValue(SourceID, string(LocalOptionsStr))]):-
        ExtOptionsList = toTerm(extOptions, LocalOptionsStr),
        SourceNode = wsBE_XmlDocument():getNode_nd([root(), self({ (_) }), descendant("*", { (O) :- toString(O) = SourceID })]),
        !,
        foreach tuple(NodeName, AttributesList) in ExtOptionsList do
            if ArgNode = wsBE_XmlDocument():getNode_nd([current(SourceNode), child(NodeName, { (_) })]) then
            else
                ArgNode = xmlElement::new("", NodeName,SourceNode),
%                ArgNode:name_P := NodeName,
                SourceNode:addNode(ArgNode)
            end if,
            foreach namedValue(AttributeName, string(NewValue)) in AttributesList do
                if _ = ArgNode:attribute(AttributeName) then
                    ArgNode:modifyAttribute(AttributeName, NewValue)
                else
                    ArgNode:addAttribute(AttributeName, NewValue)
                end if
            end foreach
        end foreach,
        wSBE_Tasks():saveWorkSpace().
    updateSourceLocalOptions(_).

clauses
    updateOptionsByExt(NameExt, ExtOptionsStr,TaskQueueObj):-
        ExtOptionsList = toTerm(extOptions, ExtOptionsStr),
        if xml_ExtOptions(NameExt, _, ExtGroup) then
            foreach tuple(NodeName, AttributesList) in ExtOptionsList do
                if CmdNode = xml_Options:getNode_nd([current(ExtGroup), child(NodeName, { (_) })]) then
                else
                    CmdNode = xmlElement::new("", NodeName,ExtGroup),
%                    CmdNode:name_P := NodeName,
                    ExtGroup:addNode(CmdNode)
                end if,
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
                end foreach
            end foreach,
            updateOptionsNotifyFE(TaskQueueObj),
            saveWSMOptions()
        end if.

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
                if ExtItem1 = xml_Options:getNode_nd([current(ExtItem),child(sourceExecute_C, {(_)})]) then
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
%                ExtItem:name_P := source_C,
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

predicates
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
%                ChItem:name_P := Name,
                ChItem:addAttribute(path_C, Value),
                FormNode:addNode(ChItem)
            elseif
                TaskNode = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- O:attribute(title_C) = wsFE_Tasks_C })])
            then
                ChItem = xmlElement::new("", Name,TaskNode),
%                ChItem:name_P := Name,
                ChItem:addAttribute(path_C, Value),
                TaskNode:addNode(ChItem)
            end if
        end foreach,
        saveWSMOptions().

clauses
    getSourceColor_nd(Name, FGColor, BGColor):-
        if
            Node = xml_Options:getNode_nd([root(), child(frontEndOptions_C, {(_)}), child(groupNode_C, { (O) :- wSFE_SourceList_C = O:attribute(title_C) }), child("color", {(_)})])
        then
            ColorValueStr = Node:attribute(textColor_C),
            ColorList = toTerm(namedValue_list, ColorValueStr),
            Name = list::getMember_nd([groupSource_C, folderSource_C, excludedSource_C]),
                namedValue(Name, string(ColorsStr)) = list::tryGetMemberEq(colorByName, Name, ColorList),
                tuple(FGColor, BGColor) = toTerm(tuple{integer, integer}, ColorsStr)
        else
            tuple(Name, FGColor, BGColor) in
                [
                tuple("Group Source", vpiDomains::color_Black, vpiDomains::color_White),
                tuple("Folder Source", vpiDomains::color_Gray60, vpiDomains::color_White),
                tuple("Excluded Source", vpiDomains::color_GrayC0, vpiDomains::color_White)
                ]
        end if.

predicates
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
%            ChItem:name_P := "lang",
            ChItem:addAttribute("cur", NewUICode),
            FormNode:addNode(ChItem)
        end if,
        ws_Events():setCurrentLng(NewUICode),
        !,
        saveWSMOptions().
    updateUILanguage(_).

end implement wSBE_Options
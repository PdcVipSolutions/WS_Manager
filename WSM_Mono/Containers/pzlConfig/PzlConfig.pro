﻿/*****************************************************************************
                                        Visual Prolog PuZzLe Studio
                        Copyright (c) 2003-2004, Prolog Development Center Spb Ltd.

Created by: Visual Prolog PuZzLe Studio

Container: Copyright (c) 2006-2016, PDCSPB
Used by: Виктор Юхтенко/WIN-5L3MH3V6R7Q
******************************************************************************/
implement pzlConfig

open core, pfc\log

clauses
%    init().
    init():-
        vpi::init().


clauses
    getContainerVersion()=iPzlConfig::pzlContainerVersion_C.

clauses
    getComponentIDInfo()=
        [
		iWS_Manager::componentDescriptor_C
        ].

% put here ONLY clauses related to the visible pzlComponents implemented  into this pzlContainer.
clauses
	new(iWS_Manager::componentID_C,Container)=_Object:-
        pzl::log():write(log::info,"WSM> started [", toString(Container),"]"),
        fail.
	new(iWS_Manager::componentID_C,Container)=Object:-
		!,
		Object = wS_Manager::new(Container).
    new(ClassUID,_Container)=pzlDomains::nullObject_C:-
        if not(ClassUID=pzlDomains::str("DummyX")) then
            MSG=string::format("The pzlComponent \"%\" is not supported in the pzlContainer \"%\", ver. \"%\"",ClassUID,pzl::getContainerName(),getContainerVersion()),
            exception::raise_User(Msg)
        end if.

end implement pzlConfig

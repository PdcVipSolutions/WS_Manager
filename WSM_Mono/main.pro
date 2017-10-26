/*****************************************************************************
Copyright © Prolog Development Center SPB
******************************************************************************/
implement main

clauses
    run() :-
        vpi::init(),
        WSM=workSpaceManager::new(),
        WSM:run(gui::getScreenWindow()),
        messageLoop::run().

end implement main

goal
    mainExe::run(main::run).
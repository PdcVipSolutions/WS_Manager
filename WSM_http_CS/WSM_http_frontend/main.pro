% Copyright © Prolog Development Center SPb

implement main
    open core

clauses
    run():-
        vpi::init(),
        Service=workSpaceManager::new(),
        Service:run().

end implement main

goal
    mainExe::run(main::run,exe_api::multiThread).
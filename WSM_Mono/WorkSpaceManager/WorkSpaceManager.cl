﻿%

class workSpaceManager : workSpaceManager
    open core

constants
    isBackEnd_app : boolean = true.
    isMonoApplicaion : boolean = true.
    isHttpService : boolean = false [compileTimeSetting].
    isHttpClient :boolean = false [compileTimeSetting].

end class workSpaceManager
#requires @"ws_manager\common\AppFrontEnd\WS_FrontEnd\WS_FrontEnd.pack"

% publicly used packages
#include @"ws_manager\common\http_Client\http_Client.ph"
#include @"WorkSpaceManager\WorkSpaceManager.ph"

#include @"ws_manager\common\AppFrontEnd\WSFE_Tasks\WSFE_Tasks.ph"

#include @"pfc\web\http\httpServerApi\requestQueue_rpc\requestQueue_rpc.ph"
#include @"Packs\Logic\EntityRegistry\EntityRegistry.ph"
#include @"ws_manager\common\WS_EventManager\WS_EventManager.ph"
#include @"pfc\core.ph"

% exported interfaces
#include @"ws_manager\common\AppFrontEnd\WS_FrontEnd\WS_FrontEnd.i"

% exported classes
#include @"ws_manager\common\AppFrontEnd\WS_FrontEnd\WS_FrontEnd.cl"

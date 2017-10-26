#requires @"ws_manager\common\AppBackEnd\WSBE_Connector\WSBE_Connector.pack"
% publicly used packages
#include @"pfc\multiThread\monitorQueue\monitorQueue.ph"
#include @"Packs\Logic\Web\tsMapM_redBlack\tsMapM_redBlack.ph"
#include @"pfc\multiThread\multiThread.ph"
#include @"ws_manager\common\AppBackEnd\WSBE_Options\WSBE_Options.ph"
#include @"ws_manager\common\AppBackEnd\WS_BackEnd\WS_BackEnd.ph"
#include @"ws_manager\common\AppBackEnd\WSFE_Messages\WSFE_Messages.ph"
#include @"ws_manager\common\AppBackEnd\WSBE_Tasks\WsBE_Tasks.ph"
#include @"ws_manager\common\AppBackEnd\WSBE_Performer\WSBE_Performer.ph"

#include @"pfc\core.ph"

% exported interfaces
#include @"ws_manager\common\AppBackEnd\WSBE_Connector\WSBE_Connector.i"

% exported classes
#include @"ws_manager\common\AppBackEnd\WSBE_Connector\WSBE_Connector.cl"

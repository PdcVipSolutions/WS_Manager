#requires @"ws_manager\common\AppFrontEnd\WSFE_Command\WSFE_Command.pack"
% publicly used packages
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\WSFE_Command.ph"
#include @"ws_manager\common\AppFrontEnd\WSFE_SourceTree\WSFE_SourceTree.ph"
#include @"pfc\gui\controls\ribbonControl\ribbonControl.ph"
#include @"ws_manager\common\AppFrontEnd\WSFE_Form\WSFE_Form.ph"
#include @"pfc\gui\gui.ph"
#include @"pfc\gui\commands\menuCommand\menuCommand.ph"
#include @"pfc\gui\commands\command\Command.ph"
#include @"pfc\core.ph"
#include @"Interfaces\Gui\CommandBlock.i"

% exported interfaces
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\MoveRemoveCommand.i"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\WSFE_Command.i"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\PerformSourceCommand.i"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\SourceContentCommand.i"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\WorkSpaceCommand.i"

% exported classes
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\MoveRemoveCommand.cl"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\WSFE_Command.cl"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\PerformSourceCommand.cl"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\SourceContentCommand.cl"
#include @"ws_manager\common\AppFrontEnd\WSFE_Command\WorkSpaceCommand.cl"


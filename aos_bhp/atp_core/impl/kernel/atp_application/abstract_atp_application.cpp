/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc implemented.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-22    lantback    Created
* 2016-04-26    lantback    Added Decode and TSetup
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-06-02    arastogi    Fixed to include core hpp files instead of adaptation
* 2016-07-06    adgupta     Added functions to return iterators of component list
* 2016-07-26    adgupta     Added console related calls
* 2016-08-10    akushwah    Added log handler related calls
* 2016-09-06    arastogi    Added btm handler related calls
* 2016-09-14    akushwah    Added DMI handler related calls
* 2016-09-30    bhidaji     Added target_calculation
* 2016-10-03    arastogi    Fixed the order in which components are run.
* 2016-10-05    bhidaji     Added supervise
* 2016-12-06    rquensel    Added call to Cross Compare
* 2016-12-15    saprasad    Added init and run call for Analyzer IF
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_atp_application.hpp"
#include "abstract_odometry.hpp"
#include "abstract_tracks.hpp"
#include "abstract_basic_ip.hpp"
#include "abstract_brake.hpp"
#include "abstract_btm_handler.hpp"
#include "abstract_config_base.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_loco_io.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_target_calculation.hpp"
#include "abstract_supervise.hpp"
#include "abstract_analyzer_if.hpp"
#include "abstract_vehicle_com.hpp"
#include "abstract_tims.hpp"
#include "abstract_tic.hpp"
#include <vfw_checkpoints.h>

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractATPApplication::AbstractATPApplication() : AbstractApplicationBase(ATC::ATPBlock)
    {

    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractATPApplication::init(void)
    {
      return true;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractATPApplication::run(void)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "AP_run_begin");

      // Manage TCC radio communication
      RadioCom::AbstractRadioHandler::corePtr()->run();

      // Read I/O devices
      IO::AbstractLocoIO::corePtr()->runIn();
      Kernel::AbstractMessageHandler::corePtr()->runIn();
      IO::AbstractBTMHandler::corePtr()->runIn();
      DMICom::AbstractDMIHandler::corePtr()->runIn();
      TG::AbstractVehicleCom::corePtr()->runIn();
      TG::AbstractTIMS::corePtr()->runIn();

      // Cross Compare Input
      Support::AbstractCrossCompare::corePtr()->runIn();

      // Process main loop
      Pos::AbstractOdometry::corePtr()->run();
      Pos::AbstractDecode::corePtr()->run();
      Pos::AbstractPosition::corePtr()->run();
      Kernel::AbstractModeControl::corePtr()->run();
      Supv::AbstractTargetCalculation::corePtr()->run();
      Supv::AbstractSupervise::corePtr()->run();
      Supv::AbstractBrake::corePtr()->run();
      TG::AbstractTIMS::corePtr()->run();
      TG::AbstractTIC::corePtr()->run();

      // Write I/O devices
      Kernel::AbstractMessageHandler::corePtr()->runOut();
      IO::AbstractLocoIO::corePtr()->runOut();
      IO::AbstractBTMHandler::corePtr()->runOut();
      DMICom::AbstractDMIHandler::corePtr()->runOut();
      TG::AbstractVehicleCom::corePtr()->runOut();
      TG::AbstractTIMS::corePtr()->runOut();

      // Execution of background processes
      DS::AbstractTracks::corePtr()->run();
      DS::AbstractTargets::corePtr()->run();
      DS::AbstractTSetup::corePtr()->run();
      ATC::AbstractEventHandler::corePtr()->run();

      ATC::AbstractLogHandler::corePtr()->run();
      ATC::AbstractBasicIP::corePtr()->run();
      ATC::AbstractConsole::corePtr()->run();
      ATC::AbstractAnalyzerIF::corePtr()->run();
      ATC::AbstractConfigBase::basePtr()->run();

      // Cross Compare
      Support::AbstractCrossCompare::corePtr()->runOut();

      vfwVisitCheckPoint(&endCp, "AP_run_end");
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractATPApplication* AbstractATPApplication::corePtr(void)
    {
      //lint -e{929} -e{1774} Cast is ok, since this is an AbstractATPApplication
      return static_cast<AbstractATPApplication*>(ATC::AbstractApplicationBase::corePtr());
    }

  }
}

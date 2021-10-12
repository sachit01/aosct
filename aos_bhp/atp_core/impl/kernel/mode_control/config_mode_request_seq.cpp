/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the ConfigModeRequestSeq class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-20   spandita      Config mode button sequence
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "config_mode_request_seq.hpp"
#include "abstract_odometry.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_radio_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_dmi_handler.hpp"

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
    ConfigModeRequestSeq::ConfigModeRequestSeq() :ModeRequestSeq()
    {
      seqState = configDmiButtonPressed;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void ConfigModeRequestSeq::run(CommonDataForModes &commonData)
    {
      ModeRequestSeqState oldState = seqState;
      //run the function corresponding to the sequence state.
      switch (seqState)
      {
      case configDmiButtonPressed:
        runConfigButtonPressed();
        break;

      case configDmiButtonConfirmed:
        runConfigDmiButtonConfirmed(commonData);
        break;

      default:
        break;
      }

      //If sequence state has changed
      if (oldState != seqState)
      {
        trace->write
        (2U, "Current DMI Button State:", static_cast<uint32_t>(seqState));
      }
    }


    /******************************************************************************
    * runConfigButtonPressed
    ******************************************************************************/
    void ConfigModeRequestSeq::runConfigButtonPressed()
    {
      bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

      if ((DMICom::DMIButtonTrainConfig == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()) &&
        isStandStill)
      {
        seqState = configDmiButtonConfirmed;
      }
    }

    /******************************************************************************
    * runConfigDmiButtonConfirmed
    ******************************************************************************/
    void ConfigModeRequestSeq::runConfigDmiButtonConfirmed(CommonDataForModes &commonData)
    {
      commonData.modeReqByDMI = ATPModeConfiguration;
      DS::AbstractTSetup::corePtr()->removeTrainSetup();
      commonData.isATPReset = false;

      seqState = configDmiButtonPressed;
    }

    /******************************************************************************
    * validateModes
    ******************************************************************************/
    bool ConfigModeRequestSeq::validateModes()
    {
      bool validMode = false;
      switch (AbstractModeControl::corePtr()->getCurrentMode())
      {
      case ATPModePossession:
      case ATPModeSplit:
      case ATPModeJoin:
      case ATPModeShunting:
      case ATPModeShuntingRoute:
      case ATPModeYard:
      case ATPModeUnregistered:
        validMode = true;
        break;
      case ATPModeSleeping:
        //check here TCC connectivity
        if (!RadioCom::AbstractRadioHandler::corePtr()->getTCCTimeoutStatus())
        {
          validMode = true;
        }
        break;
      case ATPModePowerUp:
        {
          if (Kernel::PowerUpMode::powerUpWaitConfigOrSleep
             == Kernel::AbstractModeControl::corePtr()->getPowerUpModeState())
          {
            validMode = true;
          }
        }
        break;
      case ATPModeConfiguration:
        {
          /* If the configuration sub state is rejected/aborted, set the validity to true,
           which enables visibility of Configuration button on DMI.
           This is an exception for Config requirements..*/
          if (Kernel::TrainConfigMode::trainConfigRejectedOrAborted ==
            Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState())
          {
            validMode = true;
          }
        }
        break;
      case ATPModeRegistration:
      case ATPModeBaliseSearch:
      case ATPModeNormal:
      case ATPModeLocation:
      case ATPModePoweringDown:
      case ATPModeSafetyHalt:
      case ATPModeStaffResponsible:
      case ATPModeSafeBrakeToStop:
        break;
      case ATPModeUndefined:
      case ATPModesCount:
      default:
        ATC::aosHalt(__FILE__, __LINE__, "Illegal Atp Mode");
        break;
      }

      return validMode;
    }
  }
}

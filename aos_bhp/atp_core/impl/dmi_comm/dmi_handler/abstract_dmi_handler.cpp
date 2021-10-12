/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The DMI Handler component deals with managing interaction between ATP and DMI
*  via DMI channels. It also provides other components a way to send required
*  information to the DMI and collect informations/data from the DMI. It creates,
*  manages and owns the DMI channels over which all the communications to DMI
*  takes place.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>
#include "abstract_dmi_handler.hpp"
#include "channel_config.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_console.hpp"
#include "dmi_message_in_vehicle_data.hpp"
#include "dmi_message_in_confirmation.hpp"
#include "dmi_message_in_dmi_startup.hpp"
#include "dmi_message_in_dmi_status.hpp"
#include "dmi_message_in_dmi_to_atp_data.hpp"
#include "dmi_message_in_driver_id_and_password.hpp"
#include "dmi_message_in_loco_vs_train_dir.hpp"
#include "dmi_message_in_train_vs_track_dir.hpp"
#include "dmi_message_in_registration_area.hpp"
#include "dmi_message_in_train_name.hpp"
#include "dmi_message_in_train_loaded.hpp"
#include "dmi_message_out_dmi_startup_history.hpp"
#include "atc_util.hpp"
#include "trace_interface.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
#include "abstract_dmi_handler_event_ids.hpp"

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
  namespace DMICom
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractDMIHandler::AbstractDMIHandler() :ATC::IOComponent(atpDMIHandlerId, "DMIHandler", "DH"),
      // Creating different set of objects for different type of events
      unexpectedDMIMessageType(ATC::Event::createSafeBrakeSBEvent(atpDMIHandlerId, ATC::CoreContainer, eventIdUnexpectedDMIMsg, ATC::NoSB,
        DMICom::dmiUnDefMsg, "Unexpected DMI Message Type:", true)),
      dmiChannel1((vfwGetSide() == VFW_A_SIDE) ? ATC::dmiChannel1DispToATPA : ATC::dmiChannel1DispToATPB,
      (vfwGetSide() == VFW_A_SIDE) ? ATC::dmiChannel1ATPAToDisp : ATC::dmiChannel1ATPBToDisp,
        "DMI1", 1U, "DCH1"),
      dmiChannel2((vfwGetSide() == VFW_A_SIDE) ? ATC::dmiChannel2DispToATPA : ATC::dmiChannel2DispToATPB,
      (vfwGetSide() == VFW_A_SIDE) ? ATC::dmiChannel2ATPAToDisp : ATC::dmiChannel2ATPBToDisp,
        "DMI2", 2U, "DCH2"),
      driverConfirmedTachometer1Failure(ATC::Event::createLogEvent(atpDMIHandlerId, ATC::CoreContainer, eventIdDriverConfirmedTachometer1Failure,DMICom::tachometer1FailureConfirmed,
        "Driver Confirmed Tachometer1 Sensor Failure ", false)),
      driverConfirmedTachometer2Failure(ATC::Event::createLogEvent(atpDMIHandlerId, ATC::CoreContainer, eventIdDriverConfirmedTachometer2Failure, DMICom::tachometer2FailureConfirmed,
        "Driver Confirmed Tachometer2 Sensor Failure ", false)),
      driverConfirmeddopplerFailure(ATC::Event::createLogEvent(atpDMIHandlerId, ATC::CoreContainer, eventIdDriverConfirmedDopplerFailure, DMICom::dopplerFailureConfirmed,
        "Driver Confirmed doppler Sensor Failure ", false))
    {
      if (coreDMIHandlerInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "DMI handler constructor already instantiated");
      }
      // Set up single instance pointer for core access
      coreDMIHandlerInstancePtr = this;

      initDoneChnl1 = false;
      initDoneChnl2 = false;
      messageNumberCounter = 0U;
      cycleCount = 0U;
      dmiCompatibilityVersionAccepted = false;
      activeChannel = static_cast<DMIChannel*>(NULL);
      isDriverConfirmedTachometer1Failure = false;
      isDriverConfirmedTachometer2Failure = false;
      isDriverConfirmedDopplerFailure = false;

      OdoFailureTachometer1 = false;
      OdoFailureTachometer2 = false;
      OdoFailureDoppler = false;
    }

    /******************************************************************************
    * Function: preInit()
    ******************************************************************************/
    void AbstractDMIHandler::preInit(void)
    {
      dmiChannel1.preInit();
      dmiChannel2.preInit();
    }

    /******************************************************************************
    * init
    *-----------------------------------------------------------------------------
    * creates one DMI Channel instance per simultaneously connected DMI
    * Calls the init for the DMI Channels created
    ******************************************************************************/
    bool AbstractDMIHandler::init(void)
    {
      if (!initDoneChnl1)
      {

        // Initialize the DMI Channel for DMI1
        initDoneChnl1 = dmiChannel1.init();
        // Only push it to the container 
        // if it is OK
        if (initDoneChnl1)
        {
          dmiChannels.push_back(&dmiChannel1);

          ATC::AbstractApplicationBase::corePtr()->addComponent(&dmiChannel1);
        }
      }

      if (!initDoneChnl2)
      {
        // Initialize the DMI Channel for DMI2
        initDoneChnl2 = dmiChannel2.init();
        // Only push it to the container 
        // if it is OK
        if (initDoneChnl2)
        {
          dmiChannels.push_back(&dmiChannel2);

          ATC::AbstractApplicationBase::corePtr()->addComponent(&dmiChannel2);
          // Cross Compare
          initCrossCompare();
        }
      }

      return (initDoneChnl1 && initDoneChnl2);
    }


    /******************************************************************************
    * Function: runIn()
    ******************************************************************************/
    void AbstractDMIHandler::runIn(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "DH_runIn");

      std::map<DMIMessageType, AbstractDMIMessageIn*>::iterator mapIter = dmiMessageInParser.begin();
      const std::map<DMIMessageType, AbstractDMIMessageIn*>::iterator endIter = dmiMessageInParser.end();

      // Invalidate the parsers
      while (mapIter != endIter)
      {
        dmiMessageInParser[mapIter->first]->invalidate();
        ++mapIter;
      }

      // Get Active cabin from Mode control component
      CabActiveStatus activeCabinNumber = Kernel::AbstractModeControl::corePtr()->getActiveCab();

      // Active the DMI Channel based on activeCabinNumber
      if ((!Kernel::AbstractModeControl::corePtr()->getStartUpPassed()) ||   // TODO: This is a fix, before startup we don't read the cab signals...
        (activeCabinNumber == CabAActive))
      {
        dmiChannel1.setActive(true);
        dmiChannel2.setActive(false);

        activeChannel = &dmiChannel1;
      }
      else if (activeCabinNumber == CabBActive)
      {
        dmiChannel1.setActive(false);
        dmiChannel2.setActive(true);

        activeChannel = &dmiChannel2;
      }
      else
      {
        dmiChannel1.setActive(false);
        dmiChannel2.setActive(false);

        activeChannel = static_cast<DMIChannel*>(NULL);

        trace.write(ATC::detailedTrace, "DMI Handler:No Cabin is Active");
        writeToLog(ATC::DetailedLog, "DMI Handler:No Cabin is Active", __FILE__, __LINE__);
      }

      dmiChannel1.runIn();
      dmiChannel2.runIn();

      if (activeChannel != static_cast<DMIChannel*>(NULL))
      {
        DMIMessage currDMIMessage;

        while (activeChannel->readMessage(currDMIMessage))
        {
          // Extract DMIMessageType
          // Clear the MSB(Most Significant Byte from the message Type i.e acknowledged message)
          DMIMessageType dmiMessageType = static_cast<DMIMessageType>(currDMIMessage.dmiData.msgType & 0x7FU);

          if (dmiMessageInParser.count(dmiMessageType) > 0U)
          {
            AbstractDMIMessageIn *messageInParser = dmiMessageInParser[dmiMessageType];

            // Set data to the parser 
            messageInParser->setMessageData(currDMIMessage);
            // Validate & parse the DMI Message 
            bool isValidationOK = messageInParser->validate();

            if (!isValidationOK)
            {
              trace.write(ATC::detailedTrace, "DMI Handler:Validation of the Incoming DMI Message Failed");
              writeToLog(ATC::DetailedLog, "DMI Handler:Validation of the Incoming DMI Message Failed", __FILE__, __LINE__);
            }
            else
            {
              messageInParser->logToRU();
            }
          }
          else
          {
            // Report Event for unexpected DMI Message Type
            //prepare the dynamic text to be send while reporting event.
            unexpectedDMIMessageType.setDynamicText(static_cast<uint32_t>(dmiMessageType));
            ATC::AbstractEventHandler::corePtr()->reportEvent(unexpectedDMIMessageType, __FILE__
              , __LINE__);
          }
        }
      }

      handleOdometerFailures();
      getDriverOdoFailDMIButtonConfirmation();
    }

    /******************************************************************************
    * Function: runOut()
    ******************************************************************************/
    void AbstractDMIHandler::runOut(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "DH_runOut");

      if (activeChannel != (static_cast<DMIChannel*>(NULL)))
      {
        // Get the first MessageOutCreator Pointer
        std::vector<AbstractDMIMessageOut*>::iterator iterDMIOutMessage = dmiMessageOutCreator.begin();

        while (iterDMIOutMessage != dmiMessageOutCreator.end())
        {
          AbstractDMIMessageOut *dmiMessageCreator = (*iterDMIOutMessage);
          DMIMessage dmiMessageToSend;

          // Invalidate data
          dmiMessageCreator->invalidate();

          // Collect data from other Components
          dmiMessageCreator->collectData();

          // Validate and assemble data
          if (dmiMessageCreator->validate())
          {
            bool dmiMessageReadyToSent = dmiMessageCreator->getMessageData(dmiMessageToSend);

            // Write DMI Message to the Active Channel 
            bool dmiMessageWritten = activeChannel->writeMessage(dmiMessageToSend);

            if ((!dmiMessageWritten) || (!dmiMessageReadyToSent))
            {
              trace.write(ATC::veryDetailedTrace, "DMI Handler:Getting or Writing of DMI Message on Active DMI Channel Failed");
              writeToLog(ATC::VeryDetailedLog, "DMI Handler:Getting or Writing of DMI Message on Active DMI Channel Failed", __FILE__, __LINE__);
            }
            else
            {
              dmiMessageCreator->logToRU();
            }
          }
          ++iterDMIOutMessage;
        }// End of while loop

        // Schedule the runOut() for DMIChannel
        activeChannel->runOut();
      }
      else
      {
        trace.write(ATC::detailedTrace, "DMI Handler:NO DMI Channel is active");
        writeToLog(ATC::DetailedLog, "DMI Handler:NO DMI Channel is active", __FILE__, __LINE__);
      }
      // Increase the cycle count by 1 per cycle
      cycleCount++;
    }

    /******************************************************************************
    * Function: getConfirmation()
    ******************************************************************************/
    Confirmation AbstractDMIHandler::getConfirmation()
    {
      Confirmation retVal = DMIATPConfirmationUndefined;

      if (dmiMessageInParser.count(MTypeConfirmation) > 0U)
      {

        DMIMessageInConfirmation *dmiMessageInConfirmation
          = ATC::dynamicCast<AbstractDMIMessageIn*,DMIMessageInConfirmation*>(dmiMessageInParser[MTypeConfirmation], __FILE__, __LINE__); 
        
        retVal = dmiMessageInConfirmation->getConfirmation();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: Confirmation");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: Confirmation", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * getDMIStartupStatus
    ******************************************************************************/
    bool AbstractDMIHandler::getDMIStartupStatus(uint16_t &status)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeDMIStartup) > 0U)
      {
        DMIMessageInDMIStartup *dmiMessageInDMIStartup
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInDMIStartup*> (dmiMessageInParser[MTypeDMIStartup], __FILE__, __LINE__);
        
        retVal = dmiMessageInDMIStartup->getDMIStartupStatus(status);
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DMIStartup");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DMIStartup", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * getDMICompatibilityVersion
    ******************************************************************************/
    bool AbstractDMIHandler::getDMICompatibilityVersion(uint8_t &compatibilityVersion)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeDMIStartup) > 0U)
      {
        DMIMessageInDMIStartup *dmiMessageInDMIStartup
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInDMIStartup*> (dmiMessageInParser[MTypeDMIStartup], __FILE__, __LINE__);
        
        retVal = dmiMessageInDMIStartup->getDMICompatibilityVersion(compatibilityVersion);
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DMIStartup");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DMIStartup", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * getDMIStatusWord
    ******************************************************************************/
    //lint -esym(1714,ATP::DMICom::AbstractDMIHandler::getDMIStatusWord) May be used by adaptation
    uint16_t AbstractDMIHandler::getDMIStatusWord()
    {
      uint16_t retVal = 0U;
      if (dmiMessageInParser.count(MTypeDMIStatus) > 0U)
      {
        DMIMessageInDMIStatus *dmiMessageInDMIStatus
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInDMIStatus*> (dmiMessageInParser[MTypeDMIStatus], __FILE__, __LINE__); 

        retVal = dmiMessageInDMIStatus->getDMIStatusWord();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DMIStatus");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DMIStatus", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * getDMIButtonStatus
    ******************************************************************************/
    DMIButtonStatus AbstractDMIHandler::getDMIButtonStatus()
    {
      DMIButtonStatus retVal = DMIButtonUndefined;

      if (dmiMessageInParser.count(MTypeMMIToATPData) > 0U)
      {
        DMIMessageInDMIToATPData *dmiMessageInDMIToATPData
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInDMIToATPData*> (dmiMessageInParser[MTypeMMIToATPData] ,__FILE__, __LINE__);

        retVal = dmiMessageInDMIToATPData->getDMIButtonStatus();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DMIToATPData");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DMIToATPData", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * Function: getDriverIdAndPassword()
    ******************************************************************************/
    bool AbstractDMIHandler::getDriverIdAndPassword(DriverIdAndPassword &driverIdAndPass)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeDriverIdAndPassword) > 0U)
      {
        DMIMessageInDriverIDandPassword *dmiMessageInDriverIDandPassword
          = ATC::dynamicCast<AbstractDMIMessageIn*,DMIMessageInDriverIDandPassword*> (dmiMessageInParser[MTypeDriverIdAndPassword], 
            __FILE__, __LINE__);
        
        retVal = dmiMessageInDriverIDandPassword->getDriverIdAndPassword(driverIdAndPass);
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DriverIDandPassword");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DriverIDandPassword", __FILE__, __LINE__);
      }

      return retVal;
    }


    /******************************************************************************
    * Function: getChangedTrainName()
    ******************************************************************************/
    bool AbstractDMIHandler::getChangedTrainName(char_t * const changedTrainName)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeTrainName) > 0U)
      {
        DMIMessageInTrainName *dmiMessageInTrainName
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInTrainName*> (dmiMessageInParser[MTypeTrainName], __FILE__, __LINE__); 

        retVal = dmiMessageInTrainName->getChangedTrainName(changedTrainName);
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: Incoming Train Name");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: Incoming Train Name", __FILE__, __LINE__);
      }

      return retVal;
    }


    /******************************************************************************
    * getDriverOdoFailDMIButtonConfirmation()
    ******************************************************************************/
    void AbstractDMIHandler::getDriverOdoFailDMIButtonConfirmation()
    {
      //Remember the shorter failure of Tachometer
       switch(getDMIButtonStatus())
        {
        case DMICom::DMIButtonConfirmTachometer1Failure:
          isDriverConfirmedTachometer1Failure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(driverConfirmedTachometer1Failure, __FILE__, __LINE__);
          break;
        case DMICom::DMIButtonConfirmTachometer2Failure:
          isDriverConfirmedTachometer2Failure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(driverConfirmedTachometer2Failure, __FILE__, __LINE__);
          break;
        case DMICom::DMIButtonConfirmDopplerFailure:
          isDriverConfirmedDopplerFailure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(driverConfirmeddopplerFailure, __FILE__, __LINE__);
          break;
          // To please lint:
        case DMICom::DMIButtonUndefined:
        case DMICom::DMIButtonBrakeRelease:
        case DMICom::DMIButtonTrainConfig:
        case DMICom::DMIButtonSpare5:
        case DMICom::DMIButtonHandlingDone:
        case DMICom::DMIButtonEnterYardMode:
        case DMICom::DMIButtonRetryConfig:
        case DMICom::DMIButtonAbortSetup:
        case DMICom::DMIButtonLogoutDriver:
        case DMICom::DMIButtonEnterPossessionMode:
        case DMICom::DMIButtonShuntingMode:
        case DMICom::DMIButtonSpare16:
        case DMICom::DMIButtonStartBrakeTest:
        case DMICom::DMIButtonAbortBrakeTest:
        case DMICom::DMIButtonStartBtmTest:
        case DMICom::DMIButtonAbortLastCarBrakeTest:
        case DMICom::DMIButtonConfirmYard:
        case DMICom::DMIButtonConfirmShuntingRoute:
        case DMICom::DMIButtonConfirmStaffResponsible:
        case DMICom::DMIButtonConfirmJoin:
        case DMICom::DMIButtonConfirmSleep:
        case DMICom::DMIButtonConfirmSplit:
        case DMICom::DMIButtonConfirmDeparture:
        case DMICom::DMIButtonAcceptAutomaticConfig:
        case DMICom::DMIButtonRequestFreeRolling:
        case DMICom::DMIButtonConfirmFreeRollingCleared:
        case DMICom::DMIButtonConfirmStaffResponsibleMA:
        case DMICom::DMIButtonConfirmShuntingRouteMA:
        case DMICom::DMIButtonConfirmJoinMA:
        case DMICom::DMIButtonCancelRegistrationArea:
        case DMICom::DMIButtonConfirmChangeOfTrainLoaded:
        case DMICom::DMIButtonCancelChangeOfTrainLoaded:
        case DMICom::DMIButtonConfirmAbortLastCarBrakeTest:
        case DMICom::DMIButtonCancelAbortLastCarBrakeTest:
        case DMICom::DMIButtonTIMSInhibit:
        case DMICom::DMIButtonTIMSResume:
        case DMICom::DMIButtonManualTrainIntegrity:
        case DMICom::DMIButtonConfirmManualTrainIntegrity:
        case DMICom::DMIButtonCancelManualTrainIntegrity:
        case DMICom::DMIButtonMaxCount:
          // Do nothing
          break;
        default:
          //Some Event handling
          break;
        }
    }


    /******************************************************************************
    * handleOdometerFailures()
    ******************************************************************************/
    void AbstractDMIHandler::handleOdometerFailures()
    {
      //Values of Tachometer and Doppler failures are  collected from other component,
      //Values shall be remembered until confirmed by driver

      if (Pos::AbstractOdometry::corePtr()->getTachometer1Failure())
      {
        if (!OdoFailureTachometer1)
        {
          OdoFailureTachometer1 = true;
          isDriverConfirmedTachometer1Failure = false;
        }
        else
        {
          //Nothing
        }
      }
      else
      {
        //If there is no tachometer1 failure , and driver has confirmed it
        //then stored value of tachometer1 is cleared
        if (isDriverConfirmedTachometer1Failure)
        {
          OdoFailureTachometer1 = false;
        }
        else
        {
          //Nothing
        }

      }

      if (Pos::AbstractOdometry::corePtr()->getTachometer2Failure())
      {
        if (!OdoFailureTachometer2)
        {
          OdoFailureTachometer2 = true;
          isDriverConfirmedTachometer2Failure = false;
        }
        else
        {
          //Nothing
        }
      }
      else
      {
        //If there is no tachometer2 failure , and driver has confirmed it
        //then stored value of tachometer2 is cleared
        if (isDriverConfirmedTachometer2Failure)
        {
          OdoFailureTachometer2 = false;
        }
      }

      if (Pos::AbstractOdometry::corePtr()->getDopplerFailure())
      {
        if (!OdoFailureDoppler)
        {
          OdoFailureDoppler = true;
          isDriverConfirmedDopplerFailure = false;
        }
        else
        {
          //Nothing
        }
      }
      else
      {
        //If there is no doppler failure , and driver has confirmed it
        //then stored value of doppler is cleared
        if (isDriverConfirmedDopplerFailure)
        {
          OdoFailureDoppler = false;
        }
        else
        {
          //Do Nothing
        }
      }

    }

    /******************************************************************************
    * getOdometerTacho1Failure()
    ******************************************************************************/
    bool AbstractDMIHandler::getOdometerTacho1Failure() const
    {
      return OdoFailureTachometer1;
    }

    /******************************************************************************
    * getOdometerTacho2Failure()
    ******************************************************************************/
    bool AbstractDMIHandler::getOdometerTacho2Failure() const
    {
      return OdoFailureTachometer2;
    }

    /******************************************************************************
    * getOdometerDopplerFailure()
    ******************************************************************************/
    bool AbstractDMIHandler::getOdometerDopplerFailure() const 
    {
      return OdoFailureDoppler;
    }

    /******************************************************************************
    * getDriverTacho1FailureConfirmation()
    ******************************************************************************/
    bool AbstractDMIHandler::getDriverTacho1FailureConfirmation() const
    {
      return isDriverConfirmedTachometer1Failure;
    }

    /******************************************************************************
    * getDriverTacho2FailureConfirmation()
    ******************************************************************************/
    bool AbstractDMIHandler::getDriverTacho2FailureConfirmation() const
    {
      return isDriverConfirmedTachometer2Failure;
    }

    /******************************************************************************
    * getDriverDopplerFailureConfirmation()
    ******************************************************************************/
    bool AbstractDMIHandler::getDriverDopplerFailureConfirmation() const
    {
      return isDriverConfirmedDopplerFailure;
    }

    /******************************************************************************
    * Function: getRegistrationArea()
    ******************************************************************************/
    bool AbstractDMIHandler::getRegistrationArea(uint8_t &registrationAreaId)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeRegistrationArea) > 0U)
      {
        DMIMessageInRegistrationArea *dmiMessageInRegistrationArea
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInRegistrationArea*> (dmiMessageInParser[MTypeRegistrationArea], __FILE__, __LINE__);

        retVal = dmiMessageInRegistrationArea->getRegistrationArea(registrationAreaId);
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: RegistrationArea");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: RegistrationArea", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * Function: getLocoVsTrainDir()
    ******************************************************************************/
    //lint -esym(1714,ATP::DMICom::AbstractDMIHandler::getLocoVsTrainDir) May be used by adaptation
    LocoVsTrainDirection AbstractDMIHandler::getLocoVsTrainDir()
    {
      LocoVsTrainDirection locVsTrainDir = DMIATPLocoVsTrainDirUndefined;

      if (dmiMessageInParser.count(MTypeLocoVsTrainDir) > 0U)
      {
        DMIMessageInLocoVsTrainDir *dmiMessageInLocoVsTrainDir
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInLocoVsTrainDir*> (dmiMessageInParser[MTypeLocoVsTrainDir], __FILE__, __LINE__);
        
        locVsTrainDir = dmiMessageInLocoVsTrainDir->getLocoVsTrainDir();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: LocoVsTrainDir");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: LocoVsTrainDir", __FILE__, __LINE__);
      }

      return locVsTrainDir;
    }

    /******************************************************************************
    * Function: getTrainVsTrackDirection()
    ******************************************************************************/
    TrainVsTrackDirection AbstractDMIHandler::getTrainVsTrackDirection()
    {
      TrainVsTrackDirection retVal = DMIATPTrainVsTrackDirectionUndefined;

      if (dmiMessageInParser.count(MTypeTrainVsTrackDir) > 0U)
      {
        DMIMessageInTrainVsTrackDir *dmiMessageInTrainVsTrackDir
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInTrainVsTrackDir*> (dmiMessageInParser[MTypeTrainVsTrackDir], __FILE__, __LINE__);
          
        retVal = dmiMessageInTrainVsTrackDir->getTrainVsTrackDirection();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: TrainVsTrackDir");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: TrainVsTrackDir", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * getNextMessageNumber
    ******************************************************************************/
    uint8_t AbstractDMIHandler::getNextMessageNumber(void)
    {
      ++messageNumberCounter;
      // Unacknowledged messageNumber(0) shall not be used for acknowledged messages
      if (unacknowledgedMessageNumber == messageNumberCounter)
      {
        // Acknowledged messageNumbers are in the range 1..255
        messageNumberCounter = 1U;
      }
      return messageNumberCounter;

    }

    /******************************************************************************
    * getCycleCount
    ******************************************************************************/
    uint16_t AbstractDMIHandler::getCycleCount(void) const
    {
      return cycleCount;
    }

    /******************************************************************************
    * setDMICompatibilityVersionAccepted
    ******************************************************************************/
    void AbstractDMIHandler::setDMICompatibilityVersionAccepted(void)
    {
      dmiCompatibilityVersionAccepted = true;
    }

    /******************************************************************************
    * getDMICompatibilityVersionAccepted
    ******************************************************************************/
    bool AbstractDMIHandler::getDMICompatibilityVersionAccepted(void) const
    {
      return dmiCompatibilityVersionAccepted;
    }

    /******************************************************************************
    * getDMINewTrainSetUpConfirmed
    ******************************************************************************/
    bool AbstractDMIHandler::getDMINewTrainSetUpConfirmed(void)
    {
      bool retValue = false;
      if (dmiMessageInParser.count(MTypeVehicleData) > 0U)
      {
        DMIMessageInVehicleData *dmiIMessageInCarNameList
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInVehicleData*> (dmiMessageInParser[MTypeVehicleData], __FILE__, __LINE__);
          
        retValue = dmiIMessageInCarNameList->getManualTrainSetupConfirmed();
      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser for the message CarNameList was not found!");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser for the message CarNameList was not found!", __FILE__, __LINE__);
      }

      return retValue;
    }


    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractDMIHandler::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const toWrite = "dmich         To get information which all DMI Channels are active and connected.";

        ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
      }

      return retVal;
    }


    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractDMIHandler* AbstractDMIHandler::corePtr(void)
    {
      return coreDMIHandlerInstancePtr;
    }

    /******************************************************************************
    * addStartupMsg
    ******************************************************************************/
    void AbstractDMIHandler::addStartupMsg(const char_t * const str)
    {
      std::vector<AbstractDMIMessageOut*>::iterator iterDMIOutMessage = dmiMessageOutCreator.begin();
      for (; iterDMIOutMessage != dmiMessageOutCreator.end(); ++iterDMIOutMessage)
      {
        if (MTypeStartupHistory == (*iterDMIOutMessage)->getMessageType())
        {
          DMIMessageOutDMIStartupHistory* dmiMessageOutDMIStartupHistory =
            ATC::dynamicCast<AbstractDMIMessageOut*, DMIMessageOutDMIStartupHistory*>(
            (*iterDMIOutMessage), __FILE__, __LINE__); 
          dmiMessageOutDMIStartupHistory->addStartupMsg(str);
          break;
        }
      }
    }

    /******************************************************************************
    * startupAndHealthSupTest
    ******************************************************************************/
    bool AbstractDMIHandler::startupAndHealthSupTest() const
    {
      return (isConnected());
    }

    /******************************************************************************
    * getConnected
    ******************************************************************************/
    bool AbstractDMIHandler::isConnected() const
    {
      bool dmiChannelConnected = false;

      std::vector<DMIChannel*>::const_iterator iter = dmiChannels.begin();

      while ((iter != dmiChannels.end()) && (!dmiChannelConnected))
      {
        DMIChannel* dmiChannel = (*iter);
        if (dmiChannel->isConnected())
        {
          dmiChannelConnected = true;
        }
        else
        {
          ++iter;
        }
      }
      return dmiChannelConnected;
    }

    /******************************************************************************
    * setLocoVsTrainDirData
    ******************************************************************************/
    void AbstractDMIHandler::setLocoVsTrainDirData(const LocoVsTrainDirection &locoVsTrainDirValue)
    {
      locoVsTrainDirData = locoVsTrainDirValue;
    }

    /******************************************************************************
    * getLocoVsTrainDirData
    ******************************************************************************/
    LocoVsTrainDirection AbstractDMIHandler::getLocoVsTrainDirData() const
    {
      return locoVsTrainDirData;
    }

    /******************************************************************************
    * Function: resetLocoVsTrainDirData()
    ******************************************************************************/
    void AbstractDMIHandler::resetLocoVsTrainDirData()
    {
      locoVsTrainDirData = DMIATPLocoVsTrainDirUndefined;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractDMIHandler::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      // dmiChannels : Complex type
      // activeChannel : Complex type
      // dmiMessageInParser : Complex type
      // dmiMessageOutCreator : Complex type
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unexpectedDMIMessageType));
      // dmiChannel1 : Has its own cross comparison
      // dmiChannel2 : Has its own cross comparison
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDoneChnl1));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDoneChnl2));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&messageNumberCounter));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&cycleCount));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&dmiCompatibilityVersionAccepted));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<LocoVsTrainDirection>(&locoVsTrainDirData));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&OdoFailureTachometer1));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&OdoFailureTachometer2));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&OdoFailureDoppler));

    }

  }
}

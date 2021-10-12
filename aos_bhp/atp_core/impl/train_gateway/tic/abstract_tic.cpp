/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AbstractTIC class
* which contains the core functionality of the TIC Component.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-06-11    nsyed       Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <vfw_string.h>

#include "abstract_config.hpp"
#include "abstract_console.hpp"
#include "abstract_tic.hpp"
#include "atc_util.hpp"
#include "vehicle_com.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
#include "abstract_tic_event_ids.hpp"

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
  namespace TG
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractTIC::AbstractTIC() :
      ATC::ProcComponent(atpTicId, "TIC", "TI"),
      ticAvailable(false),
      configRequested(false),
      timeoutDeliverConfigInMilliSec(0U),
      deliverTrainConfigTimeout(ATC::Event::createLogEvent(atpTicId,
        ATC::CoreContainer, eventIdDeliverTrainConfigTimeout, 0U, "Max time exceeded to deliver Train Configuration by TIC!")),
      ticIsNowUnavailable(ATC::Event::createLogEvent(atpTicId,
        ATC::CoreContainer, eventIdTicIsNowUnavailable, 0U, "TIC has now transitioned into being Unavailable")),
      initDone(false),
      ticTimeoutCounter(0U),
      isTicTimeoutExpired(false),
      ticConfigStatus(TICConfigStatusIdle)
    {
      if (coreTICInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "TIC Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreTICInstancePtr = this;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTIC::init()
    {   
      // Adding of variables for cross-compare and read from configuration only needed once.
      if (!initDone)
      {
        // Reseting the state-variables for the TIC component.
        reset();

        timeoutDeliverConfigInMilliSec = static_cast<uint32_t>(ATC::secToMSec) * 
          static_cast<uint32_t>(AbstractConfig::corePtr()->getTrainCfgTicTimeout());
        initCrossCompare();
        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTIC::run()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TI_run");

      ticAvailable = evaluateTICAvailable();
      if (ticAvailable)
      {
        handleTicAvailable();
      }
      else
      {
        if ((TICConfigStatusInProgress == ticConfigStatus) || (TICConfigStatusPending == ticConfigStatus))
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(ticIsNowUnavailable, __FILE__,
            __LINE__);
          ticConfigStatus = TICConfigStatusError;
        }
      }
    }

    /******************************************************************************
    * handleTicAvailable
    ******************************************************************************/
    void AbstractTIC::handleTicAvailable()
    {
      switch (ticConfigStatus)
      {
      case TICConfigStatusIdle:
        // Fall through
      case TICConfigStatusError:
        if (configRequested)
        {
          writeToLog(ATC::BriefLog, "Automatic TrainConfiguration requested from TIC!", __FILE__, __LINE__);
          ticConfigStatus = TICConfigStatusPending;
        }
        break;

      case TICConfigStatusPending:
        // Fall through
      case TICConfigStatusInProgress:
      {
        TICConfigReqStatus ticConfigReqSentStatus = evaluateConfigReqStatus();
        
        /*Calculate time out for TIC*/
        const uint32_t maxCountForTicTimeout = timeoutDeliverConfigInMilliSec / 100U;

        // Check if current timer has not reached timeout value
        if (ticTimeoutCounter <= maxCountForTicTimeout)
        {
          ticTimeoutCounter++;
        }
        else
        {
          isTicTimeoutExpired = true;
        }

        // If TIC timeout timer expired change the status to Error
        if(isTicTimeoutExpired)
        {
          ticConfigStatus = TICConfigStatusError;
          configRequested = false;
          ticAvailable = false;

          // TIC took too long to fetch train configuration
          ATC::AbstractEventHandler::corePtr()->reportEvent(deliverTrainConfigTimeout, __FILE__,
            __LINE__);
        }
        else if (ticConfigReqSentStatus == TICWaitingForConfig)
        {
          ticConfigStatus = TICConfigStatusInProgress;
        }
        else if (ticConfigReqSentStatus == TICConfigReceived)
        {
          ticConfigStatus = TICConfigStatusCompleted;
        }
        else
        {
          // Do nothing. Wait for timer to expire/fetch Train Configuration
        }

        break;
      }

      case TICConfigStatusCompleted:
        ticTimeoutCounter = 0U;
        isTicTimeoutExpired = false;
        break;
      
      default:
        break;
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTIC* AbstractTIC::corePtr()
    {
      return coreTICInstancePtr;
    }

    /******************************************************************************
    * getTICAvailable
    ******************************************************************************/
    bool AbstractTIC::getTICAvailable() const
    {
      return ticAvailable;
    }

    /******************************************************************************
    * getStatus
    ******************************************************************************/
    TICConfigStatus AbstractTIC::getStatus() const
    {
      return ticConfigStatus;
    }

    /******************************************************************************
    * abortConfig
    ******************************************************************************/
    void AbstractTIC::abortConfig()
    {
      configRequested = false;
      ticConfigStatus = TICConfigStatusIdle;

      if (isTicTimeoutExpired)
      {
        ticTimeoutCounter = 0U;
        isTicTimeoutExpired = false;
      }
    }

    /******************************************************************************
    * requestConfig
    ******************************************************************************/
    void AbstractTIC::requestConfig()
    {
      configRequested = true;
    }

    /******************************************************************************
    * reset
    ******************************************************************************/
    void AbstractTIC::reset()
    {
      // Reseting the state-variables for the TIC component.
      ticAvailable = false;
      configRequested = false;
      ticConfigStatus = TICConfigStatusIdle;
      ticTimeoutCounter = 0U;
      isTicTimeoutExpired = false;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTIC::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&ticAvailable));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&configRequested));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&timeoutDeliverConfigInMilliSec));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&deliverTrainConfigTimeout));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&ticIsNowUnavailable));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TICConfigStatus>(&ticConfigStatus));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isTicTimeoutExpired));  
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&ticTimeoutCounter));
    }

    /******************************************************************************
    * getLocoOrientationAvailable
    ******************************************************************************/
    //lint -esym(1714,ATP::TG::AbstractTIC::getLocoOrientationAvailable) May be used in other projects
    bool AbstractTIC::getLocoOrientationAvailable() const
    {
      return false; // Not available unless overridden by adaptation
    }

    /******************************************************************************
    * evaluateTICAvailable
    ******************************************************************************/
    bool AbstractTIC::evaluateTICAvailable()
    {
      return false; // Not available unless overridden by adaptation
    }

    /******************************************************************************
    * evaluateConfigReqStatus
    ******************************************************************************/
    TICConfigReqStatus AbstractTIC::evaluateConfigReqStatus()
    {
      return TICConfigReqNotSent; // Not available unless overridden by adaptation
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractTIC::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. Returning false will let other components handle the call. "help" always returns false.
      */

      bool retVal = false;
      char_t  buffer[512];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        //lint -e{586} snprintf is needed here
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
          "%s%11s%s", "tic", "", "To get TIC availability status and Automatic Train Configuration Status");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "tic", sizeof("tic")) && (argc == 1U))
      {
        char_t configStatus[maxConfigStatusNameLength];

        getTICConfigStatusStr(getStatus(), &configStatus[0]);

        bool connectedToLcs = AbstractVehicleCom::corePtr()->connectedVehicleComm();

        //lint -e{586} snprintf is needed here
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "TIC Availability: %s\nTICConfigStatus: %s \nConnected To LCS: %s ",
          (getTICAvailable()) ? "Available" : "Not Available", configStatus, connectedToLcs ? "Yes": "No");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else
      {
        //Do Nothing
      }

      return retVal;
    }

    /******************************************************************************
    * getTICConfigStatusStr
    ******************************************************************************/
    void AbstractTIC::getTICConfigStatusStr(const TICConfigStatus configStatus, char_t* const buffer) const
    {
      switch (configStatus)
      {
      case TICConfigStatusIdle:
        static_cast<void>(vfw_strlcpy(buffer, "Idle", maxConfigStatusNameLength));
        break;

      case TICConfigStatusPending:
        static_cast<void>(vfw_strlcpy(buffer, "Pending", maxConfigStatusNameLength));
        break;

      case TICConfigStatusInProgress:
        static_cast<void>(vfw_strlcpy(buffer, "In Progress", maxConfigStatusNameLength));
        break;

      case TICConfigStatusCompleted:
        static_cast<void>(vfw_strlcpy(buffer, "Completed", maxConfigStatusNameLength));
        break;

      case TICConfigStatusError:
        static_cast<void>(vfw_strlcpy(buffer, "Error", maxConfigStatusNameLength));
        break;

      default:
        static_cast<void>(vfw_strlcpy(buffer, "Invalid Status", maxConfigStatusNameLength));
        break;
      }
    }
  }
}

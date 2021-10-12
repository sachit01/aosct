/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the TIC class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-29    nsyed       Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "config.hpp"
#include "tic.hpp"
#include "vehicle_com.hpp"
#include "event_handler.hpp"
#include "abstract_cross_compare.hpp"
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

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
    TIC::TIC(void) :
      AbstractTIC(),
      ticInitStartTime(0)
    {
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    TIC& TIC::instance(void)
    {
      static TIC theOnlyTICInstance;

      return theOnlyTICInstance;
    }

    /******************************************************************************
    * evaluateTICAvailable
    ******************************************************************************/
    bool TIC::evaluateTICAvailable()
    {
      bool retVal = false;
      EcpbOperatingModesType ecpbOperatingMode;
      const int64_t timeNow = vfwGetReferenceTime();
      const int64_t ticInitTimeoutInMillis = 1000 * static_cast<int64_t>(Config::instance().getTicInitToRunTimeout());

      // Check that operating ECPB mode is valid (i.e. LCS is connected)
      if (VehicleCom::instance().getEcpbOperatingModes(ecpbOperatingMode))
      {
        switch (ecpbOperatingMode)
        {
        case EcpbOperatingRunMode:
        {
          // Fetch reported Brake System
          const BrakeSystemType brakeSystemReported = VehicleCom::instance().getBrakeSystem();

          // Fetch Loco-type
          const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());

          // Brake system type 2 if the stored brake system in use is ECPB
          if ((BrakeSystemType2 == brakeSystemReported) && (EMD == locoType))
          {
            // TIC is initialized and running.
            ticInitStartTime = 0;
            retVal = true;
          }
          break;
        }

        case EcpbOperatingNotAvailable:
          // TIC is unavailable
          ticInitStartTime = 0;
          break;

        case EcpbOperatingInitializationMode:
          // Fall through
        case EcpbOperatingSwitchMode:
          // Fall through
        case EcpbOperatingCutOutMode:
          // If a timeout occur -> Evaluate to false until a state transition occurs.
          // TIC is available but not in run mode yet
          if (ticInitStartTime == 0)
          {
            ticInitStartTime = timeNow;
          }

          if ((timeNow - ticInitStartTime) < ticInitTimeoutInMillis)
          {
            // Consider TIC available in these modes (until timeout occurs).
            retVal = true;
          }
          break;

        default:
          break;
        }
      }

      return retVal;
    }

    /******************************************************************************
    * evaluateConfigReqStatus
    ******************************************************************************/
    TICConfigReqStatus TIC::evaluateConfigReqStatus()
    {
      TICConfigReqStatus ticConfigReqSentStatus = TICConfigReqNotSent;

      ECPBTrainCompositionType ecpbTrainComposition;
      EcpbSequenceStatusType ecpbSequenceScanning;
      const bool seqStatValid = VehicleCom::instance().getEcpbSequenceStatus(ecpbSequenceScanning);

      if (VehicleCom::instance().getECPBTrainComposition(ecpbTrainComposition))
      {
        ticConfigReqSentStatus = TICConfigReceived;
      }
      else if (seqStatValid &&
        ((EcpbSequenceScanning == ecpbSequenceScanning) || (EcpbSequenceConfigurationKnown == ecpbSequenceScanning)))
      {
        ticConfigReqSentStatus = TICWaitingForConfig;
      }
      else
      {
        //Do nothing
      }

      return ticConfigReqSentStatus;
    }

    /******************************************************************************
    * getLocoOrientationAvailable
    ******************************************************************************/
    //lint -esym(1714,ATP::TG::TIC::getLocoOrientationAvailable) May be used in other projects
    bool TIC::getLocoOrientationAvailable() const
    {
      //In BHP TIC is not delivering LocoOrientation.
      return false;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TIC::initCrossCompare() const
    {
      AbstractTIC::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&ticInitStartTime));
    }
  }
}

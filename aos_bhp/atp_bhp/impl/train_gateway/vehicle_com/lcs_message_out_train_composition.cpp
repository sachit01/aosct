/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
* This file implements the creator for the LCSMessageOutTrainComposition message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-10    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_types.h>
#include <vfw_buffer.h>

#include "atp_types.hpp"
#include "abstract_tsetup.hpp"
#include "lcs_message_out_train_composition.hpp"
#include "lcs_message_common.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_mode_control.hpp"
#include "vehicle_com.hpp"

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
    * LCSMessageOutTrainComposition constructor, Note: Update version (2nd parameter) if message is updated
    ******************************************************************************/
    LCSMessageOutTrainComposition::LCSMessageOutTrainComposition() : AbstractLCSMessageOut(LCSMTypeTrainCompositionMessage, 1U, true)
    {
      trainComposition.numberOfVehicles = 0U;
      memset(&trainComposition.rollingStockPosition[0], 0, sizeof(trainComposition.rollingStockPosition));
    }

    /******************************************************************************
    * LCSMessageOutTrainComposition::collectData
    ******************************************************************************/
    void LCSMessageOutTrainComposition::collectData()
    {
      bool configValid = false;
      Kernel::TrainSetupReason  reason;
      
      // Time to send a TrainComposition?
      // 1) A new valid TrainSetup is received from TCC.
      // 2) LEADER has recovered from communication loss

      const bool isTsetupRecv = Kernel::AbstractMessageHandler::corePtr()->getQSetup(reason);
      const bool leaderHasRecovered = VehicleCom::instance().leaderHasRecoveredFromComLoss();
      const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

      // 3) A re-registration TSetup has been accepted by the driver. It has been confirmed and a valid TSetup exists.
      const ATPMode mode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      const Kernel::TrainConfigModeState modeState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();
      const bool reregTSetupConfirmed = (mode == ATPModeConfiguration) && (modeState == Kernel::TrainConfigMode::trainConfigSendStartUpForReReg);
      

      if ((isTsetupRecv || leaderHasRecovered || reregTSetupConfirmed) && (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL)))
      {
        configValid = true;

        // Number of cars + lead locomotive
        trainComposition.numberOfVehicles = pTrainSetup->vehicleCount;

        getTracer().write(ATC::detailedTrace, "trainComposition.numberOfVehicles:",
          static_cast<uint32_t>(trainComposition.numberOfVehicles));

        for (uint16_t i = 0U; i < pTrainSetup->vehicleCount; i++)
        {
          DS::VehicleSetup vehicleSetup;

          // Get road-numbers and vehicle types
          if (DS::AbstractTSetup::corePtr()->getVehicleSetup(i, vehicleSetup))
          {
            trainComposition.rollingStockPosition[i].vechicleType = aosToLcsVtype(vehicleSetup.vehicleType);
            getTracer().write(ATC::detailedTrace, "trainComposition.rollingStockPosition[i].vechicleType:",
              static_cast<uint32_t>(trainComposition.rollingStockPosition[i].vechicleType));

            trainComposition.rollingStockPosition[i].roadNumber = vehicleSetup.nodeAdress;
            getTracer().write(ATC::detailedTrace, "trainComposition.rollingStockPosition[i].roadNumber:",
              static_cast<uint32_t>(trainComposition.rollingStockPosition[i].roadNumber));
          }
          else
          {
            // Invalid vehicleSetup
            getTracer().write(ATC::detailedTrace, "Unable to fetch Vehicle-Setup");
            configValid = false;
          }
        }
      }

      if (true == configValid)
      {
        setDataProcessState(DataAvailable);
      }
    }
  

    /******************************************************************************
    * LCSMessageOutTrainComposition::validate
    ******************************************************************************/
    bool LCSMessageOutTrainComposition::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::briefTrace, "Validating LCSMessageOutTrainComposition");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }
      
      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageOutTrainComposition::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutTrainComposition::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

      vfwPutU16(&buffer, trainComposition.numberOfVehicles);

      // Rolling Stock Position block
      for (uint16_t i = 0U; i < trainComposition.numberOfVehicles; i++)
      {
        vfwPutU8(&buffer, static_cast<uint8_t>(trainComposition.rollingStockPosition[i].vechicleType));
        vfwPutU16(&buffer, trainComposition.rollingStockPosition[i].roadNumber);
      }
    
      traceAssembleData(parseDataValid);

      // Total length of Application-message
      appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutTrainComposition::invalidate
    ******************************************************************************/
    void LCSMessageOutTrainComposition::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }     
  }
}

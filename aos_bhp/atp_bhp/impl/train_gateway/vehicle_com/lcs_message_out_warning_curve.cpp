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
* This file implements the creator for the Warning Curve Message towards LCS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-01-31    skothiya    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_math.hpp"
#include <vfw_buffer.h>
#include "lcs_message_common.hpp"
#include "lcs_message_out_warning_curve.hpp"
#include "abstract_targets.hpp"
#include "target_calculation.hpp"


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
    * LCSMessageOutWarningCurve constructor
    ******************************************************************************/
    LCSMessageOutWarningCurve::LCSMessageOutWarningCurve() : AbstractLCSMessageOut(LCSMTypeWarningCurveMessage, 1U, true)
    {
      lcsWarningCurve.numberOfPoints = 0U;
      memset(&lcsWarningCurve.speedCurvePoints[0], 0x00, sizeof(lcsWarningCurve.speedCurvePoints[0]));

    }

    /******************************************************************************
    * LCSMessageOutWarningCurve::collectData
    ******************************************************************************/
    void LCSMessageOutWarningCurve::collectData()
    {
      //Collecting the permitted speed (first warning) curve points
      const bool dataCollection = Supv::TargetCalculation::instance().getWarningCurveMessage(lcsWarningCurve);

      //data will be collected when there will be change in most restrictive target or restrictive target will be passed
      if (dataCollection)
      {
        setDataProcessState(DataAvailable);
      }

    }

    /******************************************************************************
    * LCSMessageOutWarningCurve::validate
    ******************************************************************************/
    bool LCSMessageOutWarningCurve::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::briefTrace, "Validating Data in the Warning Curve message to LCS");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }
      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageOutWarningCurve::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutWarningCurve::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool assembleDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

      //Number of points in warning curve
      vfwPutU16(&buffer, lcsWarningCurve.numberOfPoints);

      // warning curve sample data
      for (uint16_t sampleIndx = 0U; sampleIndx < lcsWarningCurve.numberOfPoints; sampleIndx++)
      {
        vfwPutU16(&buffer, lcsWarningCurve.speedCurvePoints[sampleIndx].trackAndPosition.track);
        vfwPutU32(&buffer, lcsWarningCurve.speedCurvePoints[sampleIndx].trackAndPosition.position);
        vfwPutU16(&buffer, lcsWarningCurve.speedCurvePoints[sampleIndx].newSpeed);
      }

      traceAssembleData(assembleDataValid);

      // Total length of Application-message
      appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));

      return assembleDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutWarningCurve::invalidate
    ******************************************************************************/
    void LCSMessageOutWarningCurve::invalidate()
    {
      lcsWarningCurve.numberOfPoints = 0U;
      memset(&lcsWarningCurve.speedCurvePoints[0], 0x00, sizeof(lcsWarningCurve.speedCurvePoints[0]));
      setDataProcessState(NoDataAvailable);
    }

  }
}

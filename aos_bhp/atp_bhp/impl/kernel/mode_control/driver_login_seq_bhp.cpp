/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implements the adaptation of the driver login sequence for BHP.
*.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2020-03-19    bhermans    Created
*
*******************************************************************************/
#include "driver_login_seq_bhp.hpp"
#include "config.hpp"
#include "atc_util.hpp"
#include "cross_compare.hpp"

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
    DriverLoginSeqBHP::DriverLoginSeqBHP() : DriverLoginSeq()
    {
      loginVerficationWaitCycleCount = 0U;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void DriverLoginSeqBHP::initCrossCompare() const
    {
      DriverLoginSeq::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&loginVerficationWaitCycleCount));
    }

    /******************************************************************************
    * runDriverLoginLoggedOut
    ******************************************************************************/
    void DriverLoginSeqBHP::runDriverLoggedOut()
    {
      DriverLoginSeq::runDriverLoggedOut();
      loginVerficationWaitCycleCount = 0U;
    }


    /******************************************************************************
    * runDriverLoginVerification
    ******************************************************************************/
    void DriverLoginSeqBHP::runDriverLoginVerification()
    {
      DriverLoginSeq::runDriverLoginVerification();

      if (getState() == driverLoginVerification)
      {
        // DriverLogonStatus not yet received from TCC
        // Been waiting too long?
        // Allow up to 4 times the AOS - TCC Communication Timeout to allow TCC to respond with DriverLogonStatus
        // before AOS times out and considers the Login as failed and displays the Login button again.
        const uint16_t tccTimeOutCycleCount =
          (static_cast<uint16_t>(AbstractConfig::corePtr()->getRadioTimeOut()) * 4U * ATC::secToMSec) /
          ATC::cycleCntToMsec; //lint !e734 no loss of precision

        if (loginVerficationWaitCycleCount >= tccTimeOutCycleCount)
        {
          loginVerficationWaitCycleCount = 0U;
          seqState = driverLoggedOut;
        }
        else
        {
          ++loginVerficationWaitCycleCount;
        }
      }
      else
      {
        // DriverLogonStatus received from TCC
        loginVerficationWaitCycleCount = 0U;
      }
    }
  }
}

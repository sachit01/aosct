#ifndef DriverLoginSeqBHP_hpp
#define DriverLoginSeqBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the adaptation of the driver login sequence for BHP.
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

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "driver_login_seq.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * The class DriverLoginSeqBHP defines the Adaptation of DriverLoginSeq for BHP.
    *
    */
    class DriverLoginSeqBHP : public DriverLoginSeq
    {

    public:

      /**
      * Constructor of Driver Login Seq adaptation for BHP
      *
      */
      DriverLoginSeqBHP();

      /**
      * Register the adaptation data to be cross compared.
      *
      */
      virtual void initCrossCompare() const;

    protected:

      /**
      * Adaptation of driver login verification for BHP
      *
      */
      virtual void runDriverLoginVerification();

      /**
      * Adaptation of driver logged out for BHP
      *
      */
      virtual void runDriverLoggedOut();

    private:

      /**
      * The number of cycles passed since waiting in DriverLoginVerification
      */
      uint16_t loginVerficationWaitCycleCount;

    };
  }
}

#endif


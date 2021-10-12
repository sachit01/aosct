#ifndef DriverLoginSeq_hpp
#define DriverLoginSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for the driver to login.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-01    arastogi    Created
* 2017-04-11    skothiya    updated for cabin handling and authorization requirement implementation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "event.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    typedef uint8_t DriverLoginState;

    /**
    * The class DriverLoginSeq defines the driver login sequence.
    *
    */
    class DriverLoginSeq
    {
    public:

      /**
      * The logged out state of DriverLoginSeq
      */
      static const DriverLoginState driverLoggedOut = 1U;

      /**
      * The logged in state of DriverLoginSeq
      */
      static const DriverLoginState driverLoggedIn = 2U;

      /**
      * The logged verification state of DriverLoginSeq
      */
      static const DriverLoginState driverLoginVerification = 3U;

      /**
      * The logged failed state of DriverLoginSeq
      */
      static const DriverLoginState driverLoginFailed = 4U;

      /**
      * The logged in state of ATO authorization
      */
      static const DriverLoginState atoAuthorized = 5U;

      /**
      * Main run function of the sequence.
      *
      */
      virtual void run();

      /**
      * Register the data to be cross compared.
      *
      */
      virtual void initCrossCompare() const;

      /**
      * Constructor.
      *
      */
      DriverLoginSeq();

      /**
      * Getter for the state of the DriverLoginSeq
      *
      * @return DriverLoginState value of seqState variable.
      */
      DriverLoginState getState() const;

      /**
      * Function to check if it is allowed to login
      * @return true  if allowed to login.
      */
      virtual bool isAllowedToLogin() const;

    protected:

      /**
      * Function to run the driverLoggedOut state of DriverLoginSeq.
      *
      */
      virtual void runDriverLoggedOut();

      /**
      * Function to run the driverLoggedIn state of DriverLoginSeq.
      *
      */
      virtual void runDriverLoggedIn();

      /**
      * Function to run the driverLoginVerification state of DriverLoginSeq.
      *
      */
      virtual void runDriverLoginVerification();

      /**
      * Function to run the driverLoginFailed state of DriverLoginSeq.
      *
      */
      virtual void runDriverLoginFailed();

      /**
      * Function to run the atoAuthorized state of DriverLoginSeq.
      *
      */
      virtual void runAtoAuthorized();

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      *
      */
      virtual ~DriverLoginSeq(void) {};

      /**
      * The current state of DriverLoginSeq
      */
      DriverLoginState seqState;

    private:

      /**
      * The number of cycles to wait in login failed state before going to logged out.
      * DMI will display the login failed message for this duration.
      */
      static const uint8_t maxLoginFailedCycles = 30U;

      /**
      * The number of cycles passed since login failed.
      */
      uint8_t loginFailedCycleCount;

      /**
      * flag to indicate that login is done through Yard mode while TCC is disconnected.
      */
      bool isLogInFromYardWithoutTCC;

      /**
      * To apply standstill event in case Driver is not authorized (Logged out)
      */
      const ATC::Event driverNotAuthStandstill;

      /**
      * To issue a log event on driver logout from DMI
      */
      const ATC::Event driverLogoutEvent;

      /**
      * Event to apply Standstill when Driver Login is not Authorized.
      */
      const ATC::Event driverLoginNotAuthorizedStandstill;
    };
  }
}

#endif

#ifndef StaffResponsibleMode_hpp
#define StaffResponsibleMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Staff Responsible mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-26    spandita    Created
*
*******************************************************************************/
/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    typedef uint8_t StaffResponsibleModeState;
    /**
    * The class StaffResponsibleMode defines the Staff Responsible mode.
    *
    */
    class StaffResponsibleMode : public AbstractMode
    {
    public:

      /**
      * The start state of Staff Responsible mode
      */
      static const StaffResponsibleModeState staffResponsibleStart = 1U;

      /**
      * The state of Staff Responsible mode to wait for confirmation from DMI
      */
      static const StaffResponsibleModeState staffResponsibleWaitConfirmDMI = 2U;
    
      /**
      * The state of Staff Responsible mode to wait for MA
      */
      static const StaffResponsibleModeState staffResponsibleWaitMA = 3U;

      /**
      * Transition state to confirm MA in Staff Responsible mode by driver on DMI
      */
      static const StaffResponsibleModeState staffResponsibleConfirmMAScratch = 4U;

      /**
      * Transition state to accept MA in Staff Responsible mode
      */
      static const StaffResponsibleModeState staffResponsibleWaitMAScratch = 5U;

      /**
      * The state of Staff Responsible if staff responsible is finished
      */
      static const StaffResponsibleModeState staffResponsibleFinishOk = 6U;

      /**
      * Main run function of the Staff Responsible mode.
      * @param[in] commonData   reference to train states
      *
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      StaffResponsibleMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeStaffResponsible enum value.
      */
      virtual ATPMode getModeId();

      /**
      * To get string for modeState.
      *
      * @param[in] state modeState to get string for.
      * @param[out] buffer the mode state string is copied to the buffer
      */
      void getModeStateString(const StaffResponsibleModeState state, char_t* const buffer) const;

      /**
      * Virtual function for the mode state string.
      *
      * @param[out] str The string where the modestate is copied to.
      * @return true if the mode has a modeState, false otherwise.
      */
      virtual bool getCurrentModeStateString(char_t* const str);

      /**
      * Virtual Function for Q route type check in Staff Responsible mode
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * Getter for the state of the Staff responsible mode
      *
      * @return StaffResponsibleModeState value of modeState variable.
      */
      virtual StaffResponsibleModeState getModeState() const;

      /**
      * Function to reset mode state
      */
      virtual void resetMode();

      /**
      * To get the status of validity of unconditional shortening message in staff responsible mode
      *
      * @return true if the unconditional shortening message is valid in the staff responsible mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    protected:

      /**
      * Function to run the staffResponsibleStart state of staff responsible mode.
      * @param[in] commonData   reference to common data
      */
      virtual void runStaffResponsibleStart(CommonDataForModes &commonData);

      /**
      * Function to run the staffResponsibleWaitConfirmDMI state of staff responsible mode.
      *
      * @param[in] commonData   reference to train states
      */
      virtual void runStaffResponsibleWaitConfirmDMI(CommonDataForModes &commonData);

      /**
      * Function to run runStaffResponsibleWaitMA state of staff responsible mode.
      *
      */
      virtual void runStaffResponsibleWaitMA();

      /**
      * Function to run runStaffResponsibleConfirmMAScratch state of Staff Responsible mode.
      *
      */
      virtual void runStaffResponsibleConfirmMAScratch();

      /**
      * Function to run runStaffResponsibleWaitMAScratch state of Staff Responsible mode.
      *
      */
      virtual void runStaffResponsibleWaitMAScratch();

      /**
      * Function to run staffResponsibleFinishOk state of staff responsible mode.
      *
      */
      virtual void runStaffResponsibleFinishOk();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The current state of Staff Responsible mode
      */
      StaffResponsibleModeState modeState;

      /**
      *  Event to apply Standstill event while waiting for confirmation for Staff Responsible mode on DMI
      */
      const ATC::Event standStillWaitSRConfirm;

    };
  }
}
#endif

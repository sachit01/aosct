#ifndef DMIMessageOutATPModesAndStates_hpp
#define DMIMessageOutATPModesAndStates_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  One creator per message-type.
*  The DMIMessageOutATPModesAndStatus creator is responsible for collecting
*  ATP Modes and States from other components and validation and creation of
*  the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_dmi_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {

    /**
    * Bit Value for Additional Status ATO Enable ( Data 11 bit 0)
    */
    static const uint8_t additionalStatusAtoEnabled = 0x01U;

    /**
    * Bit Value for Additional Status Driving forward ( Data 11 bit 1)
    */
    static const uint8_t additionalStatusDrivingForward = 0x02U;

    /**
    * Bit Value for Additional Status Standstill Event active (Data 11 bit 2)
    */
    static const uint8_t additionalStatusStandStillEventActive = 0x04U;

    /**
    * Bit Value for Additional Status Brake Test Possible (Data 11 bit 3)
    */
    static const uint8_t additionalStatusBrakeTestPossible = 0x08U;

    /**
    * Bit Value for Additional Status Brake Test Notification (Data 11 bit 4)
    */

    static const uint8_t additionalStatusBrakeTestNotification = 0x10U;

    /**
    * Bit Value for Additional Status Brake Test Mandatory (Data 11 bit 5)
    */
    static const uint8_t additionalStatusBrakeTestMandatory = 0x20U;

    /**
    * Bit Value for Train Status ,Train Loaded (Data 11 bit -6)
    */
    static const uint8_t additionalTrainStatusTrainLoaded = 0x40U;

    /**
    * Bit Value for LCS Communication Status (Data 12 bit-0,1)
    */
    static const uint8_t additionalLcsCommunicationStatus = 0x03U;

    /**
    * Bit Value for Additional Status Routine Test Possible (Data 12 bit 3)
    */
    static const uint8_t additionalStatusRoutineTestPossible = 0x08U;

    /**
    * Bit Value for Additional Status Routine Test Needed (Data 12 bit 4)
    */
    static const uint8_t additionalStatusRoutineTestNeeded = 0x10U;

    /**
    * Bit Value for Additional Status Routine Test Mandatory (Data 12 bit 5)
    */
    static const uint8_t additionalStatusRoutineTestMandatory = 0x20U;

    /**
    * Bit Value for Allowed to Show Train Composition (Data 20 bit -0)
    */
    static const uint8_t allowedToShowTrainComp = 0x01U;

    /**
    * Bit Value for Allowed to Show Free Rolling button (Data 20 bit -1)
    */
    static const uint8_t allowedToShowFreeRollButton = 0x02U;

    /**
    * Bit Value for Allowed to Show Handling Done (Data 20 bit -2)
    */
    static const uint8_t allowedToShowHandlingDone = 0x04U;

    /**
    * Bit Value for Allowed to Request Cancel Registration Area (Data 20 bit -3)
    */
    static const uint8_t allowedToRequestCancelRegistrationArea = 0x08U;

    /**
    * Bit Value for Allowed to select cars on A or B side (Data 20 bit 5)
    */
    static const uint8_t allowedToSelectCarsOnAOrBSide = 0x20U;

    /**
    * Bit Value for Allowed to inhibit the train integrity supervision (Data 20 bit 6)
    */
    static const uint8_t allowedToInhibitTrainIntegritySupv = 0x40U;

    /**
    * Bit Value for Allowed to resume the train integrity supervision(Data 20 bit 7)
    */
    static const uint8_t allowedToResumeTrainIntegritySupv = 0x80U;

    /**
    * Bit Value for confirmation of pre-departure test (Data 21 bit -0)
    */
    static const uint8_t activateDepartureTestConfirmation = 0x01U;

    /**
    * Bit Value for confirmation for MA from scratch in Staff responsible (Data 21 bit -1)
    */
    static const uint8_t activateStaffResponsibleMAConfirm = 0x02U;

    /**
    * Bit Value for confirmation for MA from scratch in Shunting route (Data 21  bit -2)
    */
    static const uint8_t activateShuntingrouteMAConfirmation = 0x04U;

    /**
    * Bit Value for Allowed to confirm  clear Free Rolling (Data 21 bit -4)
    */
    static const uint8_t allowedToShowConfFreeRollButton = 0x10U;

    /**
    * Bit Value for confirmation for MA from scratch in Join mode (Data 21 bit -5)
    */
    static const uint8_t activateJoinMAConfirmation = 0x20U;

    /**
    * Bit Value for confirmation of manual integrity confirmation (Data 21 bit 6)
    */
    static const uint8_t confirmManualIntegrityConfirmation = 0x40U;

    /**
    * Bit Value for confirmation of Tachometer1 failure (Data 23 bit 0)
    */
    static const uint8_t confirmTachometer1Failure = 0x01U;

    /**
    * Bit Value for confirmation of Tachometer2 failure (Data 23 bit 1)
    */
    static const uint8_t confirmTachometer2Failure = 0x02U;

    /**
    * Bit Value for confirmation of Doppler radar failure (Data 23 bit 2)
    */
    static const uint8_t confirmDopplerFailure = 0x04U;

    /**
    * Bit Value to activate Tachometer1 failure indication on DMI (Data 24 bit 0)
    */
    static const  uint8_t platformStatusTachometer1Failure = 0x01U;

    /**
    * Bit Value to activate Tachometer2 failure indication on DMI (Data 24 bit 1)
    */
    static const uint8_t platformStatusTachometer2Failure = 0x02U;

    /**
    * Bit Value to activate Doppler radar failure indication on DMI (Data 24 bit 2)
    */
    static const uint8_t platformStatusDopplerFailure = 0x04U;

    /**
    * DMIMessageOutATPModesAndStatus is a creator for the outgoing ATPModesAndStatus message
    */
    class DMIMessageOutATPModesAndStatus : public AbstractDMIMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing ATPModesAndStatus message
      */
      DMIMessageOutATPModesAndStatus();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the ATPModesAndStatus(shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * Logs self->messageData to RU. Assumes that collectData() and validate()
      * have been successfully called.
      *
      * Only logs the message if self->dmiData has changed since the last call.
      */
      virtual void logToRU() const;

    protected:

      /**
      * The collected class for all data used to create the outgoing message
      */
      class DmiData
      {
      public:
        /**
        * Default constructor. Initializes all members of the struct.
        */
        DmiData();

        /**
        * Clears and invalidates all attributes
        */
        void invalidate();

        /**
        * Inequality operator.
        * @return True if and only if this and that differ.
        */
        bool operator !=(const DmiData& that) const;

        /**
         * The collected ATP Mode And State used to create the outgoing message
         * Will be cleared each ATP execution-cycle by invalidate()
         */
        DMIATPMode dmiATPMode;

        /**
        * The collected ATP State used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        DMIATPStates dmiATPStates;

        /**
        * The collected ATP Mode Sub State used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        ATPModeSubState atpModeState;

        /**
        * The collected ATP Driver Verification State used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        DriverVerificationState driverVerState;

        /**
        * The collected ATP Data related ATP Integrity(Data 4 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t dmiIntegrityRelatedData;

        /**
        * The collected ATP Data related Locomotive Status(Data 5 to 8 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint32_t trainStatus;

        /**
        * The collected ATP Data related additional Locomotive Status(Data 11 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t additionalStatusBits1;

        /**
        * The collected ATP Data related additional Locomotive Status(Data 12 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t additionalStatusBits2;

        /**
        * The collected ATO Mode used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        DMIATOMode dmiATOMode;

        /**
        * The collected Wanted ATO Switch Position used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        DMIWantedATOSwitchPos dmiWantedATOSwitchPos;

        /**
        * The collected ATP Data related to activate visibility of mode confirmation buttons on DMI (Data 13 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        DMIConfirmModeChange dmiConfirmModeChange;

        /**
        * The collected ATP Data related to control the visibility of buttons on DMI (Data 14 in IF Spec ATP-MMI) to used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t dmiVisibilityControl;

        /**
        * The collected brake test status (Data 15 in IF Spec ATP-MMI) being used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t brakeTestStatus;

        /**
        * The collected remaining time (minutes) until next mandatory brake test (Data 16-17 in IF Spec ATP-MMI) being used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint16_t remainingTimeToMandatoryBrakeTest;

        /**
        * The collected remaining time (minutes) until next mandatory routine test (Data 18-19 in IF Spec ATP-MMI) being used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint16_t remainingTimeToMandatoryRoutineTest;

        /**
        * The collected additional information about allowed to option on dmi (Data 20 in IF Spec ATP-MMI) being used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t additionalAllowedToInfo;

        /**
        * The collected additional information about confirmation required from DMI (Data 21 in IF Spec ATP-MMI) being used to create the outgoing message
        * Will be cleared each ATP execution-cycle by invalidate()
        */
        uint8_t additionalConfirmInfo;

        /*Adaptation Train Status (Data 22 in IF Spec ATP-DMI)*/
        uint8_t adaptationTrainStatus;

        // Collected Additional infomation about confirmation from DMI (Data 23 in IF ATP-DMI) being used to create the outgoing message
        // Shall be cleared by ATP execution cycle by invalidate()
        uint8_t additionalConfirmInfo2;

        //The collected ATP Data related to control the visibility of Indication icons on DMI (Data 24 in IF Spec ATP-MMI) to used to create the outgoing message
        //Will be cleared each ATP execution cycle by invalidate()
        uint8_t PlatformStatus;

      };

      /**
      * The collected DMI data used to create the outgoing message
      */
      DmiData dmiData;

      private:
         /**
         * Assemble the collected data
         *
         * @return true if data is valid with respect to parsing
         */
         bool assembleDMIMessageData();

         /**
         * Get the DMI ATP State based on the current ATP Mode and Driver Login Sequence
         *
         * @param[in] atpmode  current ATP mode
         *
         * @param[in] atpDriverLoginState  Driver Login Sequence
         *
         * @return current DMI ATP state
         */
         DMIATPStates getDMIATPState(ATPMode const atpmode, Kernel::DriverLoginState const atpDriverLoginState) const;

         /**
         * Get the DMI ATP Driver Verification State based on Driver Login Sequence
         *
         * @param[in] atpDriverLoginState  Driver Login Sequence
         *
         * @return current DMI ATP driver verification state
         */
         DriverVerificationState getDMIATPDriverVerState(Kernel::DriverLoginState const atpDriverLoginState) const;

         /**
         * Get the DMI ATP Configuration Mode Sub-state based on the current ATP Mode
         *
         * @param[in] atpmode  current ATP mode
         *
         * @return current  ATP Modes substate  data
         */
         ATPModeSubState getATPModeSubState(ATPMode const atpMode) const;

         /**
         * Get the Change Mode Confirmation Mode for current DMIATPMode.
         *
         * @param[in] mode  current DMIATPMode
         *
         * @return the current change mode confirmation request to DMI
         */
         DMIConfirmModeChange getConfirmModeChange(const DMIATPMode mode) const;

      /**
      * sendCycleATPModeAndState
      */
      static const uint16_t sendCycleATPModeAndState = 10U;

      /**
      * delayATPModeAndState
      */
      static const uint16_t delayATPModeAndState = 7U;

      /**
      * Bit Value for Radio Available (Data 4 Bit -0)
      */
      static const uint8_t radioAvailable = 0x01U;

      /**
      * Bit Value for TIMS Available (Data 4 Bit -1)
      */
      static const uint8_t timsAvailable = 0x02U;

      /**
      * Bit Value for TIMS OK (Data 4 Bit -2)
      */
      static const uint8_t timsOK = 0x04U;

      /**
      * Bit Value for Stop Train request (Data 4 Bit -4)
      */
      static const uint8_t stopTrainRequest = 0x10U;

      /**
      * Bit Value for Allowed to login (Data 14 Bit -0)
      */
      static const uint8_t enableLogin = 0x01U;

      /**
      * Bit Value for Allowed to enter yard mode (Data 14 Bit -1)
      */
      static const uint8_t allowedToEnterYardMode = 0x02U;

      /**
      * Bit Value for Allowed to enter possession mode (Data 14 Bit -2)
      */
      static const uint8_t allowedToEnterPossessionMode = 0x04U;

      /**
      * Bit Value for Allowed to enter Shunting mode (Data 14 Bit -3)
      */
      static const uint8_t allowedToEnterShuntingMode = 0x08U;

      /**
      * Bit Value for Allowed to enter Config mode (Data 14 Bit -4)
      */
      static const uint8_t allowedToEnterConfigMode = 0x10U;

      /**
      * Bit Value for Allowed to change Train Name (Data 14 Bit -5)
      */
      static const uint8_t allowedToChangeTrainName = 0x20U;

      /**
      * Bit Value to activate Abort Setup option (Data 14 Bit -6)
      */
      static const uint8_t allowedToAbortSetup = 0x40U;

      /**
      * Bit Value to activate Logout option (Data 14 Bit -7)
      */
      static const uint8_t allowedToLogout = 0x80U;

    };
  }
}
#endif

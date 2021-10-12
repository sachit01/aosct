#ifndef DMIMessageOutSpeedAndDistance_hpp
#define DMIMessageOutSpeedAndDistance_hpp
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
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutSpeedAndDistance is a creator for the outgoing Speed & Distance DMIMessage
    */
    class DMIMessageOutSpeedAndDistance : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Speed & Distance DMIMessage
      */
      DMIMessageOutSpeedAndDistance();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Speed & Distance(shall be called once per ATP execution-cycle)
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
      * Only logs the message if a certain time has passed since the last call.
      * (See configuration item "RuLogDynValuePeriod")
      */
      virtual void logToRU() const;

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * Current Speed (cm/s)
      */
      uint16_t curSpeed;

      /**
      * Current Odometer(m)
      */
      int32_t curOdometer;

      /**
      * Leading track section
      */
      uint16_t leadingTrackSection;

      /**
      * Leading position (cm)
      */
      uint32_t leadingPosition;

      /**
      * Trailing track section
      */
      uint16_t trailingTrackSection;

      /**
      * Trailing position (cm)
      */
      uint32_t trailingPosition;

      /**
      * Current track-gradient (permil)
      */
      int32_t currentTrackGradient;

      /**
      * Current effective gradient (permil)
      */
      int32_t currentGradient;

      /**
      * sendCycleSpeedAndDistance
      */
      static const uint16_t sendCycleSpeedAndDistance = 1U;

    };
  }
}
#endif

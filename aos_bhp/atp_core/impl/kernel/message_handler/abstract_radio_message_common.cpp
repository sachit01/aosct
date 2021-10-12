/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Implementation of general helper-functions for validation, convertion and
* collecting data in Message Handler.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
* 2016-10-17    arastogi    Add ATP reset, MA timeout, ATP intervention to train status
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "radio_message_types.hpp"
#include "abstract_radio_message_common.hpp"
#include "abstract_position.hpp"

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
    /** Min value for clock field
    */
    static const uint32_t clockFieldMin = 891514000U;

    /** Max value for direction field (3 bits)
    */
    static const uint8_t directionFieldMax = 0x07U;

    /** Max value for train status field (21 bits)
    */
    static const uint32_t trainStatusFieldMax = 0x1FFFFFU;

    /** Max value for ATO mode field
    */
    static const uint8_t  atoModeFieldMax = 4U;

    /** Max value for timeout execution field
    */
    static const uint8_t  timeoutValidFieldMax = 254U;

    /** Mask Bit 0, B_DIRECTION
    */
    static const uint8_t DrivingOrientationMask = 0x01U;

    /** Mask Bit 1, B_DIRECTION
    */
    static const uint8_t OrientationInTrackMask = 0x02U;

    /** Mask Bit 2, B_DIRECTION
    */
    //lint -esym(752,ATP::Kernel::LocomotiveOrientationMask) Might be needed by adaptation
    static const uint8_t LocomotiveOrientationMask = 0x04U;

    /** Bit 0 Value 0, B_DIRECTION
    */
    static const uint8_t DrivingDirectionForward = 0U;

    /** Bit 0 Value 1, B_DIRECTION
    */
    //lint -esym(752,ATP::Kernel::DrivingDirectionReverse) Might be needed by adaptation
    static const uint8_t DrivingDirectionReverse = 1U;

    /** Bit 1 Value 0, B_DIRECTION
    */
    static const uint8_t OrientationTrackLocoToLeg1 = 0U;

    /** Bit 1 Value 1, B_DIRECTION
    */
    //lint -esym(752,ATP::Kernel::OrientationTrackLocoToLeg0) Might be needed by adaptation
    static const uint8_t OrientationTrackLocoToLeg0 = 1U;

    /** Bit 2 Value 0, B_DIRECTION
    */
    //lint -esym(752,ATP::Kernel::LocoOrientationBEndFacingCars) Might be needed by adaptation
    static const uint8_t LocoOrientationBEndFacingCars = 0U;

    /** Bit 2 Value 1, B_DIRECTION
    */
    //lint -esym(752,ATP::Kernel::LocoOrientationAEndFacingCars) Might be needed by adaptation
    static const uint8_t LocoOrientationAEndFacingCars = 1U;

    /**
    * Max size of binary data to trace.
    */
    static const uint16_t MaxBytesInTraceBuffer = 1024U;

    /******************************************************************************
    * traceMessageData
    ******************************************************************************/
    void traceBinaryData(ATC::TraceInterface const * const trace, const uint8_t level, const uint8_t data[], const uint16_t dataLength)
    {
      uint8_t currentTraceLevel = 0U;
      bool isEnabled = false;

      trace->getTraceDetails(currentTraceLevel, isEnabled);

      // Only allocate this large buffer on stack if needed
      if ((currentTraceLevel >= level) && isEnabled)
      {
        const uint16_t charsPerByte = 3U;
        char_t tmpTraceBuffer[(MaxBytesInTraceBuffer * charsPerByte) + 1U]; // add one for NUL termination
        tmpTraceBuffer[0] = '\0';

        // Max number of bytes are MaxBytesInTraceBuffer
        uint16_t nrOfBytes = ((dataLength > MaxBytesInTraceBuffer) ? MaxBytesInTraceBuffer : dataLength);
        bool errorInString = false;

        for (uint16_t i = 0U; i < nrOfBytes; ++i)
        {
          //lint -e{586} snprintf is needed here
          const int32_t retVal = snprintf(&tmpTraceBuffer[charsPerByte * i], charsPerByte + 1U, "%02X:", data[i]); // add one for NUL termination

          if ((retVal > 0) && (static_cast<size_t>(retVal) <= charsPerByte))
          {
            // OK
          }
          else
          {
            errorInString = true;
          }
        }

        if (!errorInString)
        {
          trace->write(ATC::veryDetailedTrace, &tmpTraceBuffer[0]);
        }
      }
    }

    /******************************************************************************
    * dirAndOrientation2TravelDir
    ******************************************************************************/
    TravelDir dirAndOrientation2TravelDir(const uint8_t dirAndOrientation)
    {
      return ((dirAndOrientation & DrivingOrientationMask) == DrivingDirectionForward) ?
        DirForward : DirReverse;
    }

    /******************************************************************************
    * dirAndOrientation2OdoDir
    ******************************************************************************/
    OdoDir dirAndOrientation2OdoDir(const uint8_t dirAndOrientation)
    {
      return ((dirAndOrientation & OrientationInTrackMask) == OrientationTrackLocoToLeg1) ?
        OdoPositive : OdoNegative;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    AbstractRadioMessageCommon::~AbstractRadioMessageCommon()
    {
    }

    /******************************************************************************
    * readTRACK_DATA
    ******************************************************************************/
    void AbstractRadioMessageCommon::readTRACK_DATA(VFW_Buffer& buffer, TrackData& trackData) const
    {
      trackData.track = vfwGetU16(&buffer);
      trackData.length = vfwGetU32(&buffer);
      trackData.bdirection = vfwGetU8(&buffer);
      trackData.trvDir = dirAndOrientation2TravelDir(trackData.bdirection);
      trackData.odoDir = dirAndOrientation2OdoDir(trackData.bdirection);
      trackData.previousTrack = vfwGetU16(&buffer);
    }

    /******************************************************************************
    * Start validateXXX functions
    ******************************************************************************/

    bool AbstractRadioMessageCommon::validateNID_BG(const uint16_t val) const
    {
      return (val >= 1U) && (val <= 16383U);
    }

    bool AbstractRadioMessageCommon::validateT_CLOCK(const uint64_t val) const
    {
      return(val >= clockFieldMin);
    }

    bool AbstractRadioMessageCommon::validateQ_POWER(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(PowerUp));
    }

    bool AbstractRadioMessageCommon::validateQ_LOGON_STATUS(const uint8_t val) const
    {
      return(val <= (static_cast<uint8_t>(DriverLogonSuccesful)));
    }

    bool AbstractRadioMessageCommon::validateN_VALUE(const uint16_t val) const
    {
      return (val >= 1U);
    }

    bool AbstractRadioMessageCommon::validateQ_ACKNOWLEDGE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(RequestAcknowledged));
    }

    bool AbstractRadioMessageCommon::validateIncomingQ_PROTOCOL_RESPONSE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(TCCResponseUnrecoverableMisMatch));
    }

    bool AbstractRadioMessageCommon::validateOutgoingQ_PROTOCOL_RESPONSE(const uint8_t val) const
    {
      return((val == static_cast<uint8_t>(AOSResponseMatch)) || (val == static_cast<uint8_t>(AOSResponseMisMatch)));
    }

    bool AbstractRadioMessageCommon::validateQ_ABORT(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(AbortedByDriver));
    }

    bool AbstractRadioMessageCommon::validateB_DIRECTION(const uint8_t val) const
    {
      return(val <= directionFieldMax);
    }

    bool AbstractRadioMessageCommon::validateM_BRAKE_SYSTEM(const uint8_t val) const
    {
      //TODO for adaptation
      return(val <= 3U);
    }

    bool AbstractRadioMessageCommon::validateQ_TS_STATE(const uint8_t val) const
    {
      return((val == static_cast<uint8_t>(TrainSetupStateTemporary)) ||
        (val == static_cast<uint8_t>(TrainSetupStatePermanent)));
    }

    bool AbstractRadioMessageCommon::validateT_VALID(const uint8_t val) const
    {
      return(val <= timeoutValidFieldMax);
    }

    bool AbstractRadioMessageCommon::validateQ_ROUTE_TYPE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(RtStaffResponsible));
    }

    bool AbstractRadioMessageCommon::validateM_LOADED(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(TrainIsLoaded));
    }

    bool AbstractRadioMessageCommon::validateN_ADHESION(const uint8_t val) const
    {
      bool retValue = false;
      if ((val >= 50U) && (val <= 100U))
      {
        retValue = true;
      }

      return retValue;
    }

    bool AbstractRadioMessageCommon::validateQ_SPEED(const uint8_t val) const
    {
      return((val <= static_cast<uint8_t>(ScrRestrictiveSection)) || (val == static_cast<uint8_t>(ScrOther)));
    }

    bool AbstractRadioMessageCommon::validateQ_TRACK_DATA_TYPE(const uint8_t val) const
    {
      return (val >= static_cast<uint8_t>(TrackDataTypePowerSectionLimit))
        &&   (val <= static_cast<uint8_t>(TrackDataTypeAcousticSignal));
    }

    bool AbstractRadioMessageCommon::validateQ_DIRECTION(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(ValidDirectionBoth));
    }

    bool AbstractRadioMessageCommon::validateNID_VEHICLE_TYPE(const uint8_t /*val*/) const
    {
      return true; // All values are valid since VehicleFutureExpansionAdaptationMax is 255);
    }

    bool AbstractRadioMessageCommon::validateQ_SETUP(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(TrainSetupReposition));
    }

    bool AbstractRadioMessageCommon::validateQ_CONFIG_SOURCE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(AutoCollectedByTic));
    }

    bool AbstractRadioMessageCommon::validateQ_TIMS_AVAILABLE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(TimsAvailable));
    }

    bool AbstractRadioMessageCommon::validateQ_TIMS_SUPERVISION(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(TimsSupReq));
    }

    bool AbstractRadioMessageCommon::validateQ_POSITION(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(Pos::PosDoubtfull));
    }

    bool AbstractRadioMessageCommon::validateB_TRAIN_STATUS(const uint32_t val) const
    {
      return(val <= trainStatusFieldMax);
    }

    bool AbstractRadioMessageCommon::validateQ_ATP_MODE(const uint8_t val) const
    {
      return(val < static_cast<uint8_t>(ATPModesCount));
    }

    bool AbstractRadioMessageCommon::validateQ_ATO_MODE(const uint8_t val) const
    {
      return(val <= atoModeFieldMax);
    }

    bool AbstractRadioMessageCommon::validateQ_INITIATE(const uint8_t val) const
    {
      return(val <= static_cast<uint8_t>(ConfigKnownByTCC));
    }

    /******************************************************************************
    * End validateXXX functions
    ******************************************************************************/
  }
}

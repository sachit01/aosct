#ifndef AbstractRadioMessageCommon_hpp
#define AbstractRadioMessageCommon_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Definitions of general helper-functions for validation, convertion and
* collecting data in Message Handler.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "trace_interface.hpp"
#include "atp_types.hpp"
#include "radio_message_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Trace binary data in hex-format.
    *
    *  @param[in] trace       Trace interface to be used
    *  @param[in] level       Tracelevel
    *  @param[in] data        Pointer to binarydata to trace
    *  @param[in] dataLength  The number of bytes to trace
    *
    */
    void traceBinaryData(ATC::TraceInterface const * const trace, const uint8_t level, const uint8_t data[], const uint16_t dataLength);

    /**
    * Convert bit 0 in B_DIRECTION to TravelDir type
    *
    *  @param[int] dirAndOrientation  B_DIRECTION value
    *
    *  @return Travel direction
    */
    TravelDir dirAndOrientation2TravelDir(uint8_t dirAndOrientation);

    /**
    * Convert bit 1 in B_DIRECTION to OdoDir type
    *
    *  @param[int] dirAndOrientation  B_DIRECTION value
    *
    *  @return Orientation in track
    */
    OdoDir dirAndOrientation2OdoDir(uint8_t dirAndOrientation);

    /**
    * The base class for incoming and outgoing radio messages.
    */
    class AbstractRadioMessageCommon
    {
    public:

      /** End of message containing optional number of parameters */
      static const uint8_t M_END_OF_MESSAGE = 0U;

      /** Enum of the data processing state
      */
      enum DataProcessState
      {
        NoDataAvailable = 0,    //<! No data is available for processing
        DataAvailable,          //<! Data is available for validation
        DataValidated           //<! Data is validated and published
      };

      /**
      * Virtual Destructor for the parser base class
      */
      virtual ~AbstractRadioMessageCommon();

    protected:

      /**
      * Populates a TrackData object by reading TRACK_DATA from a buffer.
      *
      * @param[in]  buffer    buffer to read from
      * @param[out] trackData object to populate
      */
      virtual void readTRACK_DATA(VFW_Buffer& buffer, TrackData& trackData) const;

      /**
      * Validates the NID_BG parameter
      *
      * @param[in] val Value to be validated
      *
      * @return True if value is within limits
      */
      virtual bool validateNID_BG(const uint16_t val) const;

      /**
      * Validating the T_CLOCK parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateT_CLOCK(const uint64_t val) const;

      /**
      * Validating the Q_POWER parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_POWER(const uint8_t val) const;

      /**
      * Validating the Q_LOGON_STATUS parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_LOGON_STATUS(const uint8_t val) const;

      /**
      * Validating the N_VALUE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateN_VALUE(const uint16_t val) const;

      /**
      * Validating the Q_ACKNOWLEDGE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_ACKNOWLEDGE(const uint8_t val) const;

      /**
      * Validating the incoming Q_PROTOCOL_RESPONSE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateIncomingQ_PROTOCOL_RESPONSE(const uint8_t val) const;

      /**
      * Validating the outgoing Q_PROTOCOL_RESPONSE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateOutgoingQ_PROTOCOL_RESPONSE(const uint8_t val) const;

      /**
      * Validating the Q_ABORT parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_ABORT(const uint8_t val) const;

      /**
      * Validating the B_DIRECTION parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateB_DIRECTION(const uint8_t val) const;

      /**
      * Validating the T_VALID parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateT_VALID(const uint8_t val) const;

      /**
      * Validating the Q_TS_STATE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_TS_STATE(const uint8_t val) const;

      /**
      * Validating the Q_ROUTE_TYPE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_ROUTE_TYPE(const uint8_t val) const;

      /**
      * Validating the M_LOADED parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateM_LOADED(const uint8_t val) const;

      /**
      * Validating the N_ADHESION parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateN_ADHESION(const uint8_t val) const;

      /**
      * Validating the Q_SPEED parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_SPEED(const uint8_t val) const;

      /**
      * Validating the Q_TRACK_DATA_TYPE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_TRACK_DATA_TYPE(const uint8_t val) const;

      /**
      * Validating the Q_DIRECTION parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_DIRECTION(const uint8_t val) const;

      /**
      * Validating the NID_VEHICLE_TYPE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateNID_VEHICLE_TYPE(const uint8_t val) const;

      /**
      * Validating the Q_SETUP parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_SETUP(const uint8_t val) const;

      /**
      * Validating the Q_CONFIG_SOURCE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_CONFIG_SOURCE(const uint8_t val) const;

      /**
      * Validating the Q_TIMS_AVAILABLE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_TIMS_AVAILABLE(const uint8_t val) const;

      /**
      * Validating the Q_TIMS_SUPERVISION parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_TIMS_SUPERVISION(const uint8_t val) const;

      /**
      * Validating the M_BRAKE_SYSTEM parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateM_BRAKE_SYSTEM(const uint8_t val) const;

      /**
      * Validating the Q_POSITION parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_POSITION(const uint8_t val) const;

      /**
      * Validating the B_TRAIN_STATUS parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateB_TRAIN_STATUS(const uint32_t val) const;

      /**
      * Validating the Q_ATP_MODE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_ATP_MODE(const uint8_t val) const;

      /**
      * Validating the Q_ATO_MODE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_ATO_MODE(const uint8_t val) const;

      /**
      * Validating the Q_INITIATE parameter
      *
      *  @param[in] val Value to be validated
      *
      *  @return True if value is within limits
      *
      */
      virtual bool validateQ_INITIATE(const uint8_t val) const;
    };
  }
}
#endif

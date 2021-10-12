#ifndef OBRDMessageCommon_hpp
#define OBRDMessageCommon_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
*  DESCRIPTION:
* This file defines the OBRD Message Format related functions

*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "channel_config.hpp"

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {

    //Constant values for the sender and receiver id header fields for OBRD Message
    /**
    * Sender Id
    */
    static const char_t* senderId = "AUS/BHPB/AOS/4314";

    /**
    * Receiver Id
    */
    static const char_t* receiverId = "AUS/BHPB/OBRD/4314";

    /**
    * Maximum Packet/data size
    */
    static const uint8_t maxObrdDataSize = 240U;

    static const uint8_t obrdProtocolVersionMajor = 1U;
    static const uint8_t obrdProtocolVersionMinor = 11U;

    /**
    * OBRDPacket types between AOS-OBRD
    */
    typedef uint16_t OBRDPacketType;

    static const OBRDPacketType obrdPTypeProtocolVersion = 1U;
    static const OBRDPacketType obrdPTypeMessageReject = 2U;
    static const OBRDPacketType obrdPTypeUnitStatus = 10U;

    /**
    * OBRD Message types between AOS-OBRD
    */
    typedef uint16_t OBRDMessageType;

    //Incoming Messages
    static const OBRDMessageType obrdMTypeInMin = 1U;
    static const OBRDMessageType obrdMTypeInProtocolVersion = 1U;
    static const OBRDMessageType obrdMTypeInUnitStatus = 2U;
    static const OBRDMessageType obrdMTypeInMax = 3U;

    //Outgoing Message
    static const OBRDMessageType obrdMTypeOutMin = 10U;
    static const OBRDMessageType obrdMTypeOutProtocolVersion = 10U;
    static const OBRDMessageType obrdMTypeOutRejectMessage = 11U;
    static const OBRDMessageType obrdMTypeOutMax = 12U;

    /**
    * Rejection Reason
    */
    enum RejectReason
    {
      OBRDRejectReasonUndefined = 0,
      OBRDRejectReasonWrongProtocolVersion = 1,
      OBRDRejectReasonWrongSenderIdentity = 2,
      OBRDRejectReasonWrongReceiverIdentity = 3,
      OBRDRejectReasonWrongLocomotiveIdentity = 4,
      OBRDRejectReasonWrongSiteId = 5,
      OBRDRejectReasonConnectionTimedOut = 6,
      OBRDRejectReasonProtocolVersionNotInit = 7
    };

    /**
    * OBRD Data packet
    * Contains Data header + different messages
    */
    struct OBRDDataPacket
    {
      /**
      * Maximum length of OBRD packet header
      * RNID_PACKET + RL_PACKET = 4 byte
      */
      static const uint8_t headerSize = 4U;

      /**
      Message Id
      */
      OBRDPacketType packetType;

      /**
      * Packet size
      * Includes header + data length
      */
      uint16_t packetSize;

      /**
      * Data length
      */
      uint16_t dataLength;

      /**
      * Variable part of the packet
      */
      uint8_t  messageData[maxObrdDataSize];

      /**
      * Default constructor.
      */
      OBRDDataPacket();
    };


    /**
    * OBRD Message = Safety Header + OBRD Data packets
    */
    class OBRDMessage
    {

    public:

      /**
      * Maximum length of the identity strings
      */
      static const uint8_t maxVariableStringLen = 80U;

      /**
      * Maximum OBRD Fixed header Length
      * RL_MESSAGE + RNID_SITE + RNID_LOCOMOTIVE + RT_TIMESTAMP = 14 byte
      */
      static const uint8_t headerSize = 14U;

      /**
      * Length of CRC field 8 byte
      */
      static const uint8_t crcSize = 8U;

      /** Constructor
      *  Initialize data length to 0
      *  in case object is copied
      */
      OBRDMessage();

    private:

      /**
      * Copy constructor (disabled)
      */
      OBRDMessage(const OBRDMessage & msg);

      /**
      * Assignment operator (disabled)
      */
      OBRDMessage& operator = (const OBRDMessage& msg);

    public:

      /**
      * Identity of site
      */
      uint16_t siteId;

      /**
      * Receiver Identity
      */
      char_t receiverId[maxVariableStringLen];

      /**
      * Sender Identity
      */
      char_t senderId[maxVariableStringLen];

      /**
      * Identity of locomotive
      */
      uint16_t locoId;

      /**
      * Timestamp from sender
      */
      uint64_t tSender;

      /**
      * OBRD Packet Data
      */
      OBRDDataPacket obrdPacketData;
    };

    /**
    * OBRD Unit Status Message
    */
    struct OBRDUnitStatusReport
    {
      TrackAndPos unitTrackAndPos;  //!< Location of the OBRD unit
      uint64_t timeOfMeasurement;   //!< Time since 1970-01-01 00:00:00 UTC [s]
      uint8_t lastCarBrakePressure; //!< Brake pressure [psig]

      OBRDUnitStatusReport();
    };
  }
}

#endif

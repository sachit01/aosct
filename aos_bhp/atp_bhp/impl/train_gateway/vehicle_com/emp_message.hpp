#ifndef EmpMessage_hpp
#define EmpMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the EMP Message Protocol related function calls.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 19-01-2017    adgupta     Created
*******************************************************************************/
#include <vfw_buffer.h>

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_buffer.h>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    /**
    * EMP related variables as needed by the component to pack and parse the EMP message
    */
    struct EMPMsgHeader
    {
      uint8_t  protocolVersion;     //!< Version of EMP header
      uint16_t msgID;               //!< Application ID/Message Type
      uint8_t  msgVersion;          //!< Application msg version:
      uint8_t  flags;               //!< Options used in constructing Header
      uint32_t dataLen;             //!< Size of the msg Body
      uint32_t msgNumber;           //!< Message sequence number
      uint32_t msgTime;             //!< Time of message creation
      uint8_t  variableHeaderSize;  //!< Size of variable portion of header        
    };

    /**
    * Errors while parsing the EMP message
    */
    enum EMPParseError
    {
      NoError = 0,
      IncorrectProtocolVersion,
      IncorrectFlag,
      IncorrectMsgNum,
      IncorrectMsgTime,
      IncorrectVariableHeader,
      IncorrectCRC
    };

    class EmpMsg
    {
    public:

      /**
      * Default constructor
      */
      EmpMsg();

      /**
      * This function adds EMP protocol Header and Footer to the EMP message packet
      *
      * @param[in] msgID - Application Message ID
      * @param[in] msgVersion - Application message version
      * @param[in] dataLen - Size of Message Body
      *
      * @return - returns true if dataLen is within limit
      */
      bool addEMPEnvelope(const uint16_t msgID, const uint8_t msgVersion, const uint32_t dataLen);

      /**
      * This function parses the EMP message off its Header and Footer
      *
      * @param[out] msgID - message ID of the parsed message
      * @param[out] msgVersion - message Version of the parsed message
      *
      * @return - returns the error Enum for parsing
      */
      EMPParseError parseEMPMessage(uint16_t &msgID, uint8_t &msgVersion);

      /**
      * This returns the pointer to the buffer created by the EMP message class.
      * This should be used by the Vehicle Com to get the messages via VFW OR register on VFW.
      *
      * @return - pointer to the buffer created by EMP
      */
      uint8_t* getEMPBuffer();

      /**
      * This returns the pointer to the buffer created by the EMP message class.
      * This should be used by the Vehicle Com to get the messages via VFW OR register on VFW.
      *
      * @return - pointer to the buffer created by EMP
      */
      const uint8_t* getEMPBuffer() const;

      /**
      * This returns the pointer to theEMP Body. This should be the location where Vehicle Com will store the application level message.
      *
      * @return - pointer to the EMP message body buffer created by EMP
      */
      uint8_t* getEMPBodyBuffer();

      /**
      * Get the EMP message Header of sending/receiving EMP message
      *
      * @return - Returns the EMP message header
      */
      EMPMsgHeader getEMPMsgHeader() const;

      /**
      * Get the EMP message actual length (Header + ActualBody + Trail)
      *
      * @return - Returns the actual length for whole EMP message
      */
      uint32_t getEMPMessageActualLen() const;

      /**
      * Get the EMP message maximum total length that can be allocated (Header + MaxBody + Trail)
      *
      * @return - Returns the maximum length for whole EMP message
      */
      uint32_t getEMPMessageMaxTotalLen() const;

      /**
      * Get the EMP message maximum total length that can be allocated (MaxBody)
      *
      * @return - Returns the maximum EMP Body length
      */
      uint32_t getEMPMessageMaxBodyLen() const;

    private:

      /**
      * EMP message structure pointing to the buffer at Header and Body Pointer
      */
      struct EMPMsgPtr
      {
        uint8_t *empMsgHeaderPtr; //!< Pointer pointing to the buffer at Message Header Index
        uint8_t *empMsgBodyPtr;   //!< Pointer pointing to the buffer at Message Body Index
      };

      /**
      * Max Size of the EMP Message.
      * EMP Header + Data(ECPB Train Composition Message) + EMP Trail
      * 17 + (6 + 350*3)  + 4 = 1077
      */
      static const uint16_t maxEMPMsgBuffSize = 1100U;

      /**
      * Size of the EMP Message Header in Bytes
      */
      static const uint8_t sizeEMPHeader = 17U;

      /**
      * Size of the EMP Message Footer(CRC only) in Bytes
      */
      static const uint8_t sizeEMPFooter = 4U;

      /**
      * Time difference between UTC time for a message to be sent from LCS and received in ATP in sec.(Not final)
      */
      static const uint32_t timeDiffUTC = 5U;

      /**
      * Version number to be used for EMP protocol for ATP
      */
      static const uint8_t empProtocolVersion = 0x04U;

      /**
      * Flag value to be used in ATP
      *
      * Values used here:
      * TimeStamp Bit 0 - 1
      * Encryption Bit 1 - 0
      * Compression Bit 2 - 0
      * Data Integrity Bit 3,4 - 1
      */
      static const uint8_t empFlag = 0x09U;

      /**
      * Variable Header size to be used for ATP
      */
      static const uint8_t variableHdrSize = 0x0U;

      /**
      * Message Number to be sent with EMP message
      */
      uint32_t sendMsgNumber;

      /**
      * Message Number received in the EMP message received
      */
      uint32_t recvMsgNumber;

      /**
      * Message Number of previously received EMP message
      */
      uint32_t prevRecvMsgNumber;

      /**
      * The Buffer to pack the message to form an EMP message. EMP Header + EMP Body + EMP Footer(CRC)
      * Same buffer will be used for both sending and receiving EMP messages.
      */
      uint8_t buffer[maxEMPMsgBuffSize];

      /**
      * Object to store pointers to EMP Header and Body
      */
      EMPMsgPtr empMsgPtr;

      /**
      * EMP message header
      */
      EMPMsgHeader empMsgHeader;

      /**
      * Function to calculate the CRC of 
      */
      uint32_t calculateCRC(const uint8_t *start, const uint32_t length) const;

      /**
      * Function to unpack a 24 bit data onto a 32 bit uint variable
      */
      uint32_t unpack24(VFW_Buffer* const bufferunpack) const;
    };
  }
}

#endif

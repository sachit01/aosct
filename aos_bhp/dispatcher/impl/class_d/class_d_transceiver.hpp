#ifndef TransceiverClassD_hpp
#define TransceiverClassD_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
* Defines base-class for sending and receiving messages in the Class D
* protocol connections.
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-05    adgupta     Created
* 2017-02-06   spandita    Created & updated the function declarations
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "class_d_types.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ClassD
{

/**
* \brief Max size of a ClassD Message
*
* \brief Max Cars:350 -> ClassD Header + EMP Header + Data(ECPB Train Composition Message) + EMP Trail + ClassD Trail
* \brief 12 + 17 + (6 + 350*3)  + 4 + 1 = 1090
*/
static const uint32_t  maxMessageLen = 1100U;

  /**
  *  TransceiverData Class
  *
  */
  class TransceiverData
  {
  public:
    uint32_t sendBuffSize;          //<! Size of the message to be sent by Class D
    uint32_t recvBuffSize;          //<! Size of the message to be received by Class D
    uint8_t  protocolVersion;       //<! Version of the Class D protocol for message handling
    uint32_t prevRecvCommId;        //<! Command Id/message number of the last received message
    uint32_t recvCommId;            //<! Received command Id/message number
    uint32_t sendCommId;            //<! Sending Command Id/message number
    MessageType messageType;        //<! Message type of the Sent/received message
    uint8_t messageVersion;         //<! Version number of the message sent/received
    uint8_t buffer[maxMessageLen];  //<! Buffer to store the Class D message 
    uint32_t messageLength;         //<! length of message
   };

  class TransceiverClassD
  {
  private:
    TransceiverData transceiver;                      //<! Object to handle all the sending/receiving related activities for Class D connection
    static const uint8_t stx = 0x02U;                 //<! Start of Message Delimiter
    static const uint8_t etx = 0x03U;                 //<! End of Message Delimiter
    static const uint8_t maxClassDFooterSize = 1U;    //<! Size of Class D footer
    static const uint8_t maxClassDHeaderSize = 12U;   //<! Size of Class D Header
    static const uint8_t classDProtocolVer = 0x02U;   //<! Class D protocol version number 
    static const uint8_t classDMsgVer = 2U;           //<! Class D message version number 
    static const MessageType classDMsgType = DataMsg; //<! Type of message - Data type

  protected:

  public:
    /**
    * Default constructor
    */
    TransceiverClassD();

    /**
    * Parses the given received message
    *
    * @param[in] receivedData    pointer to the received bytes
    * @param[in] receivedLength  the number of received bytes
    *
    * @return  returns error type
    */
    ErrorType parseMessage(uint8_t* const receivedData, const uint32_t receivedLength);

    /**
    * Builds a Class D message packet by copying the given bytes to the body of this message
    * and adding Class D protocol header and footer.
    *
    * @param[in] body    pointer to the bytes of the body
    * @param[in] length  the number of bytes in the body
    *
    * @return returns true if successful, i.e. if length is within limit (see @ref getClassDMessageMaxBodyLen)
    */
    bool buildMessage(const uint8_t* const body, const uint32_t length);

    /**
    * This returns the pointer to Class D Body.
    *
    * @return - pointer to the Class D message body buffer created by Class D
    */
    const uint8_t* getClassDBodyPtr() const;

    /**
    * This returns the pointer to the buffer created by the CLass D message class.
    * This should be used by the message dispatcher to get the messages .
    *
    * @return - pointer to the buffer created by Class D
    */
    const uint8_t* getClassDBufferPtr() const;
  
    /**
    * Get the Class D message actual length (Header + ActualBody + Trail)
    *
    * @return - Returns the actual length for whole CLass D message
    */
    uint32_t getClassDMessageActualLen() const;
   
    /**
    * Get the message length of message Body
    *
    * @return - Returns the actual length for message body
    */
    uint32_t getActualMsgLen() const;

    /**
    * Reset the COMMID/message number 
    *
    */
    void resetCommID(void);
   
    /**
    * Get the Class D message maximum total length that can be allocated (MaxBody)
    *
    * @return - Returns the maximum Class D Body length
    */
    uint32_t getClassDMessageMaxBodyLen(void) const;

    /**
    * Get the Class-D message maximum total length that can be allocated (Header + MaxBody + Tail)
    *
    * @return - Returns the maximum length for whole Class-D message
    */
    uint32_t getClassDMessageMaxTotalLen() const;
  };
}

#endif

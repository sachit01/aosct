#ifndef ChannelConfigDispatcher_hpp
#define ChannelConfigDispatcher_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file defines the  structures for VFW  Channel (Sync/No-Sync),IP address,flags and port number
*  Which will be used by message dispatcher
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-30    spandita    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "vfw_sync.h"
#include "atc_types.hpp"
#include "vfw_identity.h"

namespace Dispatcher
{

  /**
  * Structure for read channel descriptor(ATP to dispatcher)
  */
  struct ChannelReadDesc
  {
    VFW_ChannelDesc  radioReadChannelDescA1; //!<descriptor for the  read VFW radio  channelA1
    VFW_ChannelDesc  radioReadChannelDescA2; //!<descriptor for the  read VFW radio  channelA2
    VFW_ChannelDesc  radioReadChannelDescA3; //!<descriptor for the  read VFW radio  channelA3
    VFW_ChannelDesc  radioReadChannelDescB1; //!<descriptor for the  read VFW radio  channelB1
    VFW_ChannelDesc  radioReadChannelDescB2; //!<descriptor for the  read VFW radio  channelB2
    VFW_ChannelDesc  radioReadChannelDescB3; //!<descriptor for the  read VFW radio  channelB3
    VFW_ChannelDesc  opcReadChannelDescA;    //!<descriptor for the  read VFW OPC channelA
    VFW_ChannelDesc  opcReadChannelDescB;    //!<descriptor for the  read VFW OPC  channelB
    VFW_ChannelDesc  viohReadChannelDescA;   //!<descriptor for the  read VFW VIOH channelA
    VFW_ChannelDesc  viohReadChannelDescB;   //!<descriptor for the  read VFW VIOH channelB
    VFW_ChannelDesc  ligReadChannelDescA;    //!<descriptor for the  read VFW LIG channelA
    VFW_ChannelDesc  ligReadChannelDescB;    //!<descriptor for the  read VFW LIG channelB
    VFW_ChannelDesc  dmiReadChannelDescA1;   //!<descriptor for the  read VFW DMI channelA1
    VFW_ChannelDesc  dmiReadChannelDescA2;   //!<descriptor for the  read VFW DMI channelA2
    VFW_ChannelDesc  dmiReadChannelDescB1;   //!<descriptor for the  read VFW DMI channelB1
    VFW_ChannelDesc  dmiReadChannelDescB2;   //!<descriptor for the  read VFW DMI channelB2
  };

  /**
  * Structure for write  channel descriptor(dispatcher to ATP )
  */
  struct ChannelWriteDesc
  {
    VFW_ChannelDesc  radioWriteChannelDesc1A;//!<descriptor for the  radio write channelA1
    VFW_ChannelDesc  radioWriteChannelDesc2A;//!<descriptor for the  radio write channelA2
    VFW_ChannelDesc  radioWriteChannelDesc3A;//!<descriptor for the  radio write channelA3
    VFW_ChannelDesc  radioWriteChannelDesc1B;//!<descriptor for the  radio write channelB1
    VFW_ChannelDesc  radioWriteChannelDesc2B;//!<descriptor for the  radio write channelB2
    VFW_ChannelDesc  radioWriteChannelDesc3B;//!<descriptor for the  radio write channelB3
    VFW_ChannelDesc  opcWriteChannelDescA;   //!<descriptor for the  OPC write channelA
    VFW_ChannelDesc  opcWriteChannelDescB;   //!<descriptor for the  OPC write channelB
    VFW_ChannelDesc  codWriteChannelDescA;   //!<descriptor for the  COD write channelA
    VFW_ChannelDesc  codWriteChannelDescB;   //!<descriptor for the  COD write channelB
    VFW_ChannelDesc  viohWriteChannelDescA;  //!<descriptor for the  VIOH write channelA
    VFW_ChannelDesc  viohWriteChannelDescB;  //!<descriptor for the  VIOH write channelB
    VFW_ChannelDesc  ligWriteChannelDescA;   //!<descriptor for the  LIG write channelA
    VFW_ChannelDesc  ligWriteChannelDescB;   //!<descriptor for the  LIG write channelB
    VFW_ChannelDesc  dmiWriteChannelDesc1A;  //!<descriptor for the  DMI write channelA1
    VFW_ChannelDesc  dmiWriteChannelDesc2A;  //!<descriptor for the  DMI write channelA2
    VFW_ChannelDesc  dmiWriteChannelDesc1B;  //!<descriptor for the  DMI write channelB1
    VFW_ChannelDesc  dmiWriteChannelDesc2B;  //!<descriptor for the  DMI write channelB2
  };

  /**
  * Max size of Input buffer for messages from ATP A/B
  */
  static const uint16_t maxInputMessageSize = 9000U;

  /**
  * Max size of Input temporary buffer for messages from ATP A/B
  * Will be updated at the time of implementation
  */
  static const uint16_t maxInputTempMessageSize = 1000U;
  
  /**
  * Different data types handled
  */
  enum DataType
  {
    NormalData = 0U,
    LigData = 1U,
    MiscData = 2U,
    SplData = 3U
  };
  
  /*
  * Information regarding each VFW-connection to ATP-A/B
  */

  struct ConnectionItem
  {
    uint8_t side;                                  //!< ATP-A/B
    uint32_t noOfBytesReceivedFromATP;             //!< Number of bytes read from readChannelName
    uint16_t noOfMessagesReceivedFromATP;          //!< Number of messages read from readChannelName
    const char_t *readChannelName;                 //!< Read Channel Name
    const char_t *writeChannelName;                //!< Write Channel Name
    VFW_ChannelDesc readChannelDescr;              //!< Descriptor for opened channel in read mode
    VFW_ChannelDesc writeChannelDescr;             //!< Descriptor for opened channel in write mode
    uint32_t maxMessageReadLen;                    //!> Maximum size of read message
    uint32_t maxMessageWriteLen;                   //!> Maximum size of write message
    uint32_t noOfBytesSentToATP;                   //!< Number of bytes written to writeChannelName
    uint32_t noOfMessageSentToATP;                 //!< Number of messages written to writeChannelName
    uint32_t writeQueueSize;                       //!< Queue size for write channel
    uint32_t readQueueSize;                        //!< Queue size for read channel
    uint8_t connId;                                //!> Connection ID
    DataType dataType;                             //!> Data type used
    uint8_t lastUdpSequenceNumber;                 //!> Use UDP sequence number
    bool error;                                    //!> Error in connection
  };
}
#endif

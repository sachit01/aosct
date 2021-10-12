#ifndef MessageDispatcher_hpp
#define MessageDispatcher_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file defines MessageDispatcher class which contains the functionalities of
* of the MessageDispatcher
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-30    spandita    Created
* 2016-11-26    saprasad    Make static msgDispId to remove error in linux
* 2016-01-04    spandita    updated the code with required functionalities
* 2017-03-29    marlundg    Refactoring of connectionItems, added TCC3, OPC, LIG(Client)
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "channel_config.hpp"
#include "channel_config_dispatcher.hpp"
#include "atc_base.hpp"
#include "event.hpp"
#include "class_d_transceiver.hpp"
#include "basic_ip.hpp"

namespace Dispatcher
{
  class MessageDispatcher;

  /**
   * Static trace pointer due to callback function defined for input messages from ATP to Disp
   */
  static ATC::TraceInterface *tracePtr = static_cast<ATC::TraceInterface *>(NULL);
  
  /**
  * Class-D Transceiver
  */
  static ClassD::TransceiverClassD classD;

  /*
  * Buffer handling structure for inputs from ATP-A/B
  */
  struct OutgoingDispMsg
  {
    uint8_t inputBuffer[maxInputMessageSize];
    uint32_t size;
    uint8_t connId;
    int64_t timestamp;
  };

  /**
  * Error handling for OutgoingDispMsgQueueType
  */
  class DispGPErrorHandler
  {
  public:
    static void report(const char_t * const str)
    {
      ATC::TraceInterface trace("OutgoingDispMsgQueueType", 1U, str);
      trace.write(1U, str);
    }
  };

  /*
  * List-type for outgoing messages coming from ATP-A/B
  */
  typedef ATC::GPList<OutgoingDispMsg, 15U, DispGPErrorHandler> OutgoingDispMsgQueueType;

  /**
  * The class MessageDispatcher implements the interface defined by the ComponentBase class and IOComponent class.
  *
  */
  class MessageDispatcher : public ATC::IOComponent
  {

  public:

    /**
    * Max waiting time for 2nd part of message from other ATP (in ms)
    */
    static const int64_t maxWaitMsgFromAtp = 100;

    /**
    * A-Side in connectionItems
    */
    static const uint8_t A_SIDE = 0U;

    /**
    * B-Side in connectionItems
    */
    static const uint8_t B_SIDE = 1U;

    /**
    * component ID for message dispatcher
    */
    static const ATC::ComponentIDType msgDispId = 151U; //!< message dispatcher ID 

   /**
    * Implements the virtual preInit function.
    *
    */
    virtual void preInit(void);

    /**
    *Implements the Init function.
    *
    * @return Returns true when initialization completely successful.
    */
    virtual bool init(void);

    /**
    * Implements the runIn function.
    */
    virtual void runIn(void);

    /**
    * Implements the runOut function.
    */

    virtual void runOut(void);

    /**
   * Singleton instance.
   * Only one instance of this class is allowed.
   * @return the one and only instance.
   *
   */
    static MessageDispatcher& instance(void);

    /**
    * This functions parses the arguments searches for the "help", or any other Console
    * component specific command calls and handles it. Returns true if completely handled
    * else returns false. returning false will let other components handle the call. help always returns false.
    *
    * @param[in] argc - Number of arguments in the argument array argv
    *
    * @param[in] argv - Arguments array
    *
    * @return - returns true if handled successfully, except "help"(it always returns false)
    */
    virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

    /**
    * Handler for message from ATP A/B to external connection.
    *
    * @param[in] channel  VFW Channel descriptor
    * @param[in] connectionItem     Information about the channel and the destination for the received data
    */
    void handleMessageFromATP(const VFW_SyncChannel channel, ConnectionItem* const connectionItem);

  protected:

  private:


    /**
    * Initializes the connectionItems
    */
    void initConnectionItems();

    /**
    * Set up the ports
    */
    void portInitialization();
    
    /**
    * Function to write in the NJRU log ,the channel name being written on to
    */
    void logChannelWrite(const char_t* const channelName) const;

    /**
    * Function to write in the NJRU log ,the channel name being read from
    * This function needs to be static because it is used in the vfw callbacks
    */
    static void logChannelRead(const char_t* const channelName);

    uint32_t sendLigData(const uint8_t * const messageData, const uint32_t length, const uint8_t connectionId) const;
    uint32_t sendSplMiscData(const uint8_t * const messageData, const uint32_t length, const uint8_t connectionId, const DataType dataType);

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    MessageDispatcher();

    /**
    * Read Channel initialization from dispatcher to ATP A and B
    *
    */
    void readChannelInit(void);
    
    /**
    * Write Channel initialization from dispatcher to ATP A and B
    *
    */
    void writeChannelInit(void);

    /**
    * Implements the mechanism for writing the data to VFW channels
    *
    * @param[in] iD  Connection ID
    * @param[in] buffer buffer to write data to
    * @param[in] length length of message
    */
    void writeToVfwChannel(const uint8_t iD, const uint8_t* const buffer, const uint32_t length);

    /**
    * Send the two message parts from A and B
    *
    * @param[in] msgPartFromATPA message part from A
    * @param[in] msgPartFromATPB message part from B
    */
    void sendMessageFromAB(OutgoingDispMsg& msgPartFromATPA, const OutgoingDispMsg& msgPartFromATPB);

    /**
    * Send the version string to the ATP
    */
    void sendVersionInfoToAtp();

    /**
    * Flag to prevent multiple initialization of ports.
    */
    bool portInitDone;

    /**
    * Maximum length of message from external components like TCC,DMI etc
    */
    static const uint32_t maxExternMsgSize = ATC::maxRadioMessageSize;

    /**
    * Channel write Error
    */
    const ATC::Event errorWriteChannel;

    /**
    * Connection ID Error
    */
    const ATC::Event errorConnectionId;

    /**
    * Error in the UDP sequence
    */
    const ATC::Event errorUdpSequence;

    /**
    * Udp misc data overhead size
    */
    static const uint8_t udpMiscDataLayerOverheadSize = 7U; // 7 bytes is SEQ_NUM + MSG_LEN + NID_PACKET (end) + MSG_LEN (end)

    /**
    * Spl data overhead size
    */
    static const uint8_t splDataLayerOverheadSize = 1U; // 1 byte for SEQ_NUM

    /**
     * class D object
    */
    ClassD::TransceiverClassD classDObj;

    /**
    * Max number of connections and A & B side for each.
    */
    ConnectionItem connectionItems[BasicIP::connectionMax][2U];

    /**
    * Outgoing messages coming from ATP-A/B
    */
    OutgoingDispMsgQueueType outgoingDispMsgQueue[2U];

  };
}
#endif

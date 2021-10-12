#ifndef CrossCompareOutputChannel_hpp
#define CrossCompareOutputChannel_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file defines CrossCompare class which contains the core braking logic 
* used by the AOS. 
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-27    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "cross_compare_output.hpp"
#include <vfwChannel.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/


namespace ATP
{
  namespace Support
  {
    /**
    * Adds cross comparison to a VFW output channel.
    */
    class CrossCompareOutputChannel : public CrossCompareOutput
    {
    public:

      /**
      * Writes the output data to the VFW output channel. May only be called
      * if cross comparison of this data succeeded.
      */
      virtual void commit();

      /**
      * Return a description for the cross compare.
      *
      */
      virtual const char_t* getDescription() const;

      /**
      * Destructor
      */
      virtual ~CrossCompareOutputChannel();

      /**
      * Constructor
      */
      CrossCompareOutputChannel();

      /**
      * Initializes buffers and creates the VFW output channel.
      *
      * @param [in] channelName    Name of the channel.
      * @param [in] nMessages      Max number of messages.
      * @param [in] sizeMessages   Max size of a message.
      * @param [in] divideMessage  Divide the message
      */
      void initChannel(const char_t* const channelName, const uint32_t nMessages, const uint32_t sizeMessages, const bool divideMessage);

    private:
      /** Channel handle returned by vfwChannelOpenWrite to write to OPC Agent.  */
      VFW_ChannelDesc channelWriteDesc;

      /** Channel name  */
      char_t chName[VFW_CH_NAME_MAX_Z];
      /** Channel handle returned by vfwChannelOpenWrite to write to OPC Agent.  */
      bool sendDividedMessage;
    };
  }
}
#endif

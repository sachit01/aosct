#ifndef LCSMessageOutPath_hpp
#define LCSMessageOutPath_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractLCSMessageOut.
*  One creator per message-type.
*  The LCSMessageOutPath creator is responsible for collecting the track-data
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-25    nsyed       Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_lcs_message_out.hpp"
#include "abstract_tracks.hpp"
#include "radio_message_types.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {
    static const uint8_t maxSpeedChanges = 90U;
    //lint -esym(551,ATP::TG::maxSpeedChanges) Lint is wrong, this constant *is* used

    /**
    * Structure for Speed change position block
    */
    struct SpeedChangePositionType
    {
      TrackAndPos trackAndPosition;
      uint16_t     newSpeed;
    };    
    
    /**
    * Structure for Path Message
    */
    struct LCSPathType
    {
      uint16_t                 numberOfTracks;
      uint16_t                 trackId[Kernel::maxPathTracksSize];
      uint16_t                 speedAtBeginningOfPath;
      uint8_t                  numberOfSpeedChanges;
      SpeedChangePositionType  speedChangePos[maxSpeedChanges];
      TrackAndPos              nextTargetTrackAndPos;
      uint32_t                 nextTargetRTA;
      uint16_t                 adsMapVersion; // Byte0 = Major Version, Byte1 = Minor Version
    };

    /**
    * LCSMessageOutPath is a creator for the outgoing Path Message
    */
    class LCSMessageOutPath : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing Path message
      */
      LCSMessageOutPath();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @param[in]  mData   Buffer to be used for validated message in network order
      * @param[out] length  The length of the created output data
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate(EmpMsg* const mData, uint16_t& length);

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();


    private:

      /**
      * Assembles the collected data
      *
      * @param[in] messageData   The incoming message data to be assembled
      * @param[out] appDataLength  The length of the created output data
      *
      * @return true if data is valid with respect to assembling
      */
      bool assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const;

      /**
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      LCSPathType lcsPath;
    };
  }
}
#endif

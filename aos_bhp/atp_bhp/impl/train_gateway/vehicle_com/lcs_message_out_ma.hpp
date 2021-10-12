#ifndef LCSMessageOutMA_hpp
#define LCSMessageOutMA_hpp
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
*  The LCSMessageOutMA creator is responsible for validating and sending out 
*  LCS relevant data from Movement Authority received from TCC in network order.
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

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {
    /**
    * Direction received in MA from TCC
    */
    enum MADir
    {
      MADirForward = 0,
      MADirReverse = 1
    };

    /**
    * Structure for MA message to LCS
    */
    struct LCSMovementAuthorityType
    {
      uint16_t  endOfMATrackId;
      uint32_t   endOfMAPos; // in cm
      MADir     maDir;
      uint16_t   maMargin;
    };

    /**
    * LCSMessageOutMovementAuthority is a creator for the outgoing Movement Authority Message
    */
    class LCSMessageOutMovementAuthority : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing Movement Authority message
      */
      LCSMessageOutMovementAuthority();

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
      LCSMovementAuthorityType lcsMovementAuthority;
    };
  }
}
#endif

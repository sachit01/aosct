#ifndef DMIMessageOutVehicleTypes_hpp
#define DMIMessageOutVehicleTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  One creator per message-type.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutVehicleTypes is a creator for the outgoing Vehicle Type DMIMessage
    */
    class DMIMessageOutVehicleTypes : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Vehicle Types DMIMessage
      */
      DMIMessageOutVehicleTypes();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Vehicle Type data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * Number of Vehicle Type Blocks
      */
      uint8_t noOfVehicleTypeBlocks;

      /**
      * Number of Vehicle Type Blocks
      */
      VehicleTypeBlock vehicleTypes[maxVehicleTypeBlocks];
    };
  }
}
#endif


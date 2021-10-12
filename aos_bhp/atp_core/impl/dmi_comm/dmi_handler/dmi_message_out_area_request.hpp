#ifndef DMIMessageOutAreaRequest_hpp
#define DMIMessageOutAreaRequest_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This is the message for the Area request to be displayed on the DMI.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 19-04-2017    adgupta     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutAreaRequest is a creator for the outgoing Area Request DMIMessage
    */
    class DMIMessageOutAreaRequest : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a area request
      */
      DMIMessageOutAreaRequest();

      /**
       * Validates the collected input data and creates the outgoing message in network byte-order
       *
       * @return true if data is valid and resulted in a valid outgoing message
       */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the area Ids(shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    private:

      /**
      * Maximum number of areas to be 
      */
      static const uint8_t maxAreaIds = 10U;

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * Number of Area Ids in area request message
      */
      uint8_t numAreaIds;

      /**
      * Area Ids 
      */
      uint8_t areaId[maxAreaIds];   // Area Id
    };
  }
}
#endif

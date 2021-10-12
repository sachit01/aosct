#ifndef RadioHandler_hpp
#define RadioHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*              
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-11-13    bhermans    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-15    akushwah    Radio Handler Implementation
* 2016-06-17    akushwah    Incorporated Review Comments
* 2016-08-04    adgupta     Added init flag to limit initialization to be done only once.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>
#include "abstract_radio_handler.hpp"
#include "radio_channel_central.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace RadioCom
  {

    /** The singleton instance of RadioHandler handles creation and scheduling of all radio-channels.
    */  
    class RadioHandler : public AbstractRadioHandler
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      * Register vfw channels in sync handler.
      */
      virtual void preInit(void);

      /**
      * Implements the virtual init function.
      *
      * Create the RadioChannel objects and store in container
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /** Singleton instance.
      *
      *  Only one instance of this class is allowed.
      *  @return the one and only instance.
      * 
      */
      static RadioHandler& instance(void);

    protected:

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /** Radio Channel for TCC1
      */
      RadioChannelCentral radioChannel1;

      /** Radio Channel for TCC2
      */
      RadioChannel radioChannel2;

      /** Radio Channel for TCC3
      */
      RadioChannel radioChannel3;

      /**
      * Flag to prevent multiple initialization of channel 1.
      */
      bool chnlStatus1;

      /**
      * Flag to prevent multiple initialization of channel 2.
      */
      bool chnlStatus2;

      /**
      * Flag to prevent multiple initialization of channel 3.
      */
      bool chnlStatus3;

      /**
      * Flag to prevent multiple calls of initCrossCompare
      */
      bool crossCompareInitialized;


      /** Singleton instance.
      *
      *  Declare constructor as private in order to prevent illegal use.
      * 
      */
      RadioHandler(void);

      /** Declare copy-constructor as private in order to prevent illegal use.
      * 
      */
      RadioHandler(const RadioHandler&);

      /** Declare assignment-operator as private in order to prevent illegal use.
      * 
      */
      RadioHandler& operator = (const RadioHandler&);

    };
  }
}

#endif

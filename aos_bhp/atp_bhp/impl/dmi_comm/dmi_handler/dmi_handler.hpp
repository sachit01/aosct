#ifndef DMIHandler_hpp
#define DMIHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The DMI Handler component deals with managing interaction between ATP and DMI
*  via DMI channels. It also provides other components a way to send required
*  information to the DMI and collect informations/data from the DMI. It creates,
*  manages and owns the DMI channels over which all the communications to DMI
*  takes place.
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /** The singleton instance of DMIHandler handles creation and scheduling of all DMI-channels.
    */  
    class DMIHandler : public AbstractDMIHandler
    {
    public:

      /**
      * Implements the virtual init function.
      *
      * Fill the containers for parsers and creators.
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
      static DMIHandler& instance(void);

      /**
      * Access-function for any train Loaded Status requested by driver
      *
      * @param[out] trainLoadedStatusRequested is the status requested by the driver
      *
      * @return true if request is available
      */
      bool getTrainLoadedStatusRequestedByDriver(TrainLoaded & trainLoadedStatusRequested);

      /**
      * Compatibility version for DMI
      *
      * @return the compatibility version for the DMI
      */
      virtual uint8_t getCompatibilityVersion() const;

    protected:

    private:

      /**
      * Compatibility version for DMI
      */
      static const uint8_t compatibilityVersion = 2U;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /** 
      *  Singleton instance.
      *
      *  Declare constructor as private in order to prevent illegal use.
      * 
      */
      DMIHandler(void);

      /**
      *  Declare copy-constructor as private in order to prevent illegal use.
      *
      */
      DMIHandler(const DMIHandler&);

      /**
      *  Declare assignment-operator as private in order to prevent illegal use.
      *
      */
      DMIHandler& operator = (const DMIHandler&);

    };
  }
}

#endif

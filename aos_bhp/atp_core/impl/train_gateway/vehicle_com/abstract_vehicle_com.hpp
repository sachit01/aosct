#ifndef AbstractVehicleCom_hpp
#define AbstractVehicleCom_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines AbstractVehicleCom class which contains the core functionality
* of the VehicleCom
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    class AbstractVehicleCom;
    /**
    * Static variable to store the single instance of AbstractVehicleCom
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractVehicleCom* coreVehicleComInstancePtr = static_cast<AbstractVehicleCom*>(NULL);

    /**
    * The class AbstractVehicleCom implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractVehicleCom : public ATC::IOComponent
    {
    public:

      /**
      * Get connection status towards vehicle
      *
      * @return true if connected to vehicle
      */
      virtual bool connectedVehicleComm() const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractVehicleCom* corePtr();

      /**
      * Start up and health supervision test for vehicle interface
      *
      * @return true if successful
      */
      virtual bool startupAndHealthSupTest() const;

      /**
      * Implements the init function.
      *
      * @return true when initialization completed
      */
      virtual bool init(void);

    protected:

      /**
      * Constructor
      */
      AbstractVehicleCom();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      bool abstractBaseInitialized;
    };
  }
}

#endif

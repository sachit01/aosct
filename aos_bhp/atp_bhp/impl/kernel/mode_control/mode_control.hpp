#ifndef ModeControl_hpp
#define ModeControl_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The declaration of the adaptation of the ModeControl.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-03    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Kernel
  {
    /**
    * The class ModeControl instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class ModeControl : public AbstractModeControl
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      *
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static ModeControl& instance(void);

      /**
      *Access - function for getting the Train Loaded confirm state
      *
      * @return  Train Loaded confirm needed
      */
      bool getConfirmTrainLoadedStateNeeded() const;

      /**
      *Access - function for getting the Last Car Brake Test Aborted Status
      *
      * @return  Last Casr Brake Test Aborted
      */
      bool getLastCarBrakePressureTestAborted() const;


      /**
      *Access - function to get the Manual confirmation status
      *
      * @return  Last car brake pressure test aborted Manual confirm status
      */
      bool isManualAbortConfirmationNeeded() const;

      /**
      * Access-function for setting and validating static BHPB configuration version
      *
      *  @param[in] configMajorVersion   Major version of static configuration
      *  @param[in] configMinorVersion   Minor version of static configuration
      */
      void setStaticConfigurationVersion(const uint8_t configMajorVersion, const uint8_t configMinorVersion);

      /**
      * Evaluate if handling done button is allowed on DMI
      * The adaptation also considers the free-rolling status when evaluating 
      * the driver is allowed to request handling done
      *
      * @return true if allowed to display the handling buttons on DMI
      */
      virtual bool isAllowedToDisplayHandlingDone() const;

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * To indicate SB Brake Test is requested to the Brake Component
      *
      * @return true if Brake Test is requested
      */
      bool getSbBrakeTestRequested() const;

    protected:

      /**
      * Create object of TrainConfig mode
      */
      virtual AbstractMode* createTrainConfigModeObj();

      /**
      * Create object of Location mode
      */
      virtual AbstractMode* createLocationModeObj();

      /**
      * Create object of Driver Login sequence
      */
      virtual DriverLoginSeq* createDriverLoginSeqObj();

      /**
      * Handle location exit scenarios
      */
      virtual void handleLocationData();

      /**
      * Function for logging Software, Configuration and Hardware version to TCC
      */
      virtual void logVersionToTCC(void);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      ModeControl();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      ModeControl(const ModeControl&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      ModeControl& operator = (const ModeControl&);

      /**
      * BHPB config version from TCC
      */
      uint16_t bhpbConfigVersion;

      /**
      * Event for logging Software name and version
      */
      const ATC::Event swNameAndVersion;

      /**
      * Event for logging common configuration name and version
      */
      const ATC::Event commonConfigNameAndVersion;

      /**
      * Event for logging type configuration name and version
      */
      const ATC::Event typeConfigNameAndVersion;

      /**
      * Event for logging instance configuration name and version
      */
      const ATC::Event instanceConfigNameAndVersion;

      /**
      * Event for logging maintenance configuration name and version
      */
      const ATC::Event maintConfigNameAndVersion;

      /**
      * Event for logging runtime configuration name and version
      */
      const ATC::Event runtimeConfigNameAndVersion;

      /**
      * Event for logging dispatcher configuration name and version
      */
      const ATC::Event dispatcherConfigNameAndVersion;

      /**
      * Event for logging hardware Digital Input revision name and version
      */
      const ATC::Event hwDigitalInputNameAndVersion;

      /**
      * Event for logging hardware Digital Output revision name and version
      */
      const ATC::Event hwDigitalOutputNameAndVersion;

      /**
      * Event for logging hardware Analog Input revision name and version
      */
      const ATC::Event hwAnalogInputNameAndVersion;

      /**
      * Event for logging vital Frame work name and version
      */
      const ATC::Event vitalframeworkNameAndVersion;

      /**
      * Event for logging SDP name and version
      */
      const ATC::Event sdpNameAndVersion;

      /**
      * Event for logging dispatcher name and version
      */
      const ATC::Event dispatcherNameAndVersion;

      /**
      * Event for logging vioh client name and version
      */
      const ATC::Event viohClientNameAndVersion;

      /**
      * Event for logging vioh server name and version
      */
      const ATC::Event viohServerNameAndVersion;

      /**
      * Event for logging OPC version
      */
      const ATC::Event opcVersionEvent;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Event for logging version of the ADS map received from TCC does not match the BHPB config version from LCS.
      */
      const ATC::Event adsVersionMismatch;


    };
  }
}
#endif

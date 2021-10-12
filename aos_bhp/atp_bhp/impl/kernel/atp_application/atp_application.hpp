#ifndef ATPApplication_hpp
#define ATPApplication_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc defined.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-22    lantback    Created
* 2016-06-10    adgupta     Added get and set functions to acquire VIOH Client Handler
* 2016-09-21    arastogi    added function to add all components
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_atp_application.hpp"
#include "event.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Kernel
  {
    /**
    * The class ATP application instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    */
    class ATPApplication : public AbstractATPApplication
    {
    public:

      /**
      * Implements the virtual pre-init function.
      *
      * This function should call preInit functions for all components.
      * Should be called after add components.
      */
      virtual void preInit(void);

      /**
      * Implements the init function.
      *
      * This function should initialise all the components in the component list.
      * Should be called when pre-init is done.
      * @return  True when initialization is done
      */
      virtual bool init(void);

      /**
      * Implements the run function.
      */
      virtual void run(void);

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return The one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static ATPApplication& instance(void);

      /**
      * Set the vital I/O Handler
      *
      * @param[in] Client vital I/O handler
      */
      void setVIOHClientHandle(VIOHnames::VIOHClient* const handle);

      /**
      * Get the vital I/O Handle
      *
      * @return Client vital I/O handler
      */
      virtual VIOHnames::VIOHClient* getVIOHClientHandle();

      /**
      * Create objects for all components and add it to component list
      */
      void addAllComponents(void);

      /**
      * Access-function for Last Execution Time 
      *
      *  @return Last Execution Time
      */
      int32_t getLastExecTime() const;

      /**
      * Access-function for Min Execution Time since restart or reset
      *
      *  @return Minimum Execution Time
      */
      int32_t getMinExecTime() const;

      /**
      * Access-function for Max Execution Time since restart or reset
      *
      *  @return Maximum Execution Time
      */
      int32_t getMaxExecTime() const;

      /**
      * Clears the Last/Min/Max Execution Time values
      *
      */
      void clearExecTimes(void);

      /**
      * Get the Application name
      *
      * @return application name
      */
      virtual const char_t* getApplicationName() const;

      /**
      * Get the Application Version String
      *
      * @return application version string
      */
      virtual const char_t* getApplicationVersionString() const;

      /**
      * Set the Dispatcher Version String (called by AtpMain)
      * 
      */
      const void setDispatcherVersionString(const char_t* const versionString);

      /**
      * Get the Application Version String
      *
      * @return dispatcher version string
      */
      const char_t* getDispatcherVersionString() const;

      /**
      * Implements the specific adaptation function of the version validation of the vioh versions
      * @return true if the version is validated
      */
      virtual bool validateDispatcherVersion() const;

    protected:

      /**
      * Maximum length of the dispatcher version.
      */
      static const uint8_t dispatcherVersionLength = 20U;

      /**
      * Dispatcher Version String
      */
      char_t dispatcherVersionString[dispatcherVersionLength];

    private:

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
      /**
      * Method to convert the created event-list to XML and XLSX Format.
      *
      */
      void convertEventListToXmlAndXlsx() const;

#endif

      /**
      * The initialization state to initialize Config.
      */
      static const uint8_t initConfig = 1U;

      /**
      * The initialization state to initialize the simulators.
      */
      static const uint8_t initStage1 = 2U;

      /**
      * The initialization state to initialize the components and run simulators.
      */
      static const uint8_t initStage2 = 3U;

      /**
      * The final state of initialization.
      */
      static const uint8_t initDone = 4U;

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      ATPApplication();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      ATPApplication(const ATPApplication&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      ATPApplication& operator = (const ATPApplication&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      void initCrossCompare() const;

      /**
      * The current initialization state of ATPApplication.
      */
      uint8_t initState;

      /**
      * Store the Handle to the Vital I/O Handler. Will be initialised by ATP Main.
      * This will be used by components who need access to VIO client.
      */
      VIOHnames::VIOHClient* viohClientHandle;

      /**
      * Execution time for last execution cycle in ms.
      */
      int32_t lastCycleExecutionTime;

      /**
      * Minimum execution time for one cycle since restart or reset of value.
      */
      int32_t minExecutionTime;
      
      /**
      * Maximum execution time for one cycle since restart or reset of value.
      */
      int32_t maxExecutionTime;
    };
  }
}
#endif

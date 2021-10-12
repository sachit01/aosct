#ifndef LocoIO_hpp
#define LocoIO_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The LOCO IO component deals with the interface between the vital IO and the AOS SW.
*  This is the declaration of the adaptation for the component
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-22    arastogi    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-10    adgupta     Implementation of LocoIO functionality
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_loco_io.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace IO
  {
    /**
    * The class LocoIO instantiates the abstract class and implements 
    * the interfaces needed for both inherited classes and component.
    *
    */
    class LocoIO : public AbstractLocoIO
    {
    public:

      /**
      * The Adaptation Output enums used by LocoIO.
      *
      */
      enum AdapDigitalOutputs
      {
        TCO,             //!<Traction Cut-off
        NumOfAdapDigitalOutputs
      };

      /**
      * The Adaptation Input enums used by LocoIO (shall be in the right order, lowest signal number first)
      *
      */
      enum AdapDigitalInputs
      {
        RoadM,                    //!<Road Mode - Hi Rail Wheels up
        RailM,                    //!<Rail mode - Hi Rail Wheels down
        TCOFB,                    //!<Traction Cut-off Feedback
        NONLEAD,                  //!<Non-leading locomotive
        EBCutOut1A,               //!<Emergency Break Cut-out #1a
        EBCutOut1B,               //!<Emergency Break Cut-out #1b
        EBCutOut2A,               //!<Emergency Break Cut-out #2a
        EBCutOut2B,               //!<Emergency Break Cut-out #2b
        NumOfAdapDigitalInputs
      };

      /**
      * The Adaptation Analog Input enum used by LocoIO
      * The board provides 8 inputs and two outputs.
      * AOS only uses analog inputs and the outputs are not described/defined
      */
      enum AdapAnalogInputs
      {
        BrakePressure1,         //!<Brake Pressure from Sensor 1
        BrakePressure2,         //!<Brake Pressure from Sensor 2
        NumOfAdapAnalogInputs
      };

      /**
      * Implements the virtual init function.
      */
      virtual bool init();

      /**
      * Implements the runIn function for the adaptation.
      * The runIn in the adaptation will fetch the updated values of inputs 
      * defined in adaptation from respective components
      */
      virtual void runIn(void);

      /**
      * Implements the runOut function for the adaptation.
      * The runOut in the adaptation will fetch the updated values of outputs 
      * defined in adaptation from respective components and update the output struct
      */
      virtual void runOut(void);

      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static LocoIO& instance(void);

      /**
      * Get the current value of an input from AdapDigitalInputs enum.
      *
      * @param[in] in   The input whose value is required
      * @param[out] value   The current value of selected input
      *    
      * @return the value of the input specified
      */
      bool getAdapDigitalInputValue(AdapDigitalInputs in, bool* const value) const;

      /**
      * Get the current value of an output from AdapDigitalOutputs enum.
      *
      * @param[in] out  The output whose value is required
      * @param[out] value   The current value of selected output
      *
      * @return the current value of the output specified
      */
      bool getAdapDigitalOutputValue(AdapDigitalOutputs out, bool* const value) const;

      /**
      * Set the new value of an output using output number.
      *
      * @param[in] out    The output whose value is to be set.
      * @param[in] value  The value of the output specified.
      *
      * @return true when output to be set exists/is valid
      */
      bool setAdapDigitalOutputValue(const AdapDigitalOutputs out, const bool value);

      /**
      * Get the current value of an input from AdapAnalogInputs enum.
      *
      * @param[in] in  The input whose value is required
      * @param[out] value  The Scaled value of the corresponding input
      *
      * @return true if the call is successful.
      */
      virtual bool getAdapAnalogInputValue(AdapAnalogInputs in, uint16_t &value) const;

      /**
      * Interface to call  Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /**
      * Get the value of the Sleeping Signal
      *
      * @return the value of the sleeping-signal (true = activated, false = deactivated)
      */
      virtual bool getSleepingSignal(void) const;


    protected:

      /**
      * Initializes the cross compare module. Called by the init.
      */
      virtual void initCrossCompare() const;

      /**
      * Indicates whether service brake should be applied.
      *
      * This is a virtual function to allow the adaptation to override the core behaviour.
      */
      virtual bool getSbApplied() const;

    private:

      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      LocoIO();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      LocoIO(const LocoIO&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      LocoIO& operator = (const LocoIO&);

      /**
      * Implements the specific adaptation function of the version validation of the vioh versions
      * @return true if the version is validated
      */
      virtual bool validateViohVersion() const;

      /**
      * Safety Halt event to report when HiRail Configures Invalid State
      */
      const ATC::Event errorHiRailConfigInvalidState;

      /**
      * Safety Halt event to report when HiRail Configures 'Road' or 'Transition' State
      */
      const ATC::Event errorHiRailConfigRoadTransitionState;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Handling HiRail Configuration to report SafetyHalt if it has Invalid Configuration.
      */
      void handleRailRoadSignal(void)const;
      
      /**
      * Diagnostic value for Analyzer, BP1
      */
      uint16_t scaledBP1Measurement;

      /**
      * Diagnostic value for Analyzer, BP2
      */
      uint16_t scaledBP2Measurement;

      /**
      * Diagnostic value for Analyzer, TCO Feedback
      */
      uint8_t tcoFBMeassurement;

    };
  }
}
#endif

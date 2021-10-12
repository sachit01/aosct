#ifndef VIOHSim_hpp
#define VIOHSim_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This defines the class for Vital Input Output Handler Simulator(VIOH Sim)
* This class simulates the function of the VIOH Client library.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-28    adgupta     Created
* 2016-05-24    adgupta     Implementation of VIOH simulator features.
* 2016-06-13    adgupta     Added VOU functions for Loco IO inputs.
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-10-06    adgupta     Updated after integration with AOSPC
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atc_types.hpp"
#include <vfw_sync.h>
#include "vio_types.h"
#include "vioh_client.hpp"
#include "sim_types.hpp"
#include "event.hpp"
#include "channel_config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  namespace Sim
  {
    /**
    * Number of Input and output channels
    */
    static const uint8_t numberAnalogInputs = 8U;

    /**
    * Simulated Inputs from AOS-PC to ATP
    */
    struct VIOHSimInputs
    {
      bool dataValid;
      uint32_t bInputValues;      //!< Bit masked simulated Input values. Bit 0-23 - Input 1 to 24, Bit 24-31 - Spare
      uint32_t bVIUHealthState;   //!< Bit masked simulated health state of digital input board.
      uint16_t bAnalogInputs[numberAnalogInputs]; //!< simulated Analog inputs of analog input board.
    };

    /**
    * Simulated Inputs from ATP to AOS-PC
    */
    struct VIOHSimOutputs
    {
      bool dataUpdated;         //!< The values in mirror outputs was updated
      bool dataValid;           //!< Output data was set by Loco IO atleast once
      uint16_t bOutputValuesA;  //!< Bit masked simulated Output Values A. Bit 0-11 - Output 1 to 12, Bit 12-16 - Spare
      uint16_t bOutputValuesB;  //!< Bit masked simulated Output Values B. Bit 0-11 - Output 1 to 12, Bit 12-16 - Spare
    };

    /**
    * The mirror block that defines the locally stored Input and Output Simulated values.
    */
    struct VIOHMirror
    {
      VIOHSimInputs  viohSimInputs;
      VIOHSimOutputs viohSimOutputs;
    };

    /**
    * Total length of the simulated Input message, including header, footer, etc.
    */
    static const uint16_t lenSimulatedInputTotal = (simInputDataLen + headerFooterLen);

    /**
    * Number of Input and output channels
    */
    static const uint16_t numberInputChannels = 24U;
    static const uint16_t numberOutputChannels = 12U;

    class VIOHSim;

    /**
    * Static variable to store the single instance of VIOHSim
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static VIOHSim* coreVIOHSimPtr = static_cast<VIOHSim*>(NULL);

    /**
    * The class VIOHSim implements the simulation for the VIOH library.
    */
    class VIOHSim : public IOComponent
    {
    private:

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Flag to check if Connected to Loco/AOSPC
      **/
      bool isConnected;

      /** VIOH Sim mirror to hold values locally in the Sim.
      */
      VIOHMirror mirror;

      /** Channel handle returned by vfwChannelOpenWrite()
      */
      VFW_ChannelDesc channelWriteDesc;

      /** Channel handle returned by vfwSyncAddChannel()
      */
      VFW_SyncChannel syncChannelReadDesc;

      /** Vital framework Buffer usage.
      */
      VFW_Buffer vfwBuffer;

      /** Buffer to read/write values from/to vfw channels
      */
      uint8_t buffer[ATC::maxVIOHMessageSize];

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      VIOHSim();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      VIOHSim(const VIOHSim&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      VIOHSim& operator = (const VIOHSim&);

      /**
      * Read functions to read from VFW read channels to get simulated values
      * from AOS PC which will be used to update Sim mirror.
      */
      bool readSimulatedInputs(void);

      /**
      * Write functions to update AOS PC via VFW channels.
      */
      void writeSimulatedOutputs(void);

    protected:

    public:

      /**
      * Returns the locally stored input state and health state of the specified digital input ID.
      * The using component will call the corresponding function in the adapted inherited class where connectionId is defined as an enum.
      *
      *  @param[in]  Id           An id identifying the Vital input channel ID. Range 1-24.
      *  @param[out] pbState      State of this channel identified by Id.
      *  @param[out] pHealthState Health status of the input in scope.
      *
      *  @return success/error codes enum values. enCRT_OK, enCRT_R_ERR, etc.
      */
      VIOHnames::VIOH_clientResultType VIUGetState(const uint32_t Id,
        bool_t *const pbState,
        VIOHnames::VIOH_healthStateType *const pHealthState);

      /**
      * Method that returns the input value and the health state of the specified analogue input.
      * If the analogue input is used by a vital application the input state is synchronized between A and B channel,
      * otherwise the local value is provided.
      *
      *  @param[in] Id           Specified analogue input. Range 1-8.
      *  @param[in] pValue       Pointer to allocated memory of uint16_t.
      *  @param[in] pHealthState Pointer to allocated memory of VIOH_healthStateType.
      *  @param[out] pValue       Value read. Range: 0-4095 (The 12 least significant bits of the value correspond to the 12 bits provided by the AIOU.)
      *                          The VIOH does not translate the value received from the AIOU into units e.g. acceleration.
      *  @param[out] pHealthState Health state for the specified analogue input.
      *
      *  @return success/error codes enum values. enCRT_OK, enCRT_R_ERR, etc.
      */
      VIOHnames::VIOH_clientResultType AIOUGetState(uint8_t Id, uint16_t *pValue, VIOHnames::VIOH_healthStateType *pHealthState);

      /**
      * Returns the locally stored output state and health state of the specified digital output ID.
      * The using component will call the corresponding function in the adapted inherited class where connectionId is defined as an enum.
      *
      *  @param[in]  Id           An id identifying the Vital output channel ID. Range 1-12.
      *  @param[out] pbIsActive   State of this channel identified by Id.
      *  @param[out] pHealthState Health status of the input in scope.
      *
      *  @return success/error codes enum values. enCRT_OK, enCRT_R_ERR, etc.
      */
      VIOHnames::VIOH_clientResultType VOUGetState(const uint32_t Id,
        bool_t *const pbIsActive,
        VIOHnames::VIOH_healthStateType *const pHealthState);

      /**
      * To control a single digital output.
      *
      *  @param[in]  Id           ID of the digital output.. Range 1-12.
      *  @param[in]  bIsActive    State of the digital output identified by Id. "true" = activated, "false" = deactivated.
      *  @param[in]  bIsSync      Set the synchronization of the digital output.
      *                           "true" = synchronization between A- and B-channel activated.
      *                           "false" = synchronization between A- and B-channel deactivated.
      *
      *  @return success/error codes enum values. enCRT_OK, enCRT_R_ERR, etc.
      */
      VIOHnames::VIOH_clientResultType VOUSetOutput(const uint32_t Id,
        const bool_t bIsActive,
        const bool_t bIsSync);

      /**
      * @brief   Method to specify which digital inputs the application uses.
      *
      *          Use VIURegisterResult() to check the result of the configuration
      *          request, Update() to update the client with the latest VIU information
      *          and VIUGetState() to check the updated input states.
      *
      *          The inputs list contains the identifiers of all inputs to be used by the
      *          application. A vital application uses all digital inputs in synchronized
      *          mode, a non-vital application uses all digital inputs in unsynchronized mode.
      *
      *          Digital inputs can be shared between applications, e.g. application 1 can use input 1 and 2
      *          and a application 2 can use input 3.
      *
      *          If a digital input is configured to a vital application the digital input state
      *          is synchronized between A and B channel.
      *
      * @post    A registration request will trigger the server to initially update the client with unit
      *          specific information, like device related health state information, unit revision
      *          and hardware configuration.
      *
      * @param[in] pInputList
      *          Pointer to VIOH_listType list of inputs.\n
      *          Range: 1-24 inputs
      *
      * @return  VIOH_clientResultType result/error code.\n
      *          Possible error codes for this method:
      *          @li enCRT_OK:       function succeeded
      *          @li enCRT_IPARA:    provided parameter list is a NULL pointer
      *          @li enCRT_OUT_OF_RANGE: tried to register more than 24 input channels
      *          @li enCRT_PENDING:  registration aborted, there is previous request pending
      *          @li enCRT_NO_INI:   client initialization not successful
      *
      * @snippet main.cpp register digital inputs1
      * @snippet main.cpp register digital inputs2
      */
      VIOHnames::VIOH_clientResultType VIURegister(const VIOHnames::VIOH_listType * const pInputList);

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      *
      * @return the one and only one instance.
      */
      static VIOHSim& instance(void);

      /**
      * Implements the virtual preInit function.
      *
      * Register vfw channels in sync handler.
      */
      virtual void preInit(void);

      /**
      * Implements the  init function. Initializes prerequisites for the Simulator.
      *
      * @return Returns true when initialization completed successfully
      */
      virtual bool init(void);

      /**
      * Implements the runIn function.
      */
      virtual void runIn(void);

      /**
      * Implements the runOut function.
      */
      virtual void runOut(void);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static VIOHSim* corePtr(void);

    };
  }
}

#endif

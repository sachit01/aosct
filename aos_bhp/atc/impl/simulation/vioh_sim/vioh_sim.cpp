/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This simulates/fakes the class for VIOH Client library.
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
* 2016-06-09    arastogi    Updated to fix the interface change in event handler
* 2016-06-13    adgupta     Updated to compliment Loco IO changes.
* 2016-07-05    spandita    Modified the channel names of VIOH
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-10-06    adgupta     Updated VIOH Sim afetr integration with AOS PC
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "vioh_sim.hpp"
#include "atc_util.hpp"
#include <vfw_identity.h>
#include <vfw_string.h>
#include "sim_types.hpp"
#include "abstract_event_handler.hpp"
#include <channel_config.hpp>
#include "atc_util.hpp"


/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  namespace Sim
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    VIOHSim::VIOHSim() : IOComponent(atcVIOHSimId, "VIOHSim", "VS"), initDone(false)
    {
      if (coreVIOHSimPtr != 0)
      {
        // Error handler
        aosHalt(__FILE__, __LINE__, "VIOHSim Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreVIOHSimPtr = this;

      isConnected = false;
    }


    /******************************************************************************
    * preInit
    ******************************************************************************/
    void VIOHSim::preInit()
    {
      /** Name of channel to read messages from
      */
      char_t viohReadChannelName[VFW_CH_NAME_MAX_Z];

      /** Name of channel to write messages to
      */
      char_t viohWriteChannelName[VFW_CH_NAME_MAX_Z];

      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_ChannelDesc channelReadDesc = static_cast<VFW_ChannelDesc>(NULL);

      // Initialize Channel Names
      if (vfwGetSide() == VFW_A_SIDE)
      {
        static_cast<void>(vfw_strlcpy(viohReadChannelName, viohChannelDispToATPA, sizeof(viohReadChannelName)));
        static_cast<void>(vfw_strlcpy(viohWriteChannelName, viohChannelATPAToDisp, sizeof(viohWriteChannelName)));
      }
      else if (vfwGetSide() == VFW_B_SIDE)
      {
        static_cast<void>(vfw_strlcpy(viohReadChannelName, viohChannelDispToATPB, sizeof(viohReadChannelName)));
        static_cast<void>(vfw_strlcpy(viohWriteChannelName, viohChannelATPBToDisp, sizeof(viohWriteChannelName)));
      }
      else
      {
        // To please lint:
        viohReadChannelName[0] = '\0';
        viohWriteChannelName[0] = '\0';

        aosHalt(__FILE__, __LINE__, "Invalid Side");
      }

      // Open a channel to be used when reading from AOSPC
      channelReadDesc = vfwChannelOpenRead(viohReadChannelName, viohMessageInQueueSize, maxVIOHMessageSize, viohReadChannelName);
      vfwChannelSetOverwritable(channelReadDesc);

      // Open a channel to be used when writing to AOSPC
      channelWriteDesc = vfwChannelOpenWrite(viohWriteChannelName, viohMessageOutQueueSize, maxVIOHMessageSize);
      vfwChannelSetOverwritable(channelWriteDesc);

      if ((NULL != channelReadDesc) && (NULL != channelWriteDesc))
      {
        //Sync with Diversified Channel(A/B)
        syncChannelReadDesc = vfwSyncAddChannel(channelReadDesc, trueVfw);

        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(syncChannelReadDesc);

      }
      else
      {
        aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }

    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool VIOHSim::init(void)
    {
      if (!initDone)
      {
        bool returnValue = true;
        // Reset mirror values.
        memset(&mirror, 0, sizeof(VIOHMirror));

        // initialize the vfw buffer with the raw buffer. We use this buffer for manipulating data in the buffer.
        vfwInitBuffer(&vfwBuffer, (uint8_t*)&buffer, maxVIOHMessageSize);

        initDone = returnValue;
      }

      return initDone;
    }

    /******************************************************************************
    * readSimulatedInputs
    ******************************************************************************/
    bool VIOHSim::readSimulatedInputs()
    {
      bool returnValue = true;
      int32_t numReadBytes = 0;
      uint8_t recvSTX = 0U;
      uint8_t recvSimViohInputProtocolVer = 0U;
      uint16_t recvSimInputDataLen = 0U;
      uint8_t recvSimInputNidMsgType = 0U;

      // Any new message received?
      while (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        vfwInitBuffer(&vfwBuffer, buffer, maxVIOHMessageSize);
        // Set the buffer to Read mode.
        vfwSetReadBuffer(&vfwBuffer, maxVIOHMessageSize);

        // read the new message received.
        numReadBytes = vfwSyncChannelReadBuffer(syncChannelReadDesc, &vfwBuffer);

        //Read the first 4 message data received for error check.
        //vfwGet() and vfwPut() functions take care of byte ordering,
        //pointer is shifting to next readable byte is also maintained.
        recvSTX = vfwGetU8(&vfwBuffer);
        recvSimViohInputProtocolVer = vfwGetU8(&vfwBuffer);
        recvSimInputDataLen = vfwGetU16(&vfwBuffer);
        recvSimInputNidMsgType = vfwGetU8(&vfwBuffer);

        //Check if message is read correctly
        if ((lenSimulatedInputTotal == numReadBytes) && (STX == recvSTX) && (simViohInputProtocolVer == recvSimViohInputProtocolVer)
          && (simInputDataLen == recvSimInputDataLen) && (simInputNidMsgType == recvSimInputNidMsgType))
        {
          // All good. Update the mirror now. 
          mirror.viohSimInputs.bInputValues = vfwGetU32(&vfwBuffer);
          mirror.viohSimInputs.bVIUHealthState = vfwGetU32(&vfwBuffer);
          mirror.viohSimInputs.dataValid = true;

          for (uint8_t index = 0U; index < numberAnalogInputs; index++)
          {
            mirror.viohSimInputs.bAnalogInputs[index] = vfwGetU16(&vfwBuffer);
          }
        }//end of if()
        else
        {
          //Data read incorrectly. Data is Invalid now!
          mirror.viohSimInputs.dataValid = false;
          returnValue = false;
        }

        if (true == returnValue)
        {
          // No error yet! Chk last byte to be ETX as well for correct data received.
          if (ETX != vfwGetU8(&vfwBuffer))
          {
            mirror.viohSimInputs.dataValid = false;
            returnValue = false;
          }
        }

        // No need to delete the messages as the messages are automatically deleted after being read from the channel.
        if (!isConnected)
        {
          //Received something from AOS PC/Loco. Connected now!
          //P.S.- This will always remain true once set as UDP is connection less and we don't know when it was disconnected.
          isConnected = true;
        }
      }//end of while() - Read again till the channel is empty.

      return returnValue;
    }

    /******************************************************************************
    * writeSimulatedOutputs
    ******************************************************************************/
    void VIOHSim::writeSimulatedOutputs()
    {
      if (isConnected)
      {
        //Write to buffer only if mirror values are updated/valid.
        //dataValid field gets valid when any of the Simoutput values is updated.      
        if (mirror.viohSimOutputs.dataUpdated)
        {
          //Initialize and Clear buffer before writing to it. As we reuse the same buffer.
          vfwInitBuffer(&vfwBuffer, buffer, maxVIOHMessageSize);

          vfwPutU8(&vfwBuffer, STX);
          vfwPutU8(&vfwBuffer, simViohOutputProtocolVer);
          vfwPutU16(&vfwBuffer, simOutputDataLen);
          vfwPutU8(&vfwBuffer, simOutputNidMsgType);

          //Populate the vfw buffer to be written to.
          vfwPutU16(&vfwBuffer, mirror.viohSimOutputs.bOutputValuesA);
          vfwPutU16(&vfwBuffer, mirror.viohSimOutputs.bOutputValuesB);

          vfwPutU8(&vfwBuffer, ETX);

          vfwChannelWriteBuffer(channelWriteDesc, &vfwBuffer);

          // Make the data updated false as this is now old data. Already sent to AOSPC.
          mirror.viohSimOutputs.dataUpdated = false;
        }
      }
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void VIOHSim::runIn(void)
    {
      // Get the values from AOS PC and Update the mirror.
      if (!readSimulatedInputs())
      {
        writeToLog(ATC::BriefLog,"Reading from AOS PC Failed!", __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void VIOHSim::runOut(void)
    {

      // Get the values from mirror and Update the AOS PC.
      writeSimulatedOutputs();
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    VIOHSim& VIOHSim::instance(void)
    {
      static VIOHSim viohInstance;

      return viohInstance;
    }

    /******************************************************************************
    * VIURegister
    ******************************************************************************/
    VIOHnames::VIOH_clientResultType VIOHSim::VIURegister(const VIOHnames::VIOH_listType * const /*pInputList*/)
    {
#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
      ATC::debugInfo("Warning: VIOHSim::VIURegister() may have side effects in this mode\n");

      return VIOHnames::enCRT_OK;
#else
      if (isConnected)
      {
        return VIOHnames::enCRT_OK;
      }
      else
      {
        // Wait until connected to AOSPC to complete the Initialization of Loco IO.
        return VIOHnames::enCFG_PEND;
      }
#endif
    }

    /******************************************************************************
    * VIUGetState
    ******************************************************************************/
    VIOHnames::VIOH_clientResultType VIOHSim::VIUGetState(const uint32_t Id,
      bool_t *const pbState,
      VIOHnames::VIOH_healthStateType *const pHealthState)
    {
      VIOHnames::VIOH_clientResultType returnValue = VIOHnames::enCRT_OK;

      // Check if Id is valid. 1-24 Input Channels
      if ((Id > 0U) && (Id <= numberInputChannels))
      {
        const uint32_t mask = 0x01U << (Id - 1U);    //Select the bit to be masked for pbState, create appropriate mask
        *pbState = (((mirror.viohSimInputs.bInputValues) & mask) != 0U) ? ATC::trueVfw : ATC::falseVfw;
        *pHealthState = VIOHnames::VIORes_OK;  //Health is always set to Ok unless needed to update.

        returnValue = VIOHnames::enCRT_OK;
      }
      else
      {
        // Parameter out of range. Invalid!
        returnValue = VIOHnames::enCRT_IPARA;
      }

      return returnValue;
    }

    /******************************************************************************
    * VOUGetState
    ******************************************************************************/
    VIOHnames::VIOH_clientResultType VIOHSim::VOUGetState(const uint32_t Id,
      bool_t * const pbIsActive,
      VIOHnames::VIOH_healthStateType* const pHealthState)
    {
      VIOHnames::VIOH_clientResultType returnValue = VIOHnames::enCRT_OK;

      // Check if Id is valid. 1-12 output Channels
      if ((Id > 0U) && (Id <= numberOutputChannels))
      {
        const uint32_t mask = 0x01U << (Id - 1U);    //Select the bit to be masked for pbState, create appropriate mask

        if (VFW_A_SIDE == vfwGetSide())
        {
          *pbIsActive = ((mirror.viohSimOutputs.bOutputValuesA & mask) != 0U) ? ATC::trueVfw : ATC::falseVfw;
        }
        else
        {
          *pbIsActive = ((mirror.viohSimOutputs.bOutputValuesB & mask) != 0U) ? ATC::trueVfw : ATC::falseVfw;
        }
        *pHealthState = VIOHnames::VIORes_OK;  //Health is always set to Ok unless needed to update.

        returnValue = VIOHnames::enCRT_OK;
      }
      else
      {
        // Parameter out of range. Invalid!
        returnValue = VIOHnames::enCRT_IPARA;
      }

      return returnValue;
    }

    /******************************************************************************
    * VOUSetOutput
    ******************************************************************************/
    VIOHnames::VIOH_clientResultType  VIOHSim::VOUSetOutput(const uint32_t Id,
      const bool_t bIsActive,
      const bool_t bIsSync)
    {
      VIOHnames::VIOH_clientResultType returnValue = VIOHnames::enCRT_OK;

      // Check if Id is valid. 1-12 Output Channels
      if ((Id > 0U) && (Id <= numberOutputChannels))
      {
        uint16_t mask = 0x01U << (Id - 1U);  // Create appropriate mask
        if ((bIsActive == ATC::trueVfw) && (bIsSync == ATC::trueVfw))  //Both bIsActive and bIsSync is true
        {
          mirror.viohSimOutputs.bOutputValuesA |= mask;   // Set both the sides.
          mirror.viohSimOutputs.bOutputValuesB |= mask;
        }
        else if (bIsActive == ATC::trueVfw)  //Only bIsActive is true
        {
          //Set only the active side of vfw.
          if (VFW_A_SIDE == vfwGetSide())
          {
            mirror.viohSimOutputs.bOutputValuesA |= mask;
            mask = ~mask;
            mirror.viohSimOutputs.bOutputValuesB &= mask;
          }
          else
          {
            mirror.viohSimOutputs.bOutputValuesB |= mask;
            mask = ~mask;
            mirror.viohSimOutputs.bOutputValuesA &= mask;
          }
        }
        else  //Both bIsActive and bIsSync is false. Reset both the sides.
        {
          mask = ~mask;
          mirror.viohSimOutputs.bOutputValuesA &= mask;
          mirror.viohSimOutputs.bOutputValuesB &= mask;
        }

        //Set the valid bit to true, new data received.
        mirror.viohSimOutputs.dataUpdated = true;
        mirror.viohSimOutputs.dataValid = true;
      }
      else
      {
        returnValue = VIOHnames::enCRT_IPARA;
      }

      return returnValue;
    }

    /******************************************************************************
    * AIOUGetState
    ******************************************************************************/
    VIOHnames::VIOH_clientResultType VIOHSim::AIOUGetState(uint8_t Id, uint16_t *pValue, VIOHnames::VIOH_healthStateType *pHealthState)
    {
      VIOHnames::VIOH_clientResultType returnValue = VIOHnames::enCRT_OK;
      
      // Check if Id is valid. 1-8 Analog Inputs
      if ((Id > 0U) && (Id <= numberAnalogInputs))
      {
        *pValue = mirror.viohSimInputs.bAnalogInputs[Id - 1U]; //Value of the analog Inputs 
        *pHealthState = VIOHnames::VIORes_OK;  //Health is always set to OK unless needed to update.

        returnValue = VIOHnames::enCRT_OK;
      }
      else if(Id == 0U)                                      //Analog Signal not yet defined
      {
        *pValue = 0U;                                       //Value of the analog Inputs 
        *pHealthState = VIOHnames::VIORes_NO_REGISTRATION;  //No registration has been done for current input.

        returnValue = VIOHnames::VIORes_NO_REGISTRATION;
      }
      else
      {
        // Parameter out of range. Invalid!
        returnValue = VIOHnames::enCRT_IPARA;
      }

      return returnValue;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    VIOHSim* VIOHSim::corePtr(void)
    {
      return coreVIOHSimPtr;
    }
  }
}

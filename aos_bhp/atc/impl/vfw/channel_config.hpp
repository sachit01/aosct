#ifndef ChannelConfig_hpp
#define ChannelConfig_hpp
/*******************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
 *
 * We reserve all rights in this file and in the information
 * contained therein. Reproduction, use or disclosure to third
 * parties without written authority is strictly forbidden.
 *
 *
 * DESCRIPTION:
 *
 * The Channel Config contains the configuration value of VFW Read/Write for
 * all the components making use of it.
 *
 *****************************************************************************/

 /*******************************************************************************
 *
 * REVISION HISTORY :
 *
 * Date          Name        Changes
 * ---------------------------------------------------------------------------
 * 2016-06-14    akushwah    Created
 * 2016-06-17    akushwah    Move Radio Channel Id variables to Radio Channel Component
 * 2016-06-21    akushwah    Added Channel name for Odometry component
 * 2016-07-04    spandita    Updated the review comment with addition of port number and ip address.
 * 2016-07-04    spandita    Updated with SIL macro .
 * 2016-07-05    spandita    Updated name of COD Sim channel(review comments).
 * 2016-09-08    adgupta     Added support for DMI communication
 * 2016-08-29    akushwah    Added Channel name for DMI Component
 * 2016-09-15    spandita    Added port Number and Channel names of OPC Sim
 * 2016-09-28    adgupta     Added support for BTM Handler.
 * 2016-12-02    spandita    Renamed the channel names for dispatcher to ATP and vice versa
 *******************************************************************************/

 /******************************************************************************
 * INCLUDE FILES
 ******************************************************************************/
#include <vfw_types.h>
#include "atc_types.hpp"
 /******************************************************************************
 * DECLARATIONS
 ******************************************************************************/

namespace ATC
{
  /**
  * The maximum allowed transmission time for VFW channels (ms)
  */
  static const uint32_t maxChannelTransmissionTime = 300U;

  /**
   * \brief Radio Channel names from dispatcher to ATP A & B
   */
  static const char_t * radioChannel1DispToATPA = "Disp_To_RadioChannel1_A";
  static const char_t * radioChannel1DispToATPB = "Disp_To_RadioChannel1_B";
  static const char_t * radioChannel2DispToATPA = "Disp_To_RadioChannel2_A";
  static const char_t * radioChannel2DispToATPB = "Disp_To_RadioChannel2_B";
  static const char_t * radioChannel3DispToATPA = "Disp_To_RadioChannel3_A";
  static const char_t * radioChannel3DispToATPB = "Disp_To_RadioChannel3_B";

  /**
  * \brief Radio Channel names from ATP A & B to Dispatcher
  */
  static const char_t * radioChannel1ATPAToDisp = "RadioChannel1_To_Disp_A";
  static const char_t * radioChannel1ATPBToDisp = "RadioChannel1_To_Disp_B";
  static const char_t * radioChannel2ATPAToDisp = "RadioChannel2_To_Disp_A";
  static const char_t * radioChannel2ATPBToDisp = "RadioChannel2_To_Disp_B";
  static const char_t * radioChannel3ATPAToDisp = "RadioChannel3_To_Disp_A";
  static const char_t * radioChannel3ATPBToDisp = "RadioChannel3_To_Disp_B";

  /**
  * \brief DMI channel names from dispatcher to ATP A & B
  */
  static const char_t * dmiChannel1DispToATPA = "Disp_To_DMIChannel1_A";
  static const char_t * dmiChannel1DispToATPB = "Disp_To_DMIChannel1_B";
  static const char_t * dmiChannel2DispToATPA = "Disp_To_DMIChannel2_A";
  static const char_t * dmiChannel2DispToATPB = "Disp_To_DMIChannel2_B";

  /**
  * \brief DMI Channel names from ATP A & B to Dispatcher
  */
  static const char_t * dmiChannel1ATPAToDisp = "DMIChannel1_To_Disp_A";
  static const char_t * dmiChannel1ATPBToDisp = "DMIChannel1_To_Disp_B";
  static const char_t * dmiChannel2ATPAToDisp = "DMIChannel2_To_Disp_A";
  static const char_t * dmiChannel2ATPBToDisp = "DMIChannel2_To_Disp_B";

  /**
  * \brief Channel name for BTM-Messages from dispatcher to ATP A & B
  */
  static const char_t * btmhChannelDispToATPA = "Disp_To_BTMH_A";
  static const char_t * btmhChannelDispToATPB = "Disp_To_BTMH_B";

  /**
  *  \brief Channel name for BTM-Messages from ATP A & B to dispatcher
  */
  static const char_t * btmhChannelATPAToDisp = "BTMH_To_Disp_A";
  static const char_t * btmhChannelATPBToDisp = "BTMH_To_Disp_B";

  /**
  *  \brief Channel name for Clock Sync Messages from ATP A & B to dispatcher (SAP32)
  */

  static const char_t * opcClockSyncChannelATPAToDisp = "Opc_Clock_Sync_To_Disp_A";
  static const char_t * opcClockSyncChannelATPBToDisp = "Opc_Clock_Sync_To_Disp_B";

  /**
  * \brief Channel name for Application status messages from dispatcher to ATP A & B
  */
  static const char_t * btmhChannelDispAppDataToATPA = "DispAppData_To_BTMH_A";
  static const char_t * btmhChannelDispAppDataToATPB = "DispAppData_To_BTMH_B";

  /**
  * \brief Channel name for TimeSync messages
  */
  static const char_t * tigrisOffsetChannelNameA = "TigrisOffset2A";
  static const char_t * tigrisOffsetChannelNameB = "TigrisOffset2B";

  /**
   * \brief Channels for Odometery Component for reading telegrams for ATP A.
  */
  static const char_t * odoconfigResponseChannel_A = "Odo_To_UserAppl_A";
  static const char_t * odoMeasureDataTeleChannel_A = "Odo_To_UserAppl_1_A";

  /**
   * \brief Channels for Odometery Component for writing telegrams for ATP A.
  */
  static const char_t * odoConfigTelegramChannel_A = "UserAppl_To_Odo_A";

  /**
   * \brief Channels for Odometery Component for reading telegrams for ATP B.
  */
  static const char_t* odoconfigResponseChannel_B = "Odo_To_UserAppl_B";
  static const char_t* odoMeasureDataTeleChannel_B = "Odo_To_UserAppl_1_B";

  /**
   * \brief Channels for Odometery Component for writing telegrams for ATP B
  */
  static const char_t * odoConfigTelegramChannel_B = "UserAppl_To_Odo_B";

  /**
  * \brief Channel name for LCS-Messages from dispatcher to ATP A & B
  */
  static const char_t * lcsChannelDispToATPA = "Disp_To_VehicleCom_A";
  static const char_t * lcsChannelDispToATPB = "Disp_To_VehicleCom_B";

  /**
  *  \brief Channel name for LCS-Messages from ATP A & B to dispatcher
  */
  static const char_t * lcsChannelATPAToDisp = "VehicleCom_To_Disp_A";
  static const char_t * lcsChannelATPBToDisp = "VehicleCom_To_Disp_B";
 

// OBRD Message Channels

  /**
  * \brief Channel name for OBRD-Messages from dispatcher to ATP A & B
  */
  static const char_t * obrdChannelDispToATPA = "Disp_To_OBRD_A";
  static const char_t * obrdChannelDispToATPB = "Disp_To_OBRD_B";

  /**
  *  \brief Channel name for LCS-Messages from ATP A & B to dispatcher
  */
  static const char_t * obrdChannelATPAToDisp = "OBRD_To_Disp_A";
  static const char_t * obrdChannelATPBToDisp = "OBRD_To_Disp_B";

  /**
  *  \brief Channel name for dispatcher to ATP A & B
  */
  static const char_t * dispatcherToAtpA = "Disp_To_ATP_A";
  static const char_t * dispatcherToAtpB = "Disp_To_ATP_B";

  /**
  *   In/Out Message sizes
  */

  /**
  * \brief Max size of a message on Odo_To_UserAppl_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoConfigResponseMsgSize = 26U;

  /**
  * \brief Max size of a message on UserAppl_To_Odo_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoConfigTeleMsgSize = 58U;

  /**
  * \brief Max size of a message on Odo_To_UserAppl_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoMeasurementDataTelegramMsgSize = 55U;

  /**
  * \brief Max size of an LCSMessage (for largest message-type ECPB Train Composition Message)
  *
  * \brief Max Cars:350 -> ClassD Header + EMP Header + Data(ECPB Train Composition Message) + EMP Trail + ClassD Trail
  * \brief 12 + 17 + (6 + 350*3)  + 4 + 1 = 1090
  */
  static const uint32_t  lcsMsgSize = 1100U;

  /**
  * \brief Max size of an OBRD Message
  *
   * \brief 23 + 250 + 160 = 433
  */
  static const uint16_t  maxOBRDMsgSize = 433U;

  /**
  * \brief Max size of an Dispatcher to ATP message
  *
  * \brief 20
  */
  static const uint32_t  maxDispatcherToAtpMsgSize = 20U;

  /**
  * \brief Max size of a RadioMessage (for largest message-type TrainSetup)
  *
  * \brief Max Cars:350 -> RadioHeader + TrainSetup Header/Trail + 10 VehicleTypeData + 350 VehicleIdData + RadioTrail(CRC + LeadOut)
  * \brief 11 + 35 + 18*10 + 26*350 + 3*8 + 5 = 9355 bytes
  */
  static const uint32_t   maxRadioMessageSize = 9355U;

  /**
  * \brief Max size of a DMI-message (for largest message-type CarNameList)
  * 
  * \brief Max Cars:350 -> Header + Type + Data(CarNameList) + Trailer
  * \brief 5 + 1 + (2 + 351*25) + 4) = 8787
  */
  static const uint16_t maxDMIMessageSize = 8787U;
  
  /**
  *   In/Out Queue sizes
  */

  /**
  * \brief Max no. of Odometer Measurement Data Telegrams on Odo_To_UserAppl_n_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoMeasureDataTeleQueueSize = 6U;

  /**
  * \brief Max no. of telegrams on Odo_To_UserAppl_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoConfigResponseQueueSize = 1U;

  /**
   * \brief Max no. of Odometer Configuration Telegrams on UserAppl_To_Odo_A/B channel (3NSS012264D0033).
  */
  static const uint32_t   odoConfigTeleQueueSize = 1U;

  /**
  * \brief Max no. messages on Radio Channel to TCC
  */
  static const uint32_t   radioChMsgOutQueueSize = 5U;

  /**
  * \brief Max no. messages on Radio Channel from TCC
  */
  static const uint32_t   radioChMsgInQueueSize = 5U;

  /**
  * \brief Max no. messages SPLH to OPC
  */
  static const uint32_t   splQueueSize = 10U;

  /**
  * \brief Max no. messages size OPC to SPLH
  */
  static const uint32_t   splMessageSizeOpcToAtp = 1500U;

  /**
  * \brief Max no. messages size SPLH to OPC
  */
  static const uint32_t   splMessageSizeAtpToOpc = 1500U;

  /**
  * \brief Max no. messages to Application data from OPC to SPLH
  */
  static const uint32_t   appDataQueueSize = 10U;

  /**
  * \brief Max no. messages size Application data to OPC
  */
  static const uint32_t   appDataMessageSize = 11U;
 
  /**
  * \brief Max no. messages TigrisOffset2A/B channel
  */
  static const uint32_t   tigrisOffsetQueueSize = 10U;

  /**
  * \brief Max no. message size TigrisOffset2A/B channel (note only 9 bytes are sent)
  */
  static const uint32_t   tigrisOffsetMessageSize = 64U;

  /**
  * \brief Max number of telegrams in BTM telegram list
  */
  static const uint8_t maxBtmTgmListSize = 4U;

  /** 
  * \brief Max no of messages to read and queue in one cycle from LCS
  */
  static const uint32_t   lcsMessageInQueueSize = 10U;

  /**
  * \brief Max no of messages to write and queue in one cycle to LCS
  */
  static const uint32_t   lcsMessageOutQueueSize = 10U;

  /**
  * \brief Max no of messages to read and queue in one cycle from OBRD
  */
  static const uint32_t   obrdMessageInQueueSize = 5U;

  /**
  * \brief Max no of messages to write queue in one cycle from dispatcher
  */
  static const uint32_t   dispatcherToAtpOutQueueSize = 10U;

  /**
  * \brief Max no of messages to write and queue in one cycle to OBRD
  */
  static const uint32_t   obrdMessageOutQueueSize = 5U;

  /**
  * \brief Maximum size of the DMI In message queue
  */
  static const uint32_t dmiMsgInQueueSize = 5U;

  /**
  * \brief Maximum size of the DMI Out message queue needing ACK
  */
  static const uint8_t dmiAckMsgOutQueueSize =    10U;

  /**
  * \brief Maximum size of the DMI Out message queue not needing ACK
  */
  static const uint8_t dmiNoAckMsgOutQueueSize =  5U;
  
  /**
  * \brief Maximum size of the DMI Out message queue
  */
  static const uint32_t dmiMsgOutQueueSize = dmiAckMsgOutQueueSize + dmiNoAckMsgOutQueueSize;

  /** Parameters revelant only for the simulation environments **********************************************/

  /**
  * \brief I/O Channel(VIOH) name from dispatcher to ATP A & B
  */
  static const char_t * viohChannelDispToATPA = "Disp_To_VIOHSim_A";
  static const char_t * viohChannelDispToATPB = "Disp_To_VIOHSim_B";

  /**
  *  brief I/O Channel(VIOH) name from ATP A & B to dispatcher
  */
  static const char_t * viohChannelATPAToDisp = "VIOHSim_To_Disp_A";
  static const char_t * viohChannelATPBToDisp = "VIOHSim_To_Disp_B";

  /**
  * \brief Channels for Speed Simulation of ATP A/B.
  */
  static const char_t * speedChannelSim_A = "Disp_To_CODSim_A";
  static const char_t * speedChannelSim_B = "Disp_To_CODSim_B";

  /**
  * \brief Read channels for OPC Simulation of ATP A/B.
  */
  static const char_t * opcChannelDispToATPA = "Disp_To_OPCSim_A";
  static const char_t * opcChannelDispToATPB = "Disp_To_OPCSim_B";

  /**
  * \brief Write channels for OPC Simulation of ATP A/B.
  */
  static const char_t * opcChannelATPAToDisp = "OPCSim_A_To_Disp";
  static const char_t * opcChannelATPBToDisp = "OPCSim_B_To_Disp";

  /**
  * \brief Max size of a simulated message from AOSPC to OPCSim.
  */
  static const uint32_t   opcSimMessageSize = 10U;

  /**
  * \brief Max no. messages on channel from AOSPC
  */
  static const uint32_t   opcSimMsgInQueueSize = 3U;

  /**
  * \brief Max no. messages on channel to AOSPC
  */
  static const uint32_t   opcSimMsgOutQueueSize = 3U;

  /**
  * \brief Max no. of messages on Speed simulation channels
  */
  static const uint32_t speedSimChannelQueueSize = 3U;

  /**
  * \brief Maximum Size of SimulatedMovement Telegram
  */
  static const uint8_t speedSimChannelTelSize = 14U;

  /**
  * \brief Max size of a simulated message
  */
  static const uint32_t   maxVIOHMessageSize = 30U;

  /**
  * \brief Maximum size of the VIOH Out message queue
  */
  static const uint32_t   viohMessageInQueueSize = 3U;

  /**
  * \brief Maximum size of the VIOH In message queue
  */
  static const uint32_t   viohMessageOutQueueSize = 3U;
}

#endif

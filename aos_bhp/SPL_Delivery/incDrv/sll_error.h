/**********************************************************************/
/** @file   sll_error.h
 *  @brief  SLL/STL module
 *          The SLL error reasons are defined in FFFIS-057 §5.2.5.9.
 *
 *  @note   The text definitions are used in the STL and SLL.
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: wroewe %
 *                      %version: 2.1.9 %
 *                      %date_created: 2011-07-22 14:20 %
 **********************cvs out of date*********************************
 *                      $Author: wroewe $
 *                      $Revision: 1.28 $
 *                      $Date: 2005/07/27 07:27:59 $
 *                      $Source: P://mpg/sl/sll/sll_error.h,v $
 *
 *  $Log: sll_error.h,v $
 *  Revision 1.28  2005/07/27 07:27:59  wroewe
 *  add new error numbers (greater 0xFF) but not used in the moment (t.b.d.)
 *
 *  Revision 1.27  2005/07/08 14:17:29  wroewe
 *  add comment
 *
 *  Revision 1.26  2005/06/29 14:33:03  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *    Revision 1.25  2005/06/20 08:25:11  wroewe
 *  add ETI_424_STL_Master_timeout and ETI_425_STL_Slave_timeout
 *
 *    Revision 1.24  2005/05/20 14:23:34  mkilianj
 *  converting to doxygen style comments
 *
 *    Revision 1.23  2005/05/12 07:22:42  wroewe
 *  add error number ETI_423_wrong_telegram_on_RefClock_connection
 *
 *    Revision 1.22  2005/04/21 08:21:22  wroewe
 *  add ETI_230_Incorrect_SAP
 *
 *    Revision 1.21  2005/04/04 08:11:24  wroewe
 *  new error number for dynamic and static transfer time
 *
 *    Revision 1.20  2005/03/29 08:22:48  AGeck
 *  New error: lost synchronisation
 *
 *    Revision 1.19  2005/03/11 14:54:03  AGeck
 *  New error text for disconnect from application
 *
 *    Revision 1.18  2005/03/09 10:27:04  wroewe
 *  add ETI_230_PB_Manager_no_sending
 *
 *    Revision 1.17  2005/03/03 14:35:59  wroewe
 *  add error number ETI_417_PB_Manager_no_sending
 *
 *    Revision 1.16  2005/02/28 08:30:09  ageck
 *  add new text for not yet synchronised
 *
 *    Revision 1.15  2005/02/23 13:15:50  wroewe
 *  change the error handling for FFFIS undefined error reasons
 *
 *    Revision 1.14  2004/12/22 14:17:07  mjoost
 *  Due to LZB requirements I linted the file according to MISRA rules
 *
 *    Revision 1.13  2004/12/10 13:19:30  mjoost
 *  Disconnect_Reason_Text is now ENUM driven (for SLL and STL)
 *
 *    Revision 1.12  2004/12/09 14:18:18  mjoost
 *  Disconnect_Reason_Text is now ENUM driven
 *
 *    Revision 1.10  2004/07/29 11:38:28  jdiezper
 *  Add OutOfMemory error
 *
 *    Revision 1.9  2004/06/23 14:11:15  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *    Revision 1.8  2004/06/08 11:00:47  jdiezper
 *  Renumber user defined errors
 *
 *    Revision 1.7  2004/06/03 15:13:16  wroewe
 *  add Send_MCast_Error and MCast_SenderReceiver_Error
 *
 *    Revision 1.6  2004/05/17 07:54:37  wroewe
 *  add Command_Error
 *
 *    Revision 1.5  2004/05/17 07:11:43  wroewe
 *  add Safetylevel_Error
 *
 *    Revision 1.4  2004/05/13 14:16:45  wroewe
 *  add Safelevel_Error
 *
 *    Revision 1.3  2004/05/13 13:41:25  wroewe
 *  add User_Textlength_Error
 *
 *    Revision 1.2  2004/04/30 09:31:37  wroewe
 *  add new error numbers
 *
 *    Revision 1.1  2004/03/26 12:27:27  wroewe
 *  Definition of the Disconnect Reasons in the Disconnect Telegram
 *
 **********************************************************************/

#ifndef SLL_ERROR_H_
#define SLL_ERROR_H_

#include "spl_config.h"
#include "FFFIS_Types.h"

/* the two types of disconnection frames */
#define  FINAL_DISCONNECTED  0
#define  DISCONNECTED        1

/**
 * Standard Error Numbers for Disconnect Telegram.
 ***************************************************************************/
typedef enum
{

    SLL_Disconnect_from_application              = 0x0000u,             /**< disconnect request from application */
    SLL_Bad_version_error                        = 0x0001u,             /**< bad version error */
    SLL_Error_during_connection_setup            = 0x0002u,             /**< error during connection set-up */
    SLL_Authentication_error                     = 0x0003u,             /**< authentication error */
    SLL_Connection_setup_for_existing_connection = 0x0004u,             /**< connection set-up for existing connection */
    SLL_Idle_cycle_timeout                       = 0x0005u,             /**< idle cycle time-out */
    SLL_First_incorrect_receive                  = 0x0006u,             /**< 1st incorrect receive */
    SLL_Second_incorrect_receive                 = 0x0007u,             /**< 2nd incorrect receive */
    /* reserved for SLL (FFFIS-057, §5.2.5.9)      0x0008u - 0x001Eu */
    SLL_Other_reason_for_disconnect              = 0x001Fu,             /**< other reason for disconnect */
    /* reserved for STL (FFFIS-056, §5.3.1.3)      0x0020u - 0x0040u */
    /* reserved for SLL (FFFIS-057, §5.2.5.9)      0x0041u - 0x00FFu */

    SLL_Application_error                        = 0x0100u              /**< application error. No disconnect reason, only key for text table. */
    /* reserved for STL_Application_error          0x0101u */

    /* reserved for LZB                            0x01F0u - 0x01FFu */

} SLL_Disconnect_Reasons_Enum;


/**
 * Here are all errors. They are used for disconnect reason texts and error indications
 ********************************************************************/
typedef enum
{

/**
 *  0:used for test purposes
 ************************************************************************/
    ETI_200_Dummy_Error,
/**
 *  1:do not try to send more characters than a profibus frame can transport
 ************************************************************************/
    ETI_201_User_Textlength_Overflow,
/**
 *  2:used for test purposes
 ************************************************************************/
    ETI_202_Different_Master_Setting,
/**
 *  3:used if a operation for P2P connections is applied to multicast
 *  connection and vice versa
 ************************************************************************/
    ETI_203_Different_P2P_MCast_Setting,
/**
 *  NOT used
 ***********************************************************************
    ETI_204_Master_and_ConReq_Telegram,
 */
/**
 *  4:if the user wants to create connection with unknown safety or
 *  a remote station wants to connect with unknown safety level
 ************************************************************************/
    ETI_205_Safetylevel_is_unknown,
/**
 *  5:a P2P slave node is not intended to receive connect confirm telegrams
 *  (SLL level)
 ************************************************************************/
    ETI_206_Slave_and_ConConfirm_Telegram,
/**
 *  NOT used
 ***********************************************************************
    ETI_207_Master_and_AuthReq_Telegram,
    ETI_208_Slave_and_AuthConfirm_Telegram,
    */
/**
 *  6:internal error, our state machine is weird
 ************************************************************************/
    ETI_209_Incorrect_State,
/**
 *  7:as the name describes...
 ************************************************************************/
    ETI_210_Sequence_Number_Overflow,
/**
 *  8:the user wants the library to do things which are not allowed in
 *  accordance with device safety level
 *  (maybe someone tries to set up a clock master for a SIL0 device)
 ************************************************************************/
    ETI_211_Safety_Level_is_not_SL4,

/**
 *  9:a multicast receiver is not allowed to send
 ************************************************************************/
    ETI_212_Node_is_Receiver,
/**
 *  10:what the hack does the remote station wants us to do?
 ************************************************************************/
    ETI_213_Command_is_unknown,
/**
 *  11:incompatible x version for sll
 ************************************************************************/
    ETI_214_Incorrect_SLL_Version,
/**
 *  we are suprised, we are not allowed to accept a connect request
 *  in current state
 ************************************************************************/
    /* 12: */ ETI_215_Unexpected_Telegram_Connect_Request,
    /* 13: */ ETI_217_Unexpected_Telegram_Connect_Confirm,
    /* 14: */ ETI_218_Unexpected_Telegram_Authen_Request,
/**
 *  15: validation of authentication failed
 ************************************************************************/
    ETI_219_Incorrect_Authentication,
/**
 *  16: validation of authentication confirmation failed
 ************************************************************************/
    ETI_220_Unexpected_Telegram_Authen_Confirm,
/**
 *  not used
 ***********************************************************************
    ETI_221_Received_Disconnect_Telegram,
 */
/**
 *  we received an idle frame, but we are in authentication state
 ************************************************************************/
    /* 17: */ ETI_222_Unexpected_Telegram_Idle,
    /* 18: */ ETI_223_Unexpected_Telegram_Data,
/**
 *  19: the P2p link was not established within break time (usually 5sec)
 ************************************************************************/
    ETI_224_Break_time_out,
/**
 *  20: the peer station doesn't send idle nor data frames
 ************************************************************************/
    ETI_225_Idle_Cycle_time_out,
/**
 *  21: a timer elapsed, but we do not have any idea what does the timer stands for
 ************************************************************************/
    ETI_226_unknown_timer_triggered,
/**
 *  22: we got a telegram, but the sequence number doesn't match our expectations.
 *  This may occur in case the PB interface is flooded with frames and some get lost
 ************************************************************************/
    ETI_227_Incorrect_Sequence_Number,
/**
 *  23: the frame length does not correspond to FFFIS
 ************************************************************************/
    ETI_228_Incorrect_Telegram_Length,
/**
 *  24: what should I say..
 ************************************************************************/
    ETI_229_Incorrect_CRC,
/**
 *  not used
 ***********************************************************************
    ETI_230_Incorrect_SAP,
*/
/**
 *  25: our PB hardware refuse send service
 ************************************************************************/
    ETI_231_PB_Manager_no_sending,
/**
 *  26: a P2P slave node is not intended to send a connect confirm telegrams
 ************************************************************************/
    ETI_401_Master_Slave_Error_STL,
/**
 *  27: there are exclusive services just for P2P...
 *  we received a p2p frame on a multicast connection
 ************************************************************************/
    ETI_402_Expect_P2P_instead_of_MCast_STL,
/**
 *  28: we received a multicast frame on a P2P connection
 ************************************************************************/
    ETI_403_Expect_MCast_instead_of_P2P_STL,
/**
 *  29: incompatible stl x version
 ************************************************************************/
    ETI_404_Bad_Version_STL,
/**
 *  30: time stamp check: undershoot minimum Safety Time Tolerance
 ************************************************************************/
    ETI_405_STTmin_Error,
/**
 *  31: time stamp check: maximum Safety Time Tolerance exceeded
 ************************************************************************/
    ETI_406_STTmax_Error,
/**
 *  32: stl data frame to short
 ************************************************************************/
    ETI_407_Minimal_Length_error,
/**
 *  33: we get a status indication from sll, but the stl state is not STS_connecting
 ************************************************************************/
    ETI_408_STL_state_is_not_STS_Connecting,
/**
 *  34: we received a frame on a not created connection, sll state != connection open
 ************************************************************************/
    ETI_409_Incorrect_State_STL_Con_not_open,
/**
 *  35: attempt to connect a final disconnected link
 ************************************************************************/
    ETI_410_Incorrect_State_STL_Stations_are_disc,
/**
 *  36: received a STL_ReadytoRun_Tlg_Indication or STL_Run_Tlg_Indication but in
 *  the current state we are not allowed to process these data
 ************************************************************************/
    ETI_411_Incorrect_State_STL_STS_Connected,
/**
 *  37: got a STE_Process_Status_Indication,  but state does not correspond to STS_Confirmed
 ************************************************************************/
    ETI_412_Incorrect_State_STL_STS_Confirmed,
/**
 *  38: got a STE_Process_Status_Indication, but STL state does not correspond to STS_Connecting
 ************************************************************************/
    ETI_413_Incorrect_State_STL_STS_Connecting,
/**
 *  39: got a unepected STL_ReadytoRun_Tlg_Indication, I'm a connection slave
 ************************************************************************/
    ETI_414_Master_Slave_Error_STL,
/**
 *  40: self explaining
 ************************************************************************/
    ETI_415_Data_Length_too_long_STL,
/**
 *  41: send data request if a connection is not open (Connection not created)
 ************************************************************************/
    ETI_416_STL_State_STS_Connection_open,
/**
 *  42: Static_Transfer_Time_is_too_small
 ************************************************************************/
    ETI_417_Static_Transfer_Time_is_too_small,

/**
 *  43: refuse any telegrams as long node is not synchronised
 ************************************************************************/
    ETI_418_Not_Yet_Synchronised_STL,
/**
 *  44: user disconnect
 ************************************************************************/
    ETI_419_STL_Disconnect_by_application,
/**
 *  45: we run out of sync
 ************************************************************************/
    ETI_420_Lost_Synchronisation_STL,
/**
 *  46: Static_Transfer_Time_is_too_big
 ************************************************************************/
    ETI_421_Static_Transfer_Time_is_too_big,
/**
 *  Dyn Transfer Time is too small or too large
 ************************************************************************
    ETI_422_Dyn_Transfer_Time_out_of_range,
*/
/**
 *  47: somebody tried to send non clock frames on clock SAP
 ************************************************************************/
    ETI_423_wrong_telegram_on_RefClock_connection,
/**
 *  48: STL doesn't start up within Connection_Setup_Time AND node is master
 ************************************************************************/
    ETI_424_STL_Master_timeout,
/**
 *  49: STL doesn't start up within Connection_Setup_Time AND node is slave
 ************************************************************************/
    ETI_425_STL_Slave_timeout,
/**
    50: Check that Local and Ref times increase failed
 ************************************************************************/
    ETI_426_Monotonic_Check_Error,
/**
 *  51: short term drift out of range
 ************************************************************************/
    ETI_428_Short_Term_Drift_Error,
/**
 *  52: long term drift out of range
 ************************************************************************/
    ETI_429_Long_Term_Drift_Error,
/**
 *  53: not enough valid RefSync Telegrams in TimeForLongTermDriftCheck
 ************************************************************************/
    ETI_430_RefSync_Tlg_Number_Error,
/**
 *  54: No CE_No_RefSync_Telegram within LocalClockMaxReSyncInterval
 ************************************************************************/
    ETI_431_No_RefSync_Telegram,
/**
 *  55: Static Bus Transfer Time is too big
 ************************************************************************/
    ETI_432_Static_Bus_Transfer_Time_is_too_big,
/**
 *  56: Static Bus Transfer Time is too small
 ************************************************************************/
    ETI_433_Static_Bus_Transfer_Time_is_too_small,
/**
 *  57: Dynamic Bus Transfer Time is too big
 ************************************************************************/
    ETI_434_Dynamic_Bus_Transfer_Time_is_too_big,
/**
 *  58: Dynamic Bus Transfer Time is too small
 ************************************************************************/
    ETI_435_Dynamic_Bus_Transfer_Time_is_too_small,
/**
 *  59: Dynamic Transfer Time is too big
 ************************************************************************/
    ETI_436_Dynamic_Transfer_Time_is_too_big,
/**
 *  60: Dynamic Transfer Time is too small
 ************************************************************************/
    ETI_437_Dynamic_Transfer_Time_is_too_small,

/**
 *  not used
 ************************************************************************
    ETI_601_SLL_Application_Error,
*/
/**
 *  61: it is not possible to create a multicast sender without SIL 4
 ************************************************************************/
    ETI_602_SLL_Multicast_sender_is_not_SIL4,
/**
 *  62: for test purposes
 ************************************************************************/
    ETI_603_SLL_Disconnect_for_Multicast,
/**
 *  63: for test purposes
 ************************************************************************/
    ETI_604_SLL_Bad_SLLApiProcessEvent_parameter,
/**
 *  64: for test purposes
 ************************************************************************/
    ETI_605_SLL_Bad_SLLApiProcessEvent_Event,
/**
 *  65: unknown command from application
 ************************************************************************/
    ETI_650_STL_Received_Bad_Command,
/**
 *  66: weird things happened, events and telegrams misbehave
 ************************************************************************/
    ETI_651_STL_Bad_Connection_Parameters,
/**
 *  67: unknown disconnect reason from upper layer
 ************************************************************************/
    ETI_652_STL_Bad_Disconnect_Reason,
/**
 *  68: received connect request from user when system not running
 ************************************************************************/
    ETI_653_STL_Not_Running_ConReq,
/**
 *  69: received request from user to send Mcast data when system not running
 ************************************************************************/
    ETI_654_STL_Not_Running_MCast_DataReq,
/**
 *  70: Target Address out of range (see 3NGM005016)
 ************************************************************************/
    ETI_655_SLL_Target_Address_out_of_range,
/**
 *  71: Source Address out of range (see 3NGM005016)
 ************************************************************************/
    ETI_656_SLL_Source_Address_out_of_range,
/**
 *  72: SAP out of range (see 3NGM005016)
 ************************************************************************/
    ETI_657_SLL_SAP_out_of_range,
/**
 *  73: Idle Cycle out of range (see 3NGM005016)
 ************************************************************************/
    ETI_658_SLL_Idle_Cycle_out_of_range,
/**
 *  74: Time between two Errors out of range (see 3NGM005016)
 ************************************************************************/
    ETI_659_SLL_Time_between_two_Errors_out_of_range,
/**
 *  75: Break Time out of range (see 3NGM005016)
 ************************************************************************/
    ETI_660_SLL_Break_Time_out_of_range,
/**
 *  76: Dual Bus out of range (not used)
 ************************************************************************/
    ETI_661_SLL_Dual_Bus_out_of_range,
/**
 *  77: StartupSynchronisationTimeLimit is too big (see 3NGM005016)
 ************************************************************************/
    ETI_662_Startup_Sync_Time_Limit_is_too_big,
/**
 *  78: SafeTimeLayerReStartInterval is too big (see 3NGM005016)
 ************************************************************************/
    ETI_663_ReStart_Interval_is_too_big,
/**
 *  79: SafeTimeLayerStartupInterval is too big (see 3NGM005016)
 ************************************************************************/
    ETI_664_Startup_Interval_is_too_big

} SPL_Error_Text_Index;


/**
 * Retrieves an (constant) text message from the static table and stores into
 * the user provided buffer.
 *
 * The function shall copy the text into the provided Buffer
 * if all these conditions are met:
 *     Index is found in the table.
 *     Buffer is not null.
 *     BufferSize is non zero.
 * If conditions are not met, nothing is copied.
 *
 * More over, if the BufferSize is less than the actual text length,
 * only BufferSize bytes shall be copied. In this case, a
 * terminating 0 character is alwaysd included in Buffer.
 *
 *       @param Index   A value from the SPL_Error_Text_Index enumeration.
 *
 *       @param Buffer  A non-null pointer to writable memory.
 *
 *       @param BufferSize   The number of bytes that Buffer points to.
 *
 *        @return Result     =0: If the text referenced by Index was found
 *                               in the table,
 *                           =-1: If the text referenced by Index was NOT found
 *                                in the table.
 ********************************************************************/
spINT SPL_Get_Error_Text (const SPL_Error_Text_Index Index,
                                spCHAR * const Buffer,
                          const spUINT         BufferSize);

#endif /* SLL_ERROR_H_ */











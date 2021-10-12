/** @file 
 * The telegram structures are described in the FFFIS-057.
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by:    jkrupp %
 *                      %version:       13.2.4 %
 *                      %date_created:  2013-09-12 12:10 %
 **********************************************************************
 *
 *  Revision    Date        Name        Description
 *  13.2.4      2013/09/12  jkrupp      remove sll_error.h because it is not needed here
 *  13.2.3      2013/06/26  jkrupp      change TIME to spTIME because of change in FFFIS_Types.h
 *
 *  undocumented changes after switch from CVS to Synergy
 *
 *  $Log: sll_telegram.h,v $
 *  Revision 1.37  2005/07/27 07:10:20  wroewe
 *  add comment
 *
 *  Revision 1.36  2005/07/11 11:28:47  wroewe
 *  add comment
 *
 *  Revision 1.35  2005/06/30 09:25:40  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *    Revision 1.34  2005/05/20 14:23:36  mkilianj
 *  converting to doxygen style comments
 * 
 *    Revision 1.33  2005/04/21 07:49:48  wroewe
 *  add comment
 * 
 *    Revision 1.32  2005/04/08 12:17:53  wroewe
 *  set SLL_P2P_MAX_CONNECT_DATA_LENGTH to 17
 * 
 *    Revision 1.31  2005/03/08 08:46:44  wroewe
 *  add SPL_PLATFORM_LZB (no pl_manager for LZB)
 * 
 *    Revision 1.30  2005/03/04 09:12:54  wroewe
 *  reorganization of SLL_Telegram_Struct
 * 
 *    Revision 1.29  2005/03/03 14:29:11  wroewe
 *  reorganization of the telegram structure
 * 
 *    Revision 1.28  2005/03/01 16:11:41  mjoost
 *  changed comment for local timestamp
 * 
 *    Revision 1.27  2005/02/17 14:59:07  wroewe
 *  add the implicit data in the SLL_Telegram_Struct
 * 
 *    Revision 1.26  2005/02/17 14:56:34  wroewe
 *  changes for using the implicit data in the SLL_Telegram_Struct
 * 
 *    Revision 1.25  2004/12/22 16:14:11  mjoost
 *  bugfix after linting
 * 
 *    Revision 1.24  2004/12/20 11:05:16  mjoost
 *  add a cast to avoid warnings from MISRA
 * 
 *    Revision 1.23  2004/12/02 12:19:45  jdiezper
 *  Remove WORDS/DWORDS from telegrams structures.
 * 
 *    Revision 1.22  2004/11/30 13:51:28  wroewe
 *  correction of SPL_INCLUDE_DUMP_FUNCTIONS
 * 
 *    Revision 1.21  2004/11/26 10:54:07  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 * 
 *    Revision 1.20  2004/09/30 09:30:17  wroewe
 *  add some comment
 * 
 *    Revision 1.19  2004/08/12 13:13:39  mjoost
 *  add some lines with comments
 * 
 *    Revision 1.18  2004/08/05 09:44:50  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 * 
 *    Revision 1.17  2004/07/29 11:38:34  jdiezper
 *  Use PL_FDL defines
 * 
 *    Revision 1.16  2004/06/23 14:11:18  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 * 
 *    Revision 1.15  2004/06/18 09:15:29  jdiezper
 *  Use the new telegram structure (only one length)
 * 
 *    Revision 1.14  2004/05/19 10:00:17  jdiezper
 *  Update SLL_Telegram structure(s)
 * 
 *    Revision 1.13  2004/04/30 09:31:40  wroewe
 *  change Disconnect_Reason to type spBYTE
 * 
 *    Revision 1.12  2004/04/23 15:12:31  wroewe
 *  add Disconnect_Reason_Text[SLL_P2P_MAX_DISCONECT_REASON_LENGTH]
 * 
 *    Revision 1.11  2004/04/22 14:26:23  wroewe
 *  add #define SLL_MCAST_MAX_DATA_LENGTH             230
 * 
 *    Revision 1.10  2004/04/14 09:09:29  wroewe
 *  add some comment
 * 
 *    Revision 1.9  2004/04/06 12:27:48  jdiezper
 *  Simulates a Network by binding connections togethertest/test_sim_binder.c
 * 
 *    Revision 1.8  2004/04/05 12:00:26  jdiezper
 *  Implement telegram length
 * 
 *    Revision 1.7  2004/03/30 15:19:58  jdiezper
 *  Use the SLL_Dump_Telegram functions
 * 
 *    Revision 1.6  2004/03/30 14:26:04  jdiezper
 *  Change the SLL_Api.
 *  Split Events in Events and Telegrams.
 *  Recursively propagate the changes to all files.
 * 
 *    Revision 1.5  2004/03/26 12:59:18  wroewe
 *  change spBYTE Disconnect_Reason in SLL_DisconnectReasons_Enum Disconnect_Reason
 * 
 *    Revision 1.4  2004/03/17 14:30:02  wroewe
 *  - new code
 * 
 *    Revision 1.3  2004/02/26 15:03:31  jdiezper
 *  Add some new modules
 *  Initial
 *
 ***********************************************************************/
#ifndef SLL_TELEGRAM_H_
#define SLL_TELEGRAM_H_

#include "spl_config.h"
#include "FFFIS_Types.h"

/**
 * Max P2P telegram length (e.g. Header + User Data + CRC).  */
#define SLL_P2P_MAX_TELEGRAM_LENGTH          244U 
                                                 
/**
 *   User Data + CRC. */
#define SLL_P2P_MAX_CONNECT_DATA_LENGTH       17U 
/**
 *   User Data + CRC. */
#define SLL_P2P_MAX_DATA_LENGTH              242U 
/**
 *    User Data.      */
#define SLL_P2P_MAX_DISCONECT_REASON_LENGTH   40U 

#ifdef SPL_A_DUMMY
/**
 * Special indication value for processing of incoming telegrams in A-Code simulation  */
#define SLL_INDICATION_NO_CRC 0x88
#endif

/**
 *  Max Mcast telegram length (e.g. Header + User Data + CRC).      */
#define SLL_MCAST_MAX_TELEGRAM_LENGTH        244U 

/**
 *     User Data + CRC.       */
#define SLL_MCAST_MAX_DATA_LENGTH            236U 


/*###################################################################
  #
  #                    P E E R     2     P E E R
  #
  ###################################################################*/


/**
 * The common header structure of all SLL P2P telegrams.
 ********************************************************************/
typedef struct
{
  spBYTE Sequence_Bits_08_01;  /**< The lower part of the 32 bits sequence number */
  spBYTE Command;              /**< A telegram identifier.*/
} SLL_Msg_Header_Struct;


/**
 * @Connect Request Telegram (FFFIS-057, §5.2.1.3).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header;                /**< Common header (Sequence+Command) */
  spBYTE                    Initial_Sequence_Number[4]; /**< Filled with a random number    */
  spBYTE                    Idle_Cycle_Timeout[2];    /**< Max. time between the reception of two idle/data telegrams.*/
  spBYTE                    Version_X;                /**< SLL software version.*/
  spBYTE                    Version_Y;                /**< SLL software version.*/
  spBYTE                    Version_Z;                /**< SLL software version.*/
  spBYTE                    Dual_Bus;                 /**< 0 : The node is not equipped with dual busses
                                                           1 : The node is equipped with dual busses.*/
  spBYTE                    Data [SLL_P2P_MAX_CONNECT_DATA_LENGTH];/**< Data from upper layers.*/
} SLL_Connect_Request_Struct;

/**
 * @Connect Confirm Telegram (FFFIS-057, §5.2.2.2).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header;           /**< Common header (Sequence+Command).*/
  spBYTE                    Initial_Sequence_Number[4]; /**< Filled with a random number    */
  spBYTE                    Idle_Cycle_Timeout[2];/**< Max. time between the reception of two idle/data telegrams.*/
  spBYTE                    Version_X;      /**< SLL software version.*/
  spBYTE                    Version_Y;      /**< SLL software version.*/
  spBYTE                    Version_Z;      /**< SLL software version.*/
  spBYTE                    Dual_Bus;       /**< 0 : The node is not equipped with dual busses
                                                 1 : The node is equipped with dual busses.*/
  spBYTE                    Data [SLL_P2P_MAX_CONNECT_DATA_LENGTH];/**< Data from upper layers.*/
} SLL_Connect_Confirm_Struct;

/**
 * @Authentication Telegram (FFFIS-057, §5.2.3.4).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header;      /**< Common header (Sequence+Command).*/
  spBYTE                  Authentication_Number[4];/**<The authentication number is the
                                                       received random number after 
                                                       processing by the defined algorithm.*/
} SLL_Authenticate_Request_Struct;     

/**
 * @Authentication Acknowledgement Telegram (FFFIS-057, §5.2.4.4).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header;      /**< Common header (Sequence+Command).*/
  spBYTE                  Authentication_Number [4];/**< The authentication number 
                                                         is the random number received after 
                                                         processing by the defined algorithm. */ 
} SLL_Authenticate_Confirm_Struct;


/**
 * @Disconnect Telegram (FFFIS-057, §5.2.5.9).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct          Header;    /**< Common header (Sequence+Command).*/
  spBYTE                         New_Setup_Desired;/**< 0: final disconnect, 1: non final disconnect*/
  spBYTE                         Disconnect_Reason;/**< defined in FFFIS subset-057 and subset-056  */
  spBYTE                         Disconnect_Reason_Text [SLL_P2P_MAX_DISCONECT_REASON_LENGTH];
                                                   /**< Vendor specific text, amend for diagnostics.*/
} SLL_Disconnect_Struct;

/**
 * @Idle Telegram (FFFIS-057, §5.2.6.6).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header;      /**< Common header (Sequence+Command).*/
} SLL_Idle_Struct;

/**
 * @Data Telegram (FFFIS-057, §5.2.7.2).
 ********************************************************************/
typedef struct
{
  SLL_Msg_Header_Struct   Header; /**< Common header (Sequence+Command).*/
  spBYTE                  Net_Data [SLL_P2P_MAX_DATA_LENGTH];/**< Application data.*/
} SLL_Data_Struct;

/**
 * @All P2P telegram structures (FFFIS-057, §5.2).
 ********************************************************************/
typedef struct
{
  /**
   * @The different P2P telegrams.
   ********************************************************************/
  union
  {
      spBYTE                           Bytes [SLL_P2P_MAX_TELEGRAM_LENGTH];/**< The telegram as byte array.*/
      SLL_Msg_Header_Struct            Header;   /**< Common header (Sequence+Command).*/
      SLL_Connect_Request_Struct       ConReq;   /**< Connect Request Telegram.*/
      SLL_Connect_Confirm_Struct       ConCon;   /**< Connect Confirm Telegram.*/
      SLL_Authenticate_Request_Struct  AuthReq;  /**< Authentication Telegram.*/
      SLL_Authenticate_Confirm_Struct  AuthCon;  /**< Authentication Acknowledgement Telegram.*/
      SLL_Disconnect_Struct            Disconnect;/**< Disconnect Telegram.*/
      SLL_Idle_Struct                  Idle;     /**< Idle Telegram.*/
      SLL_Data_Struct                  Data;     /**< Data Telegram.*/
  } as;

} SLL_P2P_Telegram_Struct;



/*###################################################################
  #
  #                         M U L T I C A S T
  #
  ###################################################################*/

/**
 * @Multicast Telegram.
 ********************************************************************/
typedef struct
{
  spBYTE                    Compatibility_X;     /**< SLL Software version.*/
  spBYTE                    Compatibility_Y;     /**< SLL Software version.*/
  spBYTE                    Compatibility_Z;     /**< SLL Software version.*/
  spBYTE                    Command;             /**< These command have been reserved in
                                                      the Point-to-Point connections (=8Dh).*/
  spBYTE                    Sequence_Number [4]; /**< Full sequence number.*/
  spBYTE                    Net_Data [SLL_MCAST_MAX_DATA_LENGTH];/**< Application data.*/
} SLL_MCast_Struct;

/**
 * @All MCast telegram structures (FFFIS-057, §6.2).
 ********************************************************************/
typedef union
{
  /**
   * @MCast telegram as array and structure.
   ********************************************************************/	 
  union
  {
      spBYTE                       Bytes [SLL_MCAST_MAX_TELEGRAM_LENGTH];/**< The telegram as byte array.*/
      SLL_MCast_Struct             MCast;   /**< MCast Telegram.*/
  } as;

} SLL_MCast_Telegram_Struct;


/**
 * Structure of the implicit data + telegrams (FDL data) + local timestamp.
 * @warning
 * to keep this structure portable it must have an even length to 
 * avoid misalignment and implicit padding - a dummy byte can be 
 * use to balance it.  This is only important when communicating between
 * different platforms over ethernet.
 **************************************************************************/
typedef struct
{
  /**
  * length of the P2P implicit data FFFIS subset-057 §5.1.3.4.
  *************************************************************************/	 
  #define P2P_IMPLICIT_DATA_LENGTH      8U 
  /**
  * length of the MCast implicit data FFFIS subset-057 §6.2.1.5.
  *************************************************************************/	 
  #define MCAST_IMPLICIT_DATA_LENGTH    5U


  /*
   * @warning 
   * <b> Don't change the element order of this structure.</b>
   ************************************************************************/
  spBYTE Length;             /**< length of the FDL data FFFIS subset-057 §5.1.3.4 or §6.2.1.5.*/
                             
  spBYTE Target_Address;     /**<  [0..127] sending telegram: destination address 
                                  telegram reception: address of this node
                                  In case of reception, it is the Physical address of this station
                                     It must match the field 'TS' in the PL_Driver_Initialisation_Struct.
                                  In case of transmission, it is the destination address.
                                
                                   In both cases:
                                      Value 127 is mandatory for SDN telegrams.
                                      Value in [0..126] must be used for SDA telegrams. */

  spBYTE Source_Address;     /**<   sending telegram: address of this node [0..126] 
                                  In case of reception, it is the Physical address of the sender.
                                  In case of transmission, it is the Physical address of this station
                                     It must match the field 'TS' in the PL_Driver_Initialisation_Struct
                                     passed to PL_Initialize_Bus upon initialization. */

  spBYTE Target_SAP;         /**< [0..62] the target SAP */

  spBYTE Source_SAP;         /**< [0..62] The source SAP.  Source_SAP = Target_SAP.
                                  This SAP must be have been successfully enabled (PL_Enable_SAP) */

  spBYTE Sequence_Bits_16_09;/**< byte no. 1 of the sequence number. */
  spBYTE Sequence_Bits_24_17;/**< byte no. 2 of the sequence number. */
  spBYTE Sequence_Bits_32_25;/**< byte no. 3 (highest of 4 bytes) of the sequence number.*/

 /**
  * structure of the different P2P telegrams or the MCast telegram.
  ********************************************************************/	 
  union
  {
      spBYTE                    Bytes [244];/**< Raw telegram bytes.*/
      SLL_P2P_Telegram_Struct   P2P;        /**< P2P telegram.*/
      SLL_MCast_Telegram_Struct MCast;      /**< MCast telegram.*/
  } as;

  spTIME Local_Timestamp;      /**< For incoming telegrams only.
                                 If the interface provides some timestamp for
                                   received telegrams, it is the responsability
                                   of the Driver to convert the interface timestamp
                                   to the Host CPU Time stamp.
                                
                                 If the interface does NOT provide a timestamp for
                                   received telegrams, it is the responsability
                                   of the Driver to fill this field (as soon as possible)
                                   with the current value of the Host CPU (FFFIS_Clock());
                                
                                 This timestamp shall be usedc to compute the
                                   age of the telegram in the Safe Time Layer */

#ifndef SPL_PLATFORM_LZB

  spBYTE FDL_Mode;           /**< FDL_MODE_SDN or FDL_MODE_SDA */   

  spBYTE Indication;         /**< For incoming telegrams only: true or false
                                  needed in pl_manager to seperate data telegrams
                                  filtering by service (SDA, SDN,...) is NOT sufficient 
								
								  Also used for A-code simulation to indicate that 
								  telegram shall be processed without CRC: SLL_INDICATION_NO_CRC*/

#endif

} SLL_Telegram_Struct;


#ifdef SPL_INCLUDE_DUMP_FUNCTIONS
/**
 *    @The function dumps the content of the telgram.
 *       
 *       @param pConnectionName        Connection name.
 * 
 *       @param pUserText              user defined output text.
 *       
 *       @param Verbose                output level (0 no output, higher level => more output).
 * 
 *       @param pTlg                   Telegram. 
 ******************************************************************************/
void SLL_Dump_Telegram (const spCHAR * const pConnectionName,
                        const spCHAR * const pUserText,
                        const spUINT         Verbose,
                        const SLL_Telegram_Struct * const pTlg);
#endif


#endif /* SLL_TELEGRAM_H_ */

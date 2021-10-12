/*******************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation, 2005-2007
 *
 * We reserve all rights in this file and in the information
 * contained therein. Reproduction, use or disclosure to third
 * parties without express authority is strictly forbidden.
 *
 ********************************************************************************/
/** @file
 *  @brief Generic Profibus Safety Layer application interface (API).
 *
 *  Corresponding to 3NGM005003_PBSL_Generic_API.doc
 *
 */
/*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by:    jkrupp %
 *                      %version:       4.1.10 %
 *                      %date_created:  2013-06-03 15:35 %
 *
 *  Revision 4.1.10  2013/06/03     jkrupp
 *      remove const at function return value
 *  Revision 4.1.9  2012/11/21     jkrupp
 *      add comments for UDP_Driver_Resend and UDP_Driver_Configure
 *  Revision 4.1.8  2012/10/15     jkrupp
 *      adapt parameters of function UDP_Driver_Configure
 *  Revision 4.1.7  2012/09/27     wroewe
 *      added SPL_LogPBFWVersion(), print out firmware version for Vipco driver
 *  Revision 4.1.6  2012/09/17     ageck
 *      changed protoype of UDP_Driver_Configure
 *  Revision 4.1.5  2012/09/04     jkrupp
 *      added UDP_Driver_Configure and UDP_Driver_Resend
 *      renamed PL_Ethernet_Add_Node/Set_Self to Ethernet_Add_Node/Set_Self
 *  Revision 4.1.4  2012/07/23     jkrupp
 *  - add function PL_Ethernet_Add_Node, PL_Ethernet_Set_Self and 
 *    Ethernet_Driver_Flush
 *  - cleanup redundant information in file header
 *  - add History
 *
 ***************************************************************************/
#ifndef SPL_API_INT_H_
#define SPL_API_INT_H_

/******************************************************************************
* Includes
******************************************************************************/
#include "FFFIS_Types.h"
#include "spla_protocol_parameters.h"

/*************************************************************************************
**************************************************************************************
DO NOT ADD ANY INCLUDES OR IFDEFS TO THAT FILE TO KEEP APPLICATION INTERFACE SIMPLE!!!
*************************************************************************************
**************************************************************************************/

/******************************************************************************
* Type definitions
******************************************************************************/

typedef struct S_Version_Info {

/** @defgroup FIELD_LENGTH  Field length for version strings
 */
/*@{*/
    #define MAX_VERSION_STRING 10
    #define MAX_SIL_STRING     10
    #define MAX_DRIVER_STRING  50
    #define MAX_AB_CODE_STRING 20
    #define MAX_DEBUG_STRING   20
/*@}*/

    spCHAR  Version_String[MAX_VERSION_STRING];  /**< SPL version string */
    spCHAR  SIL[MAX_SIL_STRING];                 /**< SPL safety level */
    spCHAR  Driver[MAX_DRIVER_STRING];           /**< integrated driver software (memory, profibus...)*/
    spCHAR  AB_CODE[MAX_AB_CODE_STRING];         /**< code version (A or AB) */
    spCHAR  Debug[MAX_DEBUG_STRING];             /**< is it a SPL release or debug version? */

} SPL_Version;

 /************************************************************************************
 * Structure: SPL_Connection_Struct
 *
 * Purpose:   Structure with connection data, transparent at application level
 *************************************************************************************/
struct SPL_Connection_Struct;

typedef struct SPL_Connection_Struct * SPL_Connection;
   /**< Type for connection link object handle.
        Hiding the pointer to the corresponding structure.         
        @warning Please use h-Prefix for variable and parameter names.
      */

/******************************************************************************
* Functions prototypes (used in single coded and diversified version)
******************************************************************************/

/* SPL_get_version provides some information to the user about the SPL he is working on.
 *  Function is not safety relevant. Therefor no token is used.
 *  Please note: safe applictionns must not react on result.
 *
 *      @param [in]  SPL_Version structure
 *
 *      @param [out] SPL_Version structure
 *
 *      @return      nothing
 *
 *      @note:       !!! the version string has to be modified by hand !!!
 *
 ***************************************************************************/
void SPL_Get_Version (SPL_Version * const Version);



/**
 *  Initialise the SPL library.
 *      The Profibus Manager shall be initialized.
 *      The Profibus Driver, FDL level driver and board shall be initialized too.
 *
 *      @param [in] ProfibusAddress
 *                    The profibus address of this application/station [0..126]
 *
 *      @param [in] usedDriverType  Type of Profibus driver to use, as in list below
 *
 *      @param [in] pDriverInitStruct
 *                    Structure with FDL parameters, defined in Pl_driver2.h
 *                    If this parameter is 0U, the default values are used,
 *                    My_Profibus_Address is copied to the TS field of the structure.
 *                            If this parameter is 0U, the default values are used,
 *                            My_Profibus_Address is copied to the TS field of the structure.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return      pResult  SPL_INIT_OK on success, else
 *                            SPL_INIT_BAD_PB_ADDRESS <br>
 *                            SPL_INIT_BAD_DRIVER <br>
 *                            SPL_INIT_BAD_PARAMETER <br>
 *                            SPL_INIT_DRIVER_ERROR <br>
 *
 *****************************************************************************/

int SPL_Initialise ( const unsigned int  ProfibusAddress,
                     const unsigned int  usedDriverType,
                     const PL_Driver_Initialisation_Struct * const pDriverInitStruct,
                     int * const         pResult,
                     const int           Token  );

#define SPL_Initialyze SPL_Initialise   /**< keep the old function name. */

/**
 *   Starts the shutdown of the FDL level (driver and board) and
 *   shuts down the Profibus Safety Layer library.
 *   It may, depending on the FDL level driver and hardware,
 *   take some time until the FDL level has been completely shut down
 *   and can be eventually restarted. In case of an error it may
 *   be impossible to restart without a hardware reset.
 *
 *      @param [in] Token  Input token value.
 *
 *      @return     Token  Output token value. Inverse value of input token.
 *
 ***************************************************************************/

int SPL_Shutdown ( const int Token );




/**
 * Initialize the Reference Clock object as Master.
 *  Create a SPL Connection Object for sending the Reference Clock Telegrams.
 *  Store the parameters in internal structures, they shall be used in
 *  SPL_Clock_Management.
 *
 *      @param [in] Phase1Frequency Time between sending of two sync and reference
 *                 time telegrams in the start interval
 *                 (see FFFIS Safe Time Layer subset-056, chapter 7.3)in ms,
 *                 recommended time in subset 059, 4.3.\n
 *                 50 - 200 ms
 *
 *      @param [in] Phase1Duration  Duration for sending sync and reference time
 *                telegrams with a high frequency in ms,
 *                recommended time in subset 059, 4.3. \n
 *                30s
 *
 *      @param [in] Phase2Frequency  Time between sending of two sync and reference
 *                time telegrams after the start interval
 *                (see FFFIS Safe Time Layer subset-056, chapter 7.3) in ms,
 *                recommended time in subset 059, 4.3.\n
 *                200 - 400 ms
 *
 *      @param [in] LocalClockInaccuracy  Local clock inaccuracy in ms,
 *                recommended value = 1
 *
 *      @param [in] SAP  SAP for clock connection,
 *                recommended value (subset 035) = 1000002 = 20H = 3210
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *  @return
 *     @retval SPL_CLOCK_OK    : No error
 *     @retval SPL_CLOCK_ERROR : Internal clock error
 *
 *
 * @note
 *    Assuming SPL_Clock_Management is called often enough,
 *
 *    for the first Phase_1_Duration milliseconds, a Reference Clock
 *    Telegram shall be sent every Phase_1_Frequency milliseconds;
 *
 *    after the first Phase_1_Duration milliseconds, a Reference Clock
 *    Telegram shall be sent every Phase_2_Frequency milliseconds.
 *
 ***************************************************************************/
/* function is only available if library has been compiled for SIL 4 */
int SPL_Reference_Clock_Master ( const unsigned int  Phase1Frequency,
                                 const unsigned int  Phase1Duration,
                                 const unsigned int  Phase2Frequency,
                                 const unsigned int  LocalClockInaccuracy,
                                 const unsigned int  SAP,
                                 int * const         pResult,
                                 const int           Token  );

/**
 * Read the safety relevant parameters of Reference Clock Master back in B format.
 *     @param [out] LocalClockInaccuracy:  Local clock inaccuracy in ms and B format
 *
 *  @return
 *     @retval ~SPL_CLOCK_OK    : No error
 *     @retval ~SPL_CLOCK_ERROR : Clock role or data undefined or pointer invalid
 *     @retval ~SPL_AB_ERROR    : Stored AB error (only debug mode, else immediate exit)
 *
 ***************************************************************************/
/* function is only available if library has been compiled diversified for SIL 4*/
int SPL_Get_ClockMasterDataB ( unsigned int* const LocalClockInaccuracy );



/**
 * Initialize the Reference Clock object as Slave.
 * Create a SPL Connection object for receiving the Reference Clock telegrams.
 * Store the parameters in internal structures, they shall be used
 * in SPL_Clock_Management.
 *
 *      @param [in] LocalClockMaxResyncInterval
 *                            according to subset 056, 9.2,
 *                            in ms, recommended value (subset 059) = 5 000
 *
 *      @param [in] MaxClockInaccuracyAfterAdjustFactor
 *                            according to subset 056, 9.6.
 *                            This constant gives the maximum allowed error
 *                            of LocalRefTime in relation to RefTime.
 *                            in ms, recommended value (subset 059) =  100
 *
 *      @param [in] TimeForLongTermDriftCheck
 *                            according to subset 056, 9.7,
 *                            in ms, recommended value (subset 059) =  10 000
 *
 *      @param [in] MinNumberOfSyncAndRefMsgReceived
 *                            according to subset 056, 9.8,
 *                            recommended value (subset 059) =  30
 *
 *      @param [in] LocalClockInaccuracy Local clock inaccuracy in ms, recommended value = 1
 *
 *      @param [in] SAP       SAP for clock connection, recommended value
 *                            (subset 035) = 1000002 = 20H = 3210
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token    Input token value.
 *
 *      @return      Token    Output token value. Inverse value of input token.
 *
 *      @return      pResult  SPL_CLOCK_OK on success, SPL_CLOCK_ERROR otherwise.
 *
 *
 * @note
 *    The parameters are used for the check, if the clock is still synchronized,
 *    or lost synchronisation because of missing Reference Clock telegrams,
 *    or short or long term drift over the limits.
 *
 ***************************************************************************/

int SPL_Reference_Clock_Slave ( const unsigned int  LocalClockMaxResyncInterval,
                                const unsigned int  MaxClockInaccuracyAfterAdjustFactor,
                                const unsigned int  TimeForLongTermDriftCheck,
                                const unsigned int  MinNumberOfSyncAndRefMsgReceived,
                                const unsigned int  LocalClockInaccuracy,
                                const unsigned int  SAP,
                                int * const         pResult,
                                const int           Token );

/**
 * Read the safety relevant parameters of Reference Clock Slave back in B format.
 *      @param [out] LocalClockMaxResyncInterval in ms in B format
 *      @param [out] MaxClockInaccuracyAfterAdjustFactor in ms in B format
 *      @param [out] TimeForLongTermDriftCheck in ms in B format
 *      @param [out] MinNumberOfSyncAndRefMsgReceived in B format
 *      @param [out] LocalClockInaccuracy: Local clock inaccuracy in ms and B format
 *
 *  @return
 *     @retval ~SPL_CLOCK_OK    : No error
 *     @retval ~SPL_CLOCK_ERROR : Clock role or data undefined or pointer invalid
 *     @retval ~SPL_AB_ERROR    : Stored AB error (only debug mode, else immediate exit)
 *
 ***************************************************************************/
/* function is only available if library has been compiled diversified */
int    SPL_Get_ClockSlaveDataB     (    
            unsigned int* const    LocalClockMaxResyncInterval,
            unsigned int* const    MaxClockInaccuracyAfterAdjustFactor,
            unsigned int* const    TimeForLongTermDriftCheck,
            unsigned int* const    MinNumberOfSyncAndRefMsgReceived,
            unsigned int* const    LocalClockInaccuracy
        );        

/**
 * Create an object describing a SPL peer-to-peer connection.
 *
 *      @param [in] pName      For debugging purposes only. No more than 10 characters.
 *                            Use 0U if unused.
 *
 *      @param [in] TargetProfibusAddress  The Profibus address of the remote
 *                                          station. Values   [0..126].
 *
 *      @param [in] SAP        Profibus Service Access Point [0..62].
 *
 *      @param [in] Master     =0: The remote station is master, we are slave. The remote
 *                            station shall call SPL_Connect, we must wait until
 *                            is_Active returns IS_CONNECTED. \n
 *                            =1: We are master. We shall call SPL_Connect, we
 *                            must wait until is_Active returns IS_CONNECTED. \n
 *
 *      @param [in] SafetyLevel  =SPL_SAFETY_LEVEL_LOW_SAFE (Safety level 0), \n
 *                            =SPL_SAFETY_LEVEL_MEDIUM_SAFE (Safety level 2), \n
 *                            =SPL_SAFETY_LEVEL_HIGH_SAFE (Safety level 4) \n
 *
 *      @param [in] IdleTime  Time in ms, this time will be sent to the target.
 *                            If the target does not receive a data or idle telegram within this time,
 *                            the target will disconnect the connection, i.e. the station must send
 *                            data or idle telegrams frequently enough, that the target will receive
 *                            them within this time.
 *                            Depending on the compile option SPL_OPTION_IDLE_BYAPP,
 *                            the application must call SPL_sendIdle to send Idle telegrams
 *                            if it does not send data (option set) or call
 *                            SPL_checkIdle (option not set) at the end of an application cycle,
 *                            to let the Profibus Safety layer library check, if an idle
 *                            telegram has to be sent.
 *
 *      @param [in] pProtocolParameters Structure with Profibus Safety Layer protocol
 *                            parameters for P2P connections as described above.
 *                            If the parameter is 0U the default values will be used.
 *
 *      @param [out] phConnection  Pointer to structur with Point-to-Point-Conection data.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pP2PConnection A valid SPL_Connection object pointer on success. 0U on any error.
 *
 *****************************************************************************/

int SPL_Create_P2P ( const unsigned char * const  pName,
                     const unsigned int           TargetAddress,
                     const unsigned int           SAP,
                     const unsigned int           Master,
                     const unsigned int           SafetyLevel,
                     const unsigned int           IdleTime,
                     const SPL_P2P_Protocol_Parameters * const pProtocolParameters,
                     SPL_Connection * const       phConnection,
                     const int                    Token );

/**
 * Read the parameters of a peer-to-peer connection back in B format.
 *      @param [in]  hConnection  A pointer to a valid SPL_Connection object,
 *                            returned by SPL_Create_P2P.
 *      @param [out] TargetProfibusAddress in B format
 *      @param [out] SAP in B format        
 *      @param [out] Master in B format
 *                            =~0: The remote station is master, we are slave.
 *                            =~1: We are master 
 *
 *      @param [out] SafetyLevel in B format 
 *                            = ~SPL_SAFETY_LEVEL_LOW_SAFE (Safety level 0), \n
 *                            = ~SPL_SAFETY_LEVEL_MEDIUM_SAFE (Safety level 2), \n
 *                            = ~SPL_SAFETY_LEVEL_HIGH_SAFE (Safety level 4) \n
 *
 *      @param [out] IdleTime Time in ms in B format
 *
 *      @param [out] pProtocolParameters Structure with Profibus Safety Layer protocol 
 *                            parameters for P2P connections in B format
 *
 *  @return
 *     @retval ~SPL_OK    : No error
 *     @retval ~SPL_IS_BAD_OBJECT : Bad connection parameter or pointer invalid
 *     @retval ~SPL_AB_ERROR    : Stored AB error (only debug mode, else immediate exit)
 *
 ***************************************************************************/
/* function is only available if library has been compiled diversified */
int    SPL_Get_P2PDataB     (    
            SPL_Connection    hConnection,
            unsigned int* const    TargetProfibusAddress,
            unsigned int* const    SAP,
            unsigned int* const    Master,
            unsigned int* const    SafetyLevel,
            unsigned int* const    IdleTime,
            SPL_P2P_Protocol_ParametersB* const    pProtokolParameters
        );    

/**
 * Create an object describing a SPL multicast connection.
 *
 *      @param [in] pName     For debugging purposes only. No more than 10 characters.
 *                            Use 0U if unused.
 *
 *      @param [in] SAP       Profibus Service Access Point [0..62].
 *
 *      @param [in] Receiver  =0: We are SENDING on this connection. \n
 *                            SPL_Send is allowed. \n
 *                            SPL_Recv is not allowed. \n
 *                            =1: We are RECEIVING on this connection. \n
 *                            SPL_Recv is allowed. \n
 *                            SPL_Send is not allowed. \n
 *
 *      @param [in] SafetyLevel  =SPL_SAFETY_LEVEL_LOW_SAFE (Safety level 0) \n
 *                            =SPL_SAFETY_LEVEL_MEDIUM_SAFE (Safety level 2) \n
 *                            =SPL_SAFETY_LEVEL_HIGH_SAFE (Safety level 4) \n
 *                            if Receiver=0, only Safety level 4 is allowed! \n
 *
 *      @param [in] pProtocolParameters Structure with Profibus Safety Layer protocol
 *                            parameters for multicast connections as described above.
 *                            If the parameter is 0U the default values will be used.
 *      @param [out] phConnection  Pointer to structur with Multicast-Conection data.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pMCConnection A valid SPL_Connection object pointer on success. 0U on any error.
 *
 ***************************************************************************/

int SPL_Create_MCast ( const unsigned char * const  pName,
                       const unsigned int           SAP,
                       const unsigned int           Receiver,
                       const unsigned int           SafetyLevel,
                       const SPL_MCast_Protocol_Parameters * const pProtocolParameters,
                       SPL_Connection * const       phConnection,
                       const int                    Token  );


/**
 * Read the parameters of a Multicast connection back in B format.
 *      @param [in]  hConnection  A pointer to a valid SPL_Connection object,
 *                            returned by SPL_Create_MCast.
 *      @param [out] SAP in B format        
 *      @param [out] Receiver in B format
 *                            =~0: We are SENDING on this connection..
 *                            =~1: We are RECEIVING on this connection. 
 *
 *      @param [out] SafetyLevel in B format 
 *                            = ~SPL_SAFETY_LEVEL_LOW_SAFE (Safety level 0), \n
 *                            = ~SPL_SAFETY_LEVEL_MEDIUM_SAFE (Safety level 2), \n
 *                            = ~SPL_SAFETY_LEVEL_HIGH_SAFE (Safety level 4) \n
 *
 *      @param [out] pProtocolParameters Structure with Profibus Safety Layer protocol 
 *                            parameters for Multicast connections in B format
 *
 *  @return
 *     @retval ~SPL_OK    : No error
 *     @retval ~SPL_IS_BAD_OBJECT : Bad connection parameter or pointer invalid
 *     @retval ~SPL_AB_ERROR    : Stored AB error (only debug mode, else immediate exit)
 *
 ***************************************************************************/
/* function is only available if library has been compiled diversified */
int    SPL_Get_MCastDataB     (    SPL_Connection    hConnection,
            unsigned int* const    SAP,
            unsigned int* const    Receiver,
            unsigned int* const    SafetyLevel,
            SPL_MCast_Protocol_ParametersB* const    pProtokolParameters
        );        

/**
 *  Retreive Profibus telegrams from Profibus driver
 *           and dispatch them to Profibus Manager FIFOs.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @return Result        =SPL_BUS_OK on success, <br>
 *                            =SPL_BUS_FDL_ERROR on FDL error, <br>
 *                            =SPL_BUS_UNEXPECTED as warning on unexpected telegrams, <br>
 *                            =SPL_BUS_FDL_FIFO for receiving overflow FDL. <br>
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 ******************************************************************************/

int SPL_Bus_Management ( int * const   pResult,
                         const int     Token );



/**
 * Send or receive Reference Clock Telegrams.
 * Check synchronisation and compute clock adjustmentfactor for slave.
 * MUST be called cyclically.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token    Input token value.
 *
 *      @return      Token    Output token value. Inverse value of input token.
 *      @return      pResult  SPL_CLOCK_OK on success, SPL_CLOCK_ERROR otherwise.
 *
 * @note
 * The return value does not indicate whether the clock is still
 * synchronised, use SPL_Get_Clock_Status to test this
 *
 ***************************************************************************/

int SPL_Clock_Management( int * const pResult,
                          const int   Token );


/**
 * Get the status of an existing connection (SL and FDL).
 * Process received idle and disconnect telegrams for P2P connections.
 *
 *      @param [in]  hConnection  A pointer to a valid SPL_Connection object,
 *                            returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in]  CycleTime  Maximum time since telegrams were read last
 *                     time from bus, good estimation is application cycle time.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult     Status of connection, coded as below:
 *                          =SPL_IS_BAD_OBJECT <br>
 *                          =SPL_IS_IDLE <br>
 *                          =SPL_IS_CONNECTED <br>
 *                          =SPL_IS_CONNECTING <br>
 *                          =SPL_IS_CLOSED <br>
 *                          =SPL_IS_CLOSED_2 <br>
 *                          =SPL_IS_SAP_ERROR <br>
 *                          =SPL_IS_DRIVER_ERROR <br>
 *
 ***************************************************************************/

int SPL_IsActive( SPL_Connection     hConnection,
                  const unsigned int CycleTime,
                  int * const        pResult,
                  const int          Token  );

#define SPL_isActive SPL_IsActive /**< keep old function name. */


/**
 * Get the Profibus Safety Layer Reference Time.
 *
 *      @param [out] pReferenceTime pointer to variable for reference value.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult  Returns the Profibus Safety Layer Reference Time in ms.
 *
 ****************************************************************************/

int SPL_Get_Reference_Time( unsigned long int * const pReferenceTime,
                            int * const               pResult,
                            const int                 Token );



/**
 * Get disconnect reason and disconnect reason text of the last disconnect
 * (by local or remote node) for a disconnected connection.
 *
 *      @param [in] hConnection  A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P.
 *
 *      @param [in] pReason   Output parameter for disconnect reason, 1 byte.
 *
 *      @param [in] pReasonText   A pointer to some writable buffer
 *                                of at least 'ReasonTextSize' bytes.
 *
 *      @param [in] ReasonTextSize  Size of 'TextBuffer'.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token    Input token value.
 *
 *      @return      Token    Output token value. Inverse value of input token.
 *
 *      @return pResult       >=0: Size of disconnect reason text, <br>
 *                            ok, information returned, otherwise, <br>
 *                            =SPL_GETR_BAD_OBJECT <br>
 *                            =SPL_GETR_NO_DISCONNECT <br>
 *
 ***************************************************************************/

int SPL_Get_Reason ( SPL_Connection     hConnection,
                     unsigned char * const pReason,
                     unsigned char * const pReasonText,
                     const unsigned int ReasonTextSize,
                     int * const        pResult,
                     const int          Token );                             




/**
 * Get the global status of the library, board and bus.
 * The status is a bit mask with one bit set for every defined problem observed.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token    Input token value.
 *
 *      @return      Token    Output token value. Inverse value of input token.
 *
 *      @return pResult       Error bit mask as described above,
 *                            combination of SPL_GETS_GLB_-error bit masks.
 *
 *
 ***************************************************************************/

int SPL_Get_Status( unsigned long int * const pStatus,
                    const int                 Token );


/**
 * Get the status of a connection and the SAP belonging to it.
 * The status is a bit mask with one bit set for every defined problem observed.
 *
 *      @param [in]  hConnection  A pointer to a valid SPL_Connection object, returned by
 *                            SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult        Error bit mask as described in spl_a_error_codes,
 *                            Combination of SPL_GETS_CON_-error bit masks.
 *
 ***************************************************************************/

int SPL_Get_ConnectionStatus( SPL_Connection            hConnection,
                              unsigned long int * const pStatus,
                              const int                 Token );


/**
 * Output buffer for FDL stations.
 * The buffer must be 127 bytes long. Each byte corresponds to one physical
 * address of the bus and is set to 1 if the physical address is on the bus
 * and to 0 if not.
 *
 *      @param [in]  pBuffer  A pointer to some writable buffer.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token    Input token value.
 *
 *      @return      Token    Output token value. Inverse value of input token.
 *
 *      @return pResult       =SPL_GETL_OK: Ok, information returned.  Otherwise <br>
 *                            =SPL_GETL_NOT_SUPPORTED <br>
 *                            =SPL_GETL_NOT_READY <br>
 *
 ***************************************************************************/

int SPL_Get_Livelist( unsigned char * const pBuffer,
                      long int * const      pStatus,
                      const int             Token );



/**
 * Get the Reference Clock Status.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult  Returns the Reference Clock Status: <br>
 *                       SPL_CLOCK_SYNCHRONIZED <br>
 *                       SPL_CLOCK_SYNCHRONIZING <br>
 *                       SPL_CLOCK_SYNCHRO_LOST <br>
 *
 ***************************************************************************/

int SPL_Get_Clock_Status( int * const pStatus,
                          const int   Token );


/**
 * Open an existing SPL P2P Connection.
 *
 *      @param [in]  hConnection  A valid SPL_Connection object,
 *                                    returned by SPL_Create_P2P
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult       =SPL_CONNECT_OK: If the function call was successfull.
 *                            It does *NOT* imply that the connection is Connected (this
 *                            must be checked with the SPL_IsActive function). <br>
 *                            Error codes are as follows: <br>
 *                            =SPL_CONNECT_BAD_OBJECT <br>
 *                            =SPL_CONNECT_BAD_STATE <br>
 *                            =SPL_CONNECT_FINAL_DISC <br>
 *                            =SPL_CONNECT_FDL_ERROR <br>
 *                            =SPL_CONNECT_FDL_ERROR <br>
 *
 *
 ***************************************************************************/
int SPL_Connect( SPL_Connection hConnection,
                 int * const    pResult,
                 const int      Token );





/**
 * Disconnect connection on application request.
 *
 *      @param [in] hConnection A pointer to a valid SPL_Connection object,
 *                      returned by SPL_Create_P2P.
 *
 *      @param [in] RequestNewSetup
 *                      0 = Final disconnect, connection can not be reconnected. \n
 *                      1 = Non final disconnect, connection can be reconnected.
 *
 *      @param [in] pDisconnectReason  Text describing reason for disconnect,
 *                  Maximum length including terminating zero byte = 40(decimal)
 *                  longer texts will be cut.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult =SPL_DISCON_OK: If the function call was successful. <br>
 *                      Or error codes as described below <br>
 *                      =SPL_DISCON_BAD_OBJECT <br>
 *                      =SPLB_DISCON_BAD_TYPE <br>
 *                      =SPLB_DISCON_ALREADY_DISC <br>
 *                      =SPLB_DISCON_FDL_ERROR <br>  #
 
   ***************************************************************************                                                                             
   @return Function Return Value:
      @retval  SPL_DISCON_OK           : No error
      @retval  SPL_DISCON_BAD_OBJECT   : Internal error, bad TheConnection parameter
      @retval  SPL_DISCON_BAD_TYPE     : Disconnect not possible because of type of connection
      @retval  SPL_DISCON_ALREADY_DISC : Disconnect not possible because connection                                          
                                         is already disconnected
      @retval  SPL_DISCON_FDL_ERROR    : Sending of disconnect telegram not possible 
                                         because of error at FDL level, connection locally disconnected
      @retval  SPL_AB_ERROR            : A and B code comparison error in diversified version
 
 *
 ***************************************************************************/

int SPL_Disconnect( SPL_Connection     hConnection,
                    const int          NewSetupDesired,
                    const unsigned char * const pDisconnectReason,
                    int * const        pResult,
                    const int          Token  );




/**
 * Read user data from a connection.
 *
 *      @param [in] hConnection  A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in] pBuffer         A pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *
 *      @param [in] BufferSize     Size of 'Buffer'.
 *
 *      @param [in] pReceptionTime  Time the telegram was received by the PL_Driver,
 *                            in ms, as Profibus bus reference time.
 *                            The parameter can be set to 0U, if the application
 *                            is not interested in the reception time.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult =Positive value:  Data available!
 *                            Buffer has been filled up to this length. <br>
 *                            =0: No data available. No error. <br>
 *                            =SPL_RECV_BAD_OBJECT: The 'hConnection' seems invalid. <br>
 *                            =SPL_RECV_BAD_STATE:  Receiving on a Multicast Sender <br>
 *                            connection ? Receiving not in Connected state? <br>
 *                            =SPL_RECV_MCAST_LOST: Second error on multicast connection <br>
 *                            =SPL_AB_ERROR: A and B code comparison error in diversified version <br>

 ***************************************************************************
 >> buffer size returned through pResult
 ***************************************************************************/

int SPL_recv( SPL_Connection            hConnection,
              unsigned char * const     pBuffer,
              const unsigned int        BufferSize,
              unsigned long int * const pReceptionTime,
              int * const               pResult,
              const int                 Token  );



/**
 * Send data on a connection.
 *
 *      @param [in] hConnection  A pointer to avalid SPL_Connection object.
 *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in]  pBuffer       A pointer to some readable buffer
 *                                  of at least 'BufferSize' bytes.
 *
 *      @param [in] BufferSize     Size of 'Buffer'.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult       =SPL_SEND_OK: If the function call was successfull. \n
 *                            =SPL_SEND_BAD_OBJECT: Error, bad hConnection parameter. \n
 *                            =SPL_SEND_BAD_STATE: Connection is in a state, where no telegram can be sent. \n
 *                            =SPL_SEND_BAD_SIZE: Buffersize > maximum data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)). \n
 *                            =SPL_SEND_FDL_FIFO: FDL FIFO for sending overflow. \n
 *                            =SPL_SEND_FDL_ERROR: other FDL driver error on sending. \n
 *                            =SPL_AB_ERROR: A and B code comparison error in diversified version. \n
 *
 ***************************************************************************/

int SPL_send ( SPL_Connection              hConnection,
               const unsigned char * const pBuffer,
               const unsigned int          BufferSize,
               int * const                 pResult,
               const int                   Token  );

/**
 * Send an idle telegram to the remote node.
 * No idle telegram timeout checking is done.
 *
 *      @param [in]  hConnection  A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @return pResult       =SPL_SEND_OK: If the function call was successfull. Or <br>
 *                            =SPL_SEND_BAD_OBJECT <br>
 *                            =SPL_SEND_BAD_STATE <br>
 *                            =SPL_SEND_BAD_SIZE <br>
 *                            =SPL_SEND_FDL_FIFO <br>
 *                            =SPL_SEND_FDL_ERROR <br>
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *
 ***************************************************************************/
/* library must have been compiled with SPL_OPTION_IDLE_BYAPP for this function
   this is always true for the standard diversified library for VCU-lite */

int SPL_sendIdle( SPL_Connection hConnection,
                  int * const    pResult,
                  const int      Token );


/**
* Checks, if an idle telegram has to be sent to avoid a disconnect by
* the remote node because of idle timeout. If necessary, the idle telegram
* to the remote node is sent.
*
*      @param [in]  hConnection  A pointer to a valid SPL_Connection object
*                                returned by SPL_Create_P2P.
*
*      @param [in]  CycleTime    Maximum time until next call of SPL_checkIdle in ms.
*
*      @param [out] pResult Pointer to variable for function return value.
*
*      @param [in]  Token  Input token value.
*
*      @return      Token  Output token value. Inverse value of input token.
*
*      @return pResult       =SPL_SEND_OK: If the function call was successfull. <br>
*                            =SPL_SEND_BAD_OBJECT <br>
*                            =SPL_SEND_BAD_STATE <br>
*                            =SPL_SEND_BAD_SIZE <br>
*                            =SPL_SEND_FDL_FIFO <br>
*                            =SPL_SEND_FDL_ERROR <br>
*
***************************************************************************/
/* library must have been compiled without SPL_OPTION_IDLE_BYAPP for this function */

int SPL_checkIdle( SPL_Connection     hConnection,
                   const unsigned int CycleTime,
                   int * const        pResult,
                   const int          Token );




/******************************************************************************
* Functions prototypes (used in sdiversified version only)
* only available if library has been compiled diversified
******************************************************************************/


/**
 * Read user data from a connection.
 *
 *      @param [in] hConnection  A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in] pBufferA        A pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *                            BufferA receiving the data for A code in A format
 *
 *      @param [in] pBufferB        B pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *                            BufferB receiving the data for B code in B format
 *
 *      @param [in] BufferSize     Size of 'Buffer'.
 *
 *      @param [in] pReceptionTimeA  A-Code Time the telegram was received by the PL_Driver,
 *                            in ms, as Profibus bus reference time.
 *                            The parameter can be set to 0U, if the application
 *                            is not interested in the reception time.
 *
 *      @param [in] pReceptionTimeB Time of the B-Code.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult =Positive value:  Data available!
 *                            Buffer has been filled up to this length. 
 *       @retval 0                   : No data available. No error.
 *       @retval SPL_RECV_BAD_OBJECT : The 'hConnection' seems invalid.
 *       @retval SPL_RECV_BAD_STATE  : Receiving on a Multicast Sender
 *                                    connection ? Receiving not in Connected state? \n
 *       @retval SPL_RECV_MCAST_LOST : Second error on multicast connection
 *       @retval SPL_AB_ERROR        : A and B code comparison error in diversified version \n
 *
 ***************************************************************************/

int SPL_recvAB( SPL_Connection            hConnection,
                unsigned char * const     pBufferA,
                unsigned char * const     pBufferB,
                const unsigned int        BufferSize,
                unsigned long int * const pReceptionTimeA,
                unsigned long int * const pReceptionTimeB,
                int   *             const pResult,
                const int                 Token );


/**
 * Send data on a connection.
 *
 *      @param [in] hConnection  A pointer to valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in] pBufferA        A pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *                            BufferA receiving the data for A code in A format
 *
 *      @param [in] pBufferB        B pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *                            BufferB receiving the data for B code in B format
 *
 *      @param [in] BufferSize     Size of 'BufferA' and 'BufferB'.
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @param [in]  Token  Input token value.
 *
 *      @return      Token  Output token value. Inverse value of input token.
 *
 *      @return pResult             Pointer to returned value.
 *       @retval SPL_SEND_OK         : If the function call was successfull.
 *       @retval SPL_SEND_BAD_OBJECT : Error, bad hConnection parameter.
 *       @retval SPL_SEND_BAD_STATE  : Connection is in a state, where no telegram can be sent.
 *       @retval SPL_SEND_BAD_SIZE   : Buffersize > maximum data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)).
 *       @retval SPL_SEND_FDL_FIFO   : FDL FIFO for sending overflow.
 *       @retval SPL_SEND_FDL_ERROR  : other FDL driver error on sending.
 *       @retval SPL_AB_ERROR        : A and B code comparison error in diversified version.
 *
 ***************************************************************************/

int SPL_sendAB ( SPL_Connection              hConnection,
                 const unsigned char * const pBufferA,
                 const unsigned char * const pBufferB,
                 const unsigned int          BufferSize,
                 int * const                 pResult,
                 const int                   Token );

/**
 * Send data on a connection.
 *
 *      @param [in] hConnection A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
 *
 *      @param [in] pBufferB   B pointer to some writable buffer of at least
 *                            'BufferSize' bytes.
 *                            BufferB receiving the data for B code in B format
 *
 *      @param [in] BufferSize Size of 'BufferB' (B-format).
 *
 *      @param [out] pResult  Pointer to variable for function return value.
 *
 *      @return pResult             Pointer to returned value.
 *       @retval SPL_SEND_OK         : If the function call was successfull.
 *       @retval SPL_SEND_BAD_OBJECT : Error, bad hConnection parameter.
 *       @retval SPL_SEND_BAD_STATE  : Connection is in a state, where no telegram can be sent.
 *       @retval SPL_SEND_BAD_SIZE   : Buffersize > maximum data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)).
 *       @retval SPL_SEND_FDL_FIFO   : FDL FIFO for sending overflow.
 *       @retval SPL_SEND_FDL_ERROR  : other FDL driver error on sending.
 *       @retval SPL_AB_ERROR        : A and B code comparison error in diversified version.
 * 
 ***************************************************************************/
void SPL_sendB (       SPL_Connection        hConnection,
                       const unsigned char * const pBufferB,
                       const unsigned int          BufferSize,
                       int   *         const pResult );


/**
 ********************FUNCTION DOCUMENTATION**************************************/
int SPL_Get_Reference_TimeAB ( unsigned long int * const pReferenceTimeA,
                               unsigned long int * const pReferenceTimeB,
                               int   *             const pResult,
                               const int                 Token );


/**
 ********************FUNCTION DOCUMENTATION**************************************/
void SPL_IsActiveB (    SPL_Connection     hConnection,
                        int *        const pResult );




/**
 * Get measured transfertimes of connection
 *
 *      @param [in] hConnection  A pointer to a valid SPL_Connection object.
 *                            Returned by SPL_Create_P2P.
 *
 *      @param [in] pTransferTimeCount   Output parameter: number of valid transfer times,
 *                                     -1 in case of error;
 *
 *      @param [in] pMinTransferTime   Output parameter: Minimum measured valid transfer time
 *
 *      @param [in] pMaxTransferTime   Output parameter: Maximum measured valid transfer time
 *
 *      @param [in] pAvgTransferTime   Output parameter: Sliding average valid transfer time
 *
 *      @param [in] pDevTransferTime   Output parameter: Sliding average transfer time deviation
 *
 ***************************************************************************/

void SPL_Get_TransferTimes ( SPL_Connection     hConnection,
                             int * const pTransferTimeCount,
                             int * const pMinTransferTime,
                             int * const pMaxTransferTime,
                             int * const pAvgTransferTime,
                             int * const pDevTransferTime);

#ifdef SPL_INCLUDE_DRIVER_VIPCO
/**
 * Print out the firmware version of VipCo driver
 *
 ***************************************************************************/
void SPL_LogPBFWVersion(void);
#endif

/**
*@brief Set IP address and port form the remote node (outgoing connection)
*   @param [in] Profibus_Physical_Address       currently not used
*   @param [in] HostName_Or_Address             remote IP address
*   @param [in] Port                            remote Port for outgoing data
*   @return  spTRUE everything is ok, otherwise spFALSE.
*******************************************************************************/ 
spBOOL Ethernet_Add_Node (  spBYTE   Profibus_Physical_Address,
                            spCHAR * HostName_Or_Address,
                            spUINT16 Port);

/**
*@brief Set IP address and port form the local host (incomming connection)
*   @param [in] Profibus_Physical_Address       currently not used
*   @param [in] HostName_Or_Address             local IP address
*   @param [in] Port                            local Port of incomming data
*   @return spTRUE everything is ok, otherwise spFALSE.
*******************************************************************************/ 
spBOOL Ethernet_Set_Self (  spBYTE   Profibus_Physical_Address,
                            spCHAR * HostName_Or_Address,
                            spUINT16 Port);

/**
*@brief Send all pending SPL telegrams on outgoing Ethernet connection
* Send all pending SPL telegrams, gathered in internal buffer, on outgoing Ethernet connection.
* To be called by application, if no more telegrams are expected in current task cycle.
*    @return  PL_DRIVER_NO_ERROR on success, PL_DRIVER_ERROR otherwise
*******************************************************************************/ 
spINT Ethernet_Driver_Flush ( void );

/**
*@brief The application has to call the resend function to trigger the resend of all 
* datagrams that have not yet been sent the configured number of times.
*    @return  PL_DRIVER_NO_ERROR on success, PL_DRIVER_ERROR otherwise
*******************************************************************************/
spINT UDP_Driver_Resend ( void );

/**
*@brief Configure parameters of the Ethernet driver based on UDP
*   @param [in] resendCount     How often a UDP datagram  is send
*   @param [in] resendDelay     Minimum delay between resends of the datagram
*   @param [in] sendBufferSize  Size of Ringbuffer
*   @param [in] datagramSize    Length of one datagram
*   @param [in] maxRecvGap      Max sequence number delay
*   @param [in] statisticTime   Time between two statistic entries in AE-log.
*                               If time = 0 the statistic is disabled
*   @return  PL_DRIVER_NO_ERROR on success, PL_DRIVER_INVAL otherwise
*******************************************************************************/
spINT UDP_Driver_Configure (    spUINT16 resendCount,
                                spUINT16 resendDelay,
                                spUINT16 sendBufferSize,
                                spUINT16 datagramSize,
                                spUINT16 maxRecvGap,
                                spUINT16 statisticTime );

#endif /* SPL_API_INT_H_ */

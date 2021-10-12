/** @file
 *  SPL Protocol Parameters
 ****************************************************************************************
 *  Generic Profibus Safety Layer application interface (API).
 *    Header file with function calls and structure definitions
 *    Corresponding to 3NGM005003D014_PBSL_Generic_API.doc, Version 2
 *    with extensions and changes for EVC and odometer,
 *    new driver interface and completed protocol parameters.
 *
 *
 *  Implements Common API and a-code protocol parameters.  
 *
 *
 *  used by spla_manage.h
 *  used by spla_connect.h
 *  used by spla_crc.h
 *  used by spla_clock_time.h
 *  used by spla_send.h
 *  used by spla_receive.h
 *  used by splb.h           for comparison functions
 *
 *  uses   switch SPL_ACODE_ONLY, SPL_B_DUMMY
 *
 */
/****************************************************************************************
 *                    Bombardier Transportation
 *
 *****************************************************************************************/
#ifndef SPLA_PROTOCOL_PARAMETERS_H_
#define SPLA_PROTOCOL_PARAMETERS_H_

#include "FFFIS_Types.h" /**< include the basic type definitions for SPL */

/*************************************************************************************
**************************************************************************************
DO NOT ADD ANY INCLUDES OR IFDEFS TO THAT FILE TO KEEP APPLICATION INTERFACE SIMPLE!!!
*************************************************************************************
**************************************************************************************/

/******************************************************************************
* Defines
******************************************************************************/

/**
 * @name
 * <b>Safety levels for SPL_Create_P2P/MCast.</b>
 *****************************************************************************/
/*@{*/
#define SPL_SAFETY_LEVEL_LOW_SAFE       17 /**< low SPL user safety level. */
#define SPL_SAFETY_LEVEL_MEDIUM_SAFE    55 /**< medium SPL user safety level. */
#define SPL_SAFETY_LEVEL_HIGH_SAFE       8 /**< high SPL user safety level. */
/*@}*/

/******************************************************************************
* Type definitions
******************************************************************************/

/**
 * Structure holding the Profibus FDL parameters.
 * Recommended values can be found in SUBSET-035-V211, Chapter 14.1.1.3.
 ******************************************************************************/
typedef struct
{
    spBYTE    TS;    /**< This Station physical address. A value in [0..126].
                          Unique for each station. */
   
    enum PL_Baud_Rate_Tag    
    {
        PL_BR_9_600     = 1, /**< Avoid zero value to detect uninitilazed variables */
        PL_BR_19_200,
        PL_BR_45_45,
        PL_BR_93_75,
        PL_BR_187_500,
        PL_BR_500_000,
        PL_BR_1_500_000,  /**< Preferred value in FFFIS/ERTMS applicaton */
        PL_BR_3_000_000,
        PL_BR_6_000_000,
        PL_BR_12_000_000
    } Baud_Rate;          /**< Baud rate. */

    spUINT32 Min_T_SDR;  /**< Minimum Station Delay of Responders in tBit unit */

    spUINT32 Max_T_SDR;  /**< Maximum Station Delay of Responders in tBit unit */

    spUINT32 T_SL;        /**< Slot Time in tBit unit */

    spUINT32 T_QUI;       /**< Quiet Time in tBit unit */

    spUINT32 T_SET;       /**< Setup Time in tBit unit */

    spUINT32 T_TR;        /**< Time Target Rotation in tBit unit */

    spUINT32 GAP;         /**< GAP Actualisation Factor. */

    spBYTE HSA;           /**< Highest Station Address. in [1..127] */

    spBYTE Max_Retry_Limit;/**< Max Retry Limit. */
} PL_Driver_Initialisation_Struct;

   
/**
 * @name
 * <b>Structures for SPL_Create_P2P/MCast.</b>
 ***************************************************************************/
/*@{*/


/**
 * Structure with protocol parameters for P2P connections to be filled
 *  by the application, if non standard values are to be used.
 ******************************************************************************/
typedef struct
{
    spINT32  SenderDynamicTransferTime;  /**< sender dynamic transfer time
                     as described in subset 056, 4.4.10.2, in ms, default = 50 */

    spINT32  SenderStaticTransferTime;  /**< sender static transfer time
                     as described in subset 056, 4.4.10.1, in ms, default = 0 */

    spINT32  DynamicBusTransferTime; /**< bus dynamic transfer time
                   as described in subset 056, 4.4.10.5, in ms
                   default = 50~ 2*(20 (token rotation time) +  2* 2.5 (max tlg
                   transmission time)) */

    spINT32  StaticBusTransferTime;  /**< bus static transfer time
                   as described in subset 056, 4.4.10.4, in ms, default = 0 */

    spUINT32 ConnectionSetupTimeLimit;  /**< connection setup time limit
                   (time to wait for Run/ReadyToRun tlg) as described in
                   subset 056, 9.12, in ms, default = 5000 */

    spUINT32 ConnectConfirmTimeout;  /**< Acknowledgement timeout period
                   (time to wait for Connect/Authenticate Confirm tlg. a.k.a
                   break timeout) as described in subset 057, 7.2, in ms,
                   default = 5000 */

    spUINT32 SecondIncorrectReceiveTimeout;  /**< Minimum time allowed for
                     second incorrect receive as described in subset 056,
                     5.2.5.8.4, in sec, default =  24*60*60 ~ 24 hours */
} SPL_P2P_Protocol_Parameters;


/**
 * Structure with protocol parameters for multicast connections
 *  to be filled by the application, if non standard values are to be used.
 *****************************************************************************/
typedef struct
{
    spINT32  SenderDynamicTransferTime;  /**< Sender dynamic transfer time
                     as described in subset 056, 4.4.10.2, in ms, default = 50 */

    spINT32  SenderStaticTransferTime;  /**< Sender static transfer time
                     as described in subset 056, 4.4.10.1, in ms, default = 0 */

    spINT32  DynamicBusTransferTime; /**< Bus dynamic transfer time
                   as described in subset 056, 4.4.10.5, in ms,
                   default = 50~2* (20 (token rotation time) +  2* 2.5 (max tlg
                   transmission time)) */

    spINT32  StaticBusTransferTime;  /**< Bus static transfer time
                   as described in subset 056, 4.4.10.4, in ms, default = 0 */

    spUINT32 SafeTimeLayerStartupInterval;  /**< SafeTimeLayerStartupInterva
                     as described in subset 056, 6.6.1.3, in ms, recommended value
                     (subset 059) =  50 - 300 , default = 100 */

    spUINT32 SafeTimeLayerRestartInterval;  /**< SafeTimeLayerRestartInterval
                     as described in subset 056, 6.6.1.4, in ms, recommended value
                     (subset 059) =  300 - 1000 ms, default = 1000 */

    spUINT32 StartupSynchronisationTimeLimit;  /**< StartupSynchronisationTimeLimit
                     as described in subset 056, 6.6.1.4, in sec,
                     recommended value (subset 059) =  30s, default = 30 */

} SPL_MCast_Protocol_Parameters;


/*@}*/

/***************************************************************************************/

/** @defgroup SPL_CONNECTION_PARAMETERS Parameters of SPL Connection. 
 *  SPL API Connection Multicast and peer2peer parameter structures and types 
 ***************************************************************************/
/*@{*/


typedef SPL_P2P_Protocol_Parameters   SPLA_P2PProtocolParameters;
typedef SPL_MCast_Protocol_Parameters SPLA_MCastProtocolParameters;
/*@}*/

/**
 * Structure with protocol parameters for P2P connections in B format
 *  for read back by SPL_Get_P2PDataB.
 ******************************************************************************/
typedef struct
{
    unsigned int SecondIncorrectReceiveTimeout;
    unsigned int ConnectConfirmTimeout;
    unsigned int ConnectionSetupTimeLimit;
    int StaticBusTransferTime;
    int DynamicBusTransferTime;
    int SenderStaticTransferTime;
    int SenderDynamicTransferTime;
} SPL_P2P_Protocol_ParametersB;

/**
 * Structure with protocol parameters for Multicast connections in B format
 *  for read back by SPL_Get_MCastDataB.
 ******************************************************************************/
typedef struct
{
    unsigned int StartupSynchronisationTimeLimit;   
    unsigned int SafeTimeLayerRestartInterval;
    unsigned int SafeTimeLayerStartupInterval;
    int StaticBusTransferTime;
    int DynamicBusTransferTime;
    int SenderStaticTransferTime;
    int SenderDynamicTransferTime;
} SPL_MCast_Protocol_ParametersB;

#endif /* SPLA_PROTOCOL_PARAMETERS_H_ */


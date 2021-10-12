/** @file
 * Type declaration for SPL UDP driver.
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by:    jkrupp %
 *                      %version:       10 %
 *                      %date_created:  2014-02-05 17:43 %
 **********************************************************************
 *
 *  Revision    Date        Name        Description
 *  10          2014/02/05  jkrupp      add define MAX_MEM_SIZE with calculation of the maximal memory used
 *                                      reduce max configuration parameters to reduce max memory size
 *  9           2013/09/12  jkrupp      add FFFIS_Common.h
 *                                      remove fffis_trace.h, fffis_string.h and fffis_clock.h because 
 *                                          FFFIS_Common.h is added and they are already inside this file
 *  8           2013/06/26  jkrupp      change TIME to spTIME because of change in FFFIS_Types.h
 *  7           2013/04/25  jkrupp      merge Revision 5.1.1 with Revision 6
 *  5.1.1       2013/02/27  ageck       enable second instance and clock telegram bypass for module test
 *  6           2013/02/25  jkrupp      add struct DatagramCorrupt_Type for Subsystem test
 *  5           2013/01/28  jkrupp      add new variable to Statistic_Type
 *  4           2013/01/16  jkrupp      add RecvStat_Type struct
 *                                      add recvStat and summary in Statistic_Type struct
 *                                      add defines for configuration parameter borders
 *  3           2012/11/21  jkrupp      move MAX_COLLECT_DELAY to be available for all versions with
 *                                      the same value.
 *                                      Change MAX_COLLECT_DELAY from 200 to 1000
 *                                      add lastErrorNo to Recv_Type struct
 *  2           2012/09/24  jkrupp      remove unused Connection_Type struct
 *  1           2012/09/07  ageck       reconstructed form previous version not checked in
 *
 **********************************************************************/
#ifndef PL_DRIVER_UDP_TYPES_H_
#define PL_DRIVER_UDP_TYPES_H_

/**********************************************************************
 * Include files
 **********************************************************************/
/*#include <tbsw_wrapper.h>*/

#include <fffis_trace.h>
#include <fffis_string.h>
#include <fffis_clock.h>
#include "FFFIS_Common.h"

/**********************************************************************
 * Defines
 **********************************************************************/
#define MAX_TELEGRAM_LENGTH             251
#define MIN_TELEGRAM_LENGTH             9
#define SPL_HEADER                      7

#define MAX_SAP                         63          /* Max allowed SAP addresses */
#define MAX_COLLECT_DELAY               1000UL      /* Time after a telegram is outdated in ms */

#define STAT_TIME_FACTOR                1000UL      /* statisticTime in seconds */

/* Configuration parameter borders*/
#define MIN_RESEND_COUNT                0
#define MIN_SEND_BUFFER_SIZE            0
#define MIN_DATAGRAM_SIZE               500
#define MIN_MAX_RECV_GAP                2

#define MAX_RESEND_COUNT                10UL
#define MAX_RESEND_DELAY                100
#define MAX_SEND_BUFFER_SIZE            25
#define MAX_DATAGRAM_SIZE               1500
#define MAX_MAX_RECV_GAP                20

#ifndef SPL_BS_CSS
 /** queuing outgoing clock telegrams for synchronization purpose of SPL instance
 *  not directly connected to Profibus.
 *  In order to increase the performance this bypass solution is intended to use on VCU only
 *  instead of bypass via socket and local loop.
 */
    #define SPL_INCLUDE_CLOCK_TELEGRAM_BYPASS       /* enable clock telegram internal bypass */
    #define SPL_ETHERNET_SEMAPHORE_TIMEOUT  20      /* semaphore timeout to get access to clock telegram queue in ms */
    #define SPL_ETHERNET_CLOCK_QUEUE_SIZE   10      /* Clock telegram queue size */
#endif

/**
 *  In the Global_Driver_Status and Local_SAP_Status we manipulate bits
 *  (setting and resetting). Let's declare two macros, they will help
 *  to fix compiler or Lint warnings at a single place.
 *
 **********************************************************************/
#define SET_BIT(the_variable,the_bit)   ((the_variable) |= (the_bit))


/**********************************************************************
 * Structures
 **********************************************************************/
#if defined(CORRUPTOR) && !defined(SPL_ETH_DRIVER_SECOND_INSTANCE)
typedef struct
{
    spUINT32    length;
    spBYTE      seqNo;
}DatagramCorrupt_Type;
#endif

typedef struct
{
    spBYTE      seqNo;
    spBYTE      *bytes;
}Datagram_Type;

typedef struct
{
    spUINT16    currentPos;
    spUINT16    resentCount;
    spUINT32    sendTime;
    Datagram_Type data;
}DatagramSendBuffer_Type;

typedef struct
{
    spUINT16    port;
    spUINT32    addr;
} Addr_Type;

typedef struct
{
    spBYTE      seqNo;
    spUINT32    count;
} RecvStat_Type;

typedef struct
{
    spUINT32    recvCounter;
    spUINT32    oldRecvCounter;
    spUINT32    acceptedCounter;
    spTIME      statisticLastTime;
    spUINT32    lastidx;
    spUINT32    initCount;
    RecvStat_Type   *recvStat;
    spUINT32    *summary;
} Statistic_Type;

typedef struct
{
    spUINT16    maxRecvGap;
    spUINT16    sendBufferSize;
    spUINT16    resendCount;
    spUINT16    resendDelay;
    spUINT16    dgramSize;
    spUINT16    statisticTime;
    Addr_Type   local;
    Addr_Type   remote;
}Config_Type;

typedef struct{
    spBYTE      seqNo;
    spINT32     socket;
    spUINT16    currentDatagram;
    spUINT16    oldestDatagram;
    spBOOL      hostDown;
    DatagramSendBuffer_Type *buffer;
}Send_Type;

typedef struct{
    spINT32     socket;
    spINT32     lastErrorNo;
    spUINT16    currentPos;
    spUINT16    dataLength;
    spBOOL      seqNoValid;
    spTIME      lastCall;
    Statistic_Type  statistic;
    Datagram_Type   data;
}Recv_Type;

typedef struct
{
    Config_Type config;
    Send_Type   send;
    Recv_Type   recv;
}UdpDriver_Type;

#if (defined(SPL_PLATFORM_VCU) || defined (MOD_TEST)) && defined(SPL_INCLUDE_CLOCK_TELEGRAM_BYPASS)
    /* clock telegram bypass */
    typedef enum
    {
        ETH_BUFFER_FREE   = 0x00,  /**< buffer is free */
        ETH_BUFFER_SET    = 0x10   /**< buffer contains data */
    } ETH_Buf_Status;
#endif

/**
 *  Definition of the structure that holds information about the Clock telegrams
 *
 **********************************************************************/
#if (defined(SPL_PLATFORM_VCU) || defined (MOD_TEST)) && defined(SPL_INCLUDE_CLOCK_TELEGRAM_BYPASS)
    typedef struct
    {
        spINT clock_sem_id;     /**< semaphore ID of the clock */
        spINT First_Elem;       /**< First element of the clock information */
        spINT Next_Elem;        /**< Next element of the clock information */
        ETH_Buf_Status  Status[SPL_ETHERNET_CLOCK_QUEUE_SIZE];              /**< Status of the buffer */
        SLL_Telegram_Struct Clock_Telegram[SPL_ETHERNET_CLOCK_QUEUE_SIZE];  /**< clock telegram data packed in SLL structure */
    } Clock_Struct;
#endif

/**
 *  Definition of the structure that holds information for each enabled SAP.
 *  For each possible SAP, we need some information:`
 *      The status.
 *      Some configuration information
 *      A buffer for double reception handling.
 *
 *  We cannot allocate this information for the 63 possible SAPs.
 *  it would use too much memory.
 *
 *  Let's allocate (statically) only SPL_MAX_CONNECTIONS information
 *  structures and let's use an indirection array that
 *  shall indicate for each possible SAP wich information
 *  buffer is used.
 *
 *  Of course, a never enabled SAP shall not point to
 *  any information buffer.
 **********************************************************************/
typedef struct
{
    spDWORD Local_SAP_Status;               /**< returned by Get_SAP_Status public function */
    spBOOL  Allow_Input;                    /**< User settings */
    spBOOL  Allow_Output;                   /**< User settings */
    spBOOL  Priority;                       /**< Priority of messages sent from SAP
                                                 i.e. spTRUE=HIGH, spFALSE=LOW      */
    spBYTE    FDL_Mode;                     /**< User settings */
} Local_SAP_Info_Struct;

/**********************************************************************
 * Function definitions
 **********************************************************************/
#if (defined(SPL_PLATFORM_VCU) || defined (MOD_TEST)) && defined(SPL_INCLUDE_CLOCK_TELEGRAM_BYPASS)
    extern void UDP_Driver_SetTimeData(const SLL_Telegram_Struct * pTelegram);
#endif

/**********************************************************************
 * Defines with calculation
 **********************************************************************/
#define MAX_MEM_SIZE    ( ( (spUINT32)(2 * MAX_MAX_RECV_GAP * (spINT)sizeof(RecvStat_Type)) \
                        + (MAX_RESEND_COUNT * (spUINT32)sizeof(spUINT32)) \
                        + (spUINT32)(MAX_SEND_BUFFER_SIZE * (spINT)sizeof(DatagramSendBuffer_Type)) \
                        + (spUINT32)(MAX_SEND_BUFFER_SIZE * MAX_DATAGRAM_SIZE) \
                        + (spUINT32)MAX_DATAGRAM_SIZE ) / (spUINT32)sizeof(spUINT32) )

#endif /* PL_DRIVER_UDP_TYPES_H_ */

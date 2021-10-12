/**
 * @file Header file of Local Time Functions
 * 
 **********************************************************************/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by:    jkrupp %
 *                      %version:       13 %
 *                      %date_created:  2014-01-08 16:45 %
 **********************************************************************
 *
 *  Revision    Date        Name        Description
 *  13          2014/01/08  ageck/jkrupp    moved FFFIS_Clock_Tick_Rate prototype to fffis_clock.c
 *  12          2013/06/26  jkrupp      change TIME to spTIME because of change in FFFIS_Types.h
 *
 *  undocumented changes after switch from CVS to Synergy
 *
 *  $Log: fffis_clock.h,v $
 *  Revision 1.8  2005/07/19 09:00:06  wroewe
 *  add STL_CLOCK_SYNC
 *
 *  Revision 1.7  2005/06/24 12:14:38  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.6  2005/05/20 14:22:44  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.5  2005/05/12 08:17:12  wroewe
 *  add STL_SEND_MCAST_STARTUP
 *
 *  Revision 1.4  2005/03/09 12:51:57  wroewe
 *  add identifer for time check operation
 *
 *  Revision 1.3  2004/09/17 13:44:04  mjoost
 *  added some ifdef's VXWORKS in order to run the code on VCU-Lite
 *
 *  Revision 1.2  2004/06/23 14:11:10  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.1  2004/06/23 13:05:10  jdiezper
 *  Local Time Function
 *
 **********************************************************************/
#ifndef FFFIS_CLOCK_H_
#define FFFIS_CLOCK_H_

#ifdef SPL_PLATFORM_LZB

 #ifdef CHIPVIEW
 
  spTIME FFFIS_Clock (void);
 
 #else /* CHIPVIEW */

  #include "../include/typmisra.h"
  #include "../include/sysClock.h"

  #define FFFIS_Clock()  SystemClockLZBSTMNRV.l
  
  extern register timeunion SystemClockLZBSTMNRV;
 #endif /* CHIPVIEW */
 
 #else /* SPL_PLATFORM_LZB */

/**
 * The function returns the value of the clock last set via FFFIS_Set_Clock().
 * This changed functionality is due to A and B code requiring the same
 * time when calling FFFIS_Clock.  To obtain the local time use 
 * FFFIS_Get_Local_Time.
 * The function still provides the current relative time.  
 * Precision is 1ms.
 * 
 *       @return spTIME     The value last set by FFFIS_Set_Clock
 * 
 * @note
 * This is a local clock, NOT the FFFIS Reference time.
 *********************************************************************/

spTIME FFFIS_Clock (void);



/**
 * This function is used to update/increment the the value to be 
 * returned by FFFIS_Clock. 
 * Provides the current relative time.Time starts at 0 when application starts.
 * Precision is 1ms.
 * 
 *       @param Now     To set the value of The number of ms elapsed 
 *                      since application start.  Should be set to
 *                      FFFIS_Get_Local_Time().
 * 
 * @note
 * This is a local clock, NOT the FFFIS Reference time.
 *********************************************************************/
void FFFIS_Set_Clock (spTIME Now);


/**
 * Provides the current relative time.Time starts at 0 when application starts.
 * Precision is 1ms.
 * 
 *       @return spTIME     The number of ms elapsed since application start.
 * 
 * @note
 * This is a local clock, NOT the FFFIS Reference time.
 *********************************************************************/
spTIME FFFIS_Get_Local_Time (void);


/**
 * Returns the current system time in seconds since 01.01.1970 (TBSW, PC),
 * or time in seconds from system start (VxWorks with TBSW Sim)
 *
 *      @return spTIME      System Time in seconds
 *
 *
 * @note
 * Function is used only during Random Generator init
 *********************************************************************/
spTIME FFFIS_Get_Date (void);



#endif /* SPL_PLATFORM_LZB */



/**
 * @name
 * <b>Control of send Idle Telegrams, receive Idle Telegrams, channel is connected
 * (break).</b>
 **********************************************************************/
 /*@{*/

/**
 * Only for SLL. */
#define MAX_SLL_TIME_CHECK           3u
/**
 * Only for SLL. */
#define SLL_SEND_IDLE                0u
/**
 * Only for SLL. */
#define SLL_RECV_IDLE                1u
/**
 * Only for SLL. */
#define SLL_BREAK_TIME               2u


/**
 * Only for STL. */
#define MAX_STL_TIME_CHECK           2u
/**
 * Only for STL. */
#define STL_CONNECTION_SETUP_TIME    0u
/**
 * Only for STL. */
#define STL_SEND_MCAST_STARTUP       1u 


/**
 * Only for Clock 
 * the time value is a part of the clock structure (not of the connection structure). */
#define STL_CLOCK_SYNC               MAX_STL_TIME_CHECK


/**
 * Need for checking send and receive Idle telegrams and break condition */
typedef struct
{
    spBOOL Valid;
    spTIME   Time;
} Time_Check_Struct;
/*@}*/

/**
 * Definitions where the time compare takes place.
 **********************************************************************/
typedef enum
{
  SLL_Timers, /**< SLL. */
  STL_Timers  /**< STL. */
} Timer_Location_Enum;

/**
 *     Timer Identifier are used to trigger events on time outs.
 **********************************************************************/
typedef spUINT FFFIS_Timer_Id_Type;

#endif /* #define FFFIS_CLOCK_H_ */

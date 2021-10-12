/** 
 * @file Debugging functionality for TRACE output.  Functionality 
 * include multiple swithable trace channels.
 *
 */
/*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: alamb %
 *                      %version: 9 %
 *                      %date_created: Wed May 03 16:07:22 2006 %
 *
 **********************cvs out of date********************************* 
 *                      $Author: jkusmira $
 *                      $Revision: 1.27 $
 *                      $Date: 2005/06/27 08:14:00 $
 *                      $Source: P://mpg/sl/com/fffis_trace.h,v $
 *
 *  $Log: fffis_trace.h,v $
 *  Revision 1.27  2005/06/27 08:14:00  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.26  2005/05/20 14:22:46  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.25  2005/02/25 10:44:56  mjoost
 *  add the debug channel in order to display implicit data
 *
 *  Revision 1.24  2005/02/11 10:09:04  mjoost
 *  add channel CH_PL_DRIVER_SOFTING_DEBUG
 *
 *  Revision 1.23  2005/01/17 10:44:55  jdiezper
 *  Add the STL_CLOCK channels
 *
 *  Revision 1.22  2005/01/10 19:41:32  alamb
 *  Additions for hilscher driver
 *
 *  Revision 1.21  2004/12/20 10:29:27  mjoost
 *  all disabled trace functions defined as ((void)0)
 *
 *  Revision 1.20  2004/11/26 10:53:14  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 *  Revision 1.19  2004/10/26 07:45:38  mjoost
 *  Now I did what was promised in the last log statement
 *
 *  Revision 1.18  2004/10/26 07:07:06  mjoost
 *  redirect Trace to Trace_Function
 *
 *  Revision 1.17  2004/10/13 08:25:22  mczaprag
 *  add TEST_SIM channel
 *
 *  Revision 1.16  2004/10/07 11:08:10  jdiezper
 *  House keeping
 *
 *  Revision 1.15  2004/09/21 14:42:42  mjoost
 *  added the fclose function (for VxWorks)
 *
 *  Revision 1.14  2004/08/05 09:44:45  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 *
 *  Revision 1.13  2004/07/29 11:38:27  jdiezper
 *  Add some new channels
 *
 *  Revision 1.12  2004/07/06 16:00:10  jdiezper
 *  Add the STL_API channel
 *
 *  Revision 1.11  2004/06/23 14:11:11  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.10  2004/06/18 09:15:10  jdiezper
 *  Add the IFAK channels
 *
 *  Revision 1.9  2004/06/08 14:20:20  mjoost
 *  IFAK test added
 *
 *  Revision 1.8  2004/06/03 13:25:02  jdiezper
 *  Add the Set_Output_Console
 *
 *  Revision 1.7  2004/04/16 11:37:57  jdiezper
 *  Cleaning the SIM/PARSER traces
 *
 *  Revision 1.6  2004/04/15 13:40:36  jdiezper
 *  Add the intercative channel setup
 *
 *  Revision 1.5  2004/04/14 10:19:02  jdiezper
 *  Add ERROR channel
 *
 *  Revision 1.4  2004/04/06 15:23:46  mjoost
 *  Add the CH_CRC channel
 *
 *  Revision 1.3  2004/04/05 12:00:12  jdiezper
 *  Add a new channel
 *
 *  Revision 1.2  2004/03/30 08:55:09  jdiezper
 *  Some minor changes in the TRACE_ENTER functions.
 *
 *  Revision 1.1  2004/03/04 09:01:35  jdiezper
 *  Traces and standard library replacement
 *
 *
 **********************************************************************/
#ifndef FFFIS_Trace_H_
#define FFFIS_Trace_H_

/**
 * @name
 * <b>Definitions of the Trace Channels (first parameter of the
 * TRACE_Function).</b>
 *
 * @note Please ensure this list agrees with the corresponding list in 
 * fffis_trace.c
 *********************************************************************/
/*@{*/
/**
 * Error channel.*/
#define CH_ERROR                     0U
/**
 * Standard channel.*/
#define CH_STD                       1U
/**
 * Function Enter channel.*/
#define CH_FCT_ENTER                 2U
/**
 * Function Exit channel.*/
#define CH_FCT_EXIT                  3U
/**
 * SLL API channel.*/
#define CH_SLL_API                   4U
/**
 * STL API channel.*/
#define CH_STL_API                   5U
/**
 * CRC channel.*/
#define CH_CRC                       6U
/**
 * Private Test channel.*/
#define CH_TEST_SIM                  7U
#define CH_SIM_RUNTIME               8U
/**
 * Simulator Runtime Debug channel.*/
#define CH_SIM_RUNTIME_DEBUG         9U
/**
 * Simulator Runtime Error channel.*/
#define CH_SIM_RUNTIME_ERROR        10U
/**
 * Simulator Parser channel.*/
#define CH_SIM_PARSER               11U
/**
 * Simulator Parser Debug channel.*/
#define CH_SIM_PARSER_DEBUG         12U
/**
 * Simulator Parser Error channel.*/
#define CH_SIM_PARSER_ERROR         13U
/**
 * Memory Driver channel.*/
#define CH_PL_DRIVER_MEM            14U
/**
 * IFAK Driver channel.*/
#define CH_PL_DRIVER_IFAK           15U
/**
 * Ethernet Driver channel.*/
#define CH_PL_DRIVER_ETH            16U
/**
 * PL Manager channel.*/
#define CH_PL_MAN                   17U
/**
 * Softing Driver channel.*/
#define CH_PL_DRIVER_SOFTING        18U
/**
 * Softing Driver Debug channel.*/
#define CH_PL_DRIVER_SOFTING_DEBUG  19U
/**
 * Hilscher Driver Debug channel.*/
#define CH_PL_DRIVER_HIL_DEBUG      20U
/**
 * Hilscher Driver Error channel.*/
#define CH_PL_DRIVER_HIL_ERROR      21U
/**
 * STL Clock channel.*/
#define CH_STL_CLOCK                22U
/**
 * STL Clock Debug channel.*/
#define CH_STL_CLOCK_DEBUG          23U
/**
 * Debug channel.*/
#define CH_DEBUG                    24U
/**
 * Vipco Driver Error channel.*/
#define CH_PL_DRIVER_VIPCO_ERROR    25U
/**
 * Vipco Driver Debug channel.*/
#define CH_PL_DRIVER_VIPCO_DEBUG    26U
/* @note debug channels for diversified implementation of SPL */
#define CH_TOKEN                    27U
#define CH_SPLA                     28U
#define CH_SPLA_FNCALL              29U
#define CH_SPLA_FNCALL_CYCLIC       30U
#define CH_SPLA_STD                 31U
#define CH_SPLA_DEBUG               32U
#define CH_SPLB_FNCALL              33U
#define CH_SPLB_FNCALL_CYCLIC       34U
#define CH_SPL_COM                  35U
#define CH_SPL_COM_FNCALL           36U
#define CH_SPL_COM_FNCALL_CYCLIC    37U
#define CH_SPL_COM_STD              38U
#define CH_SPL_COM_DEBUG            39U
#define CH_SPL_API                  40U
#define CH_SPL_API_FNCALL           41U
#define CH_SPL_API_FNCALL_CYCLIC    42U
#define CH_SPL_API_STD              43U
#define CH_SPL_API_DEBUG            44U
/*@}*/



#ifdef SPL_OPTION_ENABLE_TRACES
    /**
     * @name
     * <b>T R A C E S     E N A B L E D.</b>
     *********************************************************************/
     /*@{*/
    
    /**
     * Enable or Disable one particular channel.
     * 
     *   @param    Channel        Variable description.
     * 
     *   @param    Enable         Variable description.
     * 
     *********************************************************************/
    void TRACE_Channel_Setup (const spUINT Channel, const spBOOL Enable);

    /**
     * Get a base path length.
     *   
     * @return  (uint) length ot the base path [0..TRACE_MAX_BASE_PATH_LENGTH]
     * 
     *********************************************************************/
    const spCHAR * TRACE_Get_Indent_String ( spINT Count ); 
        
    /**
     * Works as the standard printf C function.
     * 
     *   @param    Channel        Variable description.
     * 
     *   @param    pFormat        Variable description.
     * 
     *********************************************************************/
    /*lint --e(960) variable no. arguments - not used in production version */
    void TRACE_Function (const spUINT Channel, const spCHAR * pFormat, ...);

    /**
     * @todo Define description.
     *********************************************************************/
    #define TRACE(all_parameters) TRACE_Function all_parameters
    /**
     * Used to log functions entering.
     * For debugging purposes only.
     *********************************************************************/
    #define TRACE_ENTER(pFunctionName) TRACE_Function (CH_FCT_ENTER, "%s-> %s (%s:%d)\n",\
               TRACE_Get_Indent_String(0), pFunctionName, __FILE__, __LINE__)
    /**
     * Used to log functions exiting.
     * For debugging purposes only.
     *********************************************************************/
    #define TRACE_EXIT()               TRACE_Function (CH_FCT_ENTER, "%s<- (%s:%d)\n",\
                    TRACE_Get_Indent_String(0), __FILE__, __LINE__)
  /**
     * Used to log functions entering of SPL A code.
     * For debugging purposes only.
     *********************************************************************/
   #define TRACE_ENTER_CHANNEL(iChannel, pFunctionName) \
                           TRACE_Function (iChannel, "%s-> %s (%s:%d)\n",\
                                           TRACE_Get_Indent_String(1), \
                                           pFunctionName,  __FILE__, __LINE__)
    /**
     * Used to log functions exiting for SPL A code.
     * For debugging purposes only.
     *********************************************************************/   
    #define TRACE_EXIT_CHANNEL(iChannel, FunctionResult) \
                           TRACE_Function (iChannel, "%s<-[%d] (%s:%d)\n",\
                                           TRACE_Get_Indent_String(-1), FunctionResult, \
                                           __FILE__, __LINE__)
    /* /define */
    /*@}*/
    
#endif /* SPL_OPTION_ENABLE_TRACES*/


#ifdef SPL_OPTION_DISABLE_TRACES
    /**
     * @name
     * <b>T R A C E S     D I S A B L E D.</b>
     * Let's define all the functions as emty macros.
     *********************************************************************/
     /*@{*/

    #define TRACE(all_parameters) ((void)0)

    #define TRACE_Channel_Setup(_unused_parameter_,_another_unused_parameter_) ((void)0)

    #define TRACE_ENTER(_unused_parameter_)      ((void)0)
    #define TRACE_EXIT()                         ((void)0)
    #define TRACE_ENTER_CHANNEL(_unused_parameter_,_another_unused_parameter_)     ((void)0)   
    #define TRACE_EXIT_CHANNEL(_unused_parameter_,_another_unused_parameter_) ((void)0) 
 
    /*@}*/

#endif /* SPL_OPTION_DISABLE_TRACES*/

#endif /* FFFIS_Trace_H_ */



































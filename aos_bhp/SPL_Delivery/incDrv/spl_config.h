/**********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: jkrupp %
 *                      %version: 13 %
 *                      %date_created: 2012-09-27 09:53 %
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  Revision 13  2012/09/27      jkrupp
 *		remove TSG part in define names
 *
 *  Revision 12  2012/09/14      ageck
 *		removed softing, added PC_UDP, smaller corrections
 *
 *  Revision 11  2012/08/27      jkrupp
 *		add VCU_UDP
 *		rename VCU_ETHERNET to VCU_TCP
 *		rename VCU_TSG_SWITCH_DRIVER to VCU_TSG_SWITCH_DRIVER_TCP
 *		add VCU_TSG_SWITCH_DRIVER_UDP
 *
 *  Revision 10  2012/06/13      jkrupp
 *		add VCU_TSG_SWITCH_DRIVER
 *
 *  Undocumented changes after change from CVS to Synergy
 *
 *  $Log: spl_config.h.base,v $
 *  Revision 1.3  2004/11/26 15:10:18  mjoost
 *   there was a very small bug, juan fixed it
 *
 *  Revision 1.2  2004/11/26 12:07:26  wroewe
 *  Added a comment anout the TRACE OUTPUT FUNCTION
 *
 *  Revision 1.1  2004/11/26 10:57:08  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 **********************************************************************/
#ifndef SPL_CONFIG_H_
#define SPL_CONFIG_H_


/**@name Operating system / platform
 *    ------------------------------
 *
 *    Select one, and only one platform.
 **********************************************************************/
 /**@{*/
#if defined (PC_UDP) || defined(PC_HILSCHER) || defined(PC_MEMORY) || defined(PC_ETHERNET)
	#define SPL_PLATFORM_PC_GNU
#endif

#if defined(VCU_SOFTING)||defined(VCU_TCP)||defined(VCU_UDP)|| defined(VCU_MEMORY)|| defined(VCU_VIPCO) || defined (VCU_SWITCH_TCP) || defined (VCU_SWITCH_UDP)
	#define SPL_PLATFORM_VCU
#endif

#if defined(PPC_ETHERNET)||defined(PPC_HILSCHER)||defined(PPC_MEMORY)
	#define SPL_PLATFORM_PPC
#endif

#if defined(_C196_)
	#define SPL_PLATFORM_LZB
    	#define SPL_ACODE_ONLY
    	#define SPL_OPTION_SIL_4
#endif


#if defined(ARM_ATPCU)||defined(ARM_MEMORY)||defined(ARM_ETHERNET)
	#define SPL_PLATFORM_ARM
#endif

#if defined(MPX_ATPCU)||defined(MPX_MEMORY)||defined(MPX_ETHERNET)
	#define SPL_PLATFORM_PPC_LINUX
#endif

#if defined(X86_ATPCU)||defined(X86_MEMORY)||defined(X86_ETHERNET)
	#define SPL_PLATFORM_PC_LINUX
#endif
/**@}*/



/**@name Endianness
 *    -------------
 *
 *   One, and *only* one, of the two following defines must be defined:
 *      SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN for Motorola like processors.
 *      SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN for Intel like processors.
 *
 *   This define is used in FFFIS_Endianness.h to define
 *      macros to convert 16 bits or 23 bits integers to/from
 *      network order (big endian) to local order (big or little endian)
 **********************************************************************/
 /**@{*/
#if    defined(VCU_MEMORY)	\
    || defined(VCU_VIPCO)	\
    || defined(VCU_TCP)     \
    || defined(VCU_UDP)     \
    || defined(VCU_SWITCH_TCP)\
    || defined(VCU_SWITCH_UDP)\
    || defined(PPC_ETHERNET)\
    || defined(PPC_HILSCHER)\
    || defined(PPC_MEMORY)  \
    || defined(MPX_ATPCU)   \
    || defined(MPX_MEMORY)  \
    || defined(MPX_ETHERNET)

	#define SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN
#endif

#if    defined(PC_HILSCHER) \
	|| defined(PC_MEMORY)   \
	|| defined(PC_ETHERNET) \
	|| defined(PC_UDP) \
	|| defined(_C196_)      \
    || defined(ARM_ATPCU)   \
    || defined(ARM_MEMORY)  \
    || defined(ARM_ETHERNET)\
	|| defined(X86_ATPCU)   \
    || defined(X86_MEMORY)  \
    || defined(X86_ETHERNET)

    #define SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN

#endif
/**@}*/

/**@name Debugging Traces
 *    -------------------
*
 * *  TRACE macros populate all modules in the SPL code.
 *    They behave as a standard printf 'c' function.
*
 * *  If SPL_OPTION_DISABLE_TRACES is defined, the TRACE macro
 *      becomes an empty macro, ensuring that no trace shall be
 *      produced and that the TRACE's arguments shall not be
 *      evaluated.
*
 * *  If  SPL_OPTION_ENABLE_TRACES is defined, the TRACE macro
 *       shall evaluate the parameters, build a human
 *       readable string and output the string (console,
 *       serial line...) through the usage of
 *       the user provided macro:
 * 	 SPL_TRACE_OUTPUT_FUNCTION
*
 * *   For better performance, disable the traces.
*
 * *   For debugging or testing, enable the traces
 *       and define SPL_TRACE_OUTPUT_FUNCTION as Simulator_Trace_Function:
 * 	#define SPL_TRACE_OUTPUT_FUNCTION(_text_) Simulator_Trace_Function (_text_)
 *       or define it to your own output function
 * 	#define SPL_TRACE_OUTPUT_FUNCTION(_text_) Send_To_Rs232(_text_)
 *       In both cases, you must supüply the prototype of the function.
*
***********************************************************************/

/**@{*/
#if defined(DEBUG)

    #define SPL_OPTION_ENABLE_TRACES

    void Simulator_Trace_Function (char * Text);

    #define SPL_TRACE_OUTPUT_FUNCTION(_text_) Simulator_Trace_Function (_text_)

#else

    #define SPL_OPTION_DISABLE_TRACES

#endif
/**@}*/

/**@name  4. Debugging Assertions
 *    -----------------------
*
 * *  Assertions are always turned on.
 *    When one Assertion fails, the application is halted.
*
***********************************************************************/


/**
*********************************************************************
 *    5. Dump of internal telegrams and events
 *    ----------------------------------------
*
 * *  In the STL_Api and SLL_Api each time a telegram or event is
 *      received or sent, the SLL_Dump_Telegram or SLL_Dump_Event
 *      functions are called.
*
 * *  These two functions shall produce a lot of TRACES.
*
 * *  In order to improve speed and remove useless code,
 *      it is recommended to disable traces and not to
 *      include the Dump_Functions because they have
 *      a significant CPU overhead and theyr code size
 *      is quite important.
 **********************************************************************/

/**@{*/
#if defined(DEBUG)

#define SPL_INCLUDE_DUMP_FUNCTIONS

#else

#define SPL_EXCLUDE_DUMP_FUNCTIONS

#endif
/**@}*/

/**@name Maximum Number of simultaneous SPL connections.
 *    --------------------------------------------------
*
 * *  SPL Connection Objects (SPL_Connection structure) are statically
 *    allocated in the spl_api.c module.
*
 * *  When user creates such an object (SPL_Create_P2P or SPL_Create_MCast)
 *    he receives a pointer to the statically allocated objects in SPL_Api.c
*
 * *  SPL_MAX_CONECTIONS defines the maximum number of connections that an
 *    application is able to create.
*
 * *  Notes:
 *        1) The Spl_Api shall always create one private for the clock
 *           synchronization. The remaining connections
 *           (SPL_MAX_CONNECTIONS-1) are left fot the application.
*
 * *      2) This constant is also used in the Profibus Manager
 * 	  (pl_manager.c) to statically define the maximum
 * 	  number of profibus channels (SAPs) that can be open.
*
 * *      3) Mandatory minimum value should be 1
*
 * *      4) Recommended value is 10
 *
 **********************************************************************/
 /**@{*/
#ifdef SPL_PLATFORM_LZB
#define SPL_MAX_CONNECTIONS 16
#else
#define SPL_MAX_CONNECTIONS 64
#endif
/**@}*/



/**
*********************************************************************
 *    7. Number of messages stored (per SAP) on the Profibus Layer Manager
 *    --------------------------------------------------------------------
*
 * *  For each channel (SAP), the Profinus Layer Manager shall statically
 *    create a FIFO with a depth (maximum number of telegrams) equal to
 *    SPL_MAX_FIFO_DEPTH.
*
 * *  Each telegrams is 260 bytes long.
*
 * *  Notes:
 *        1) Assuming each Profibus Manager Channel (SAP) has a FIFO of
 *           SPL_MAX_FIFO_DEPTH telegrams, and assuming there are
 * 	  SPL_MAX_CONNECTIONS channels we can compute:
*
 * *  SPL_MAX_CONNECTIONS*SPL_MAX_FIFO_DEPTH*260 is the amount
 * 	  of statically allocated memory for storing telegrams.
*
 * *      2) Mandatory minimum value should be 1
*
 * *      3) Recommended value is 10
 **********************************************************************/
#define SPL_MAX_FIFO_DEPTH   20


/**
*********************************************************************
 *    8. Idle messages sent by application or the library
 *    --------------------------------------------------------------------
 *
 * *  This option determines whether the application or the library should
 *    determine when to send idle messages.  According to this option,
 *    SPL_checkIdle() or SPL_sendIdle() is used.
 *
 * *  Notes:
 *        1) This is the default option for a released, however most
 *           development is easier with the option not set.
 *
 **********************************************************************/
#ifdef SPL_BS_TBSW

/** if SPL_OPTION_IDLE_BYAPP is defined the application is responsible itself for
 *  idle monitoring. If this option is set, the application is responsible for sending out idle
 *  telegrams with a proper timing. This is done by calls to SPL_sendIdle, which forces the SPL
 *  sending out idle telegrams unconditionally.
 *  If this option is not set the application must call SPL_checkIdle, which means checking the
 *  the state of the idle timers and sending out an idle telegram if necessary.
 */

#define SPL_OPTION_IDLE_BYAPP

#endif


/**
*********************************************************************
 *    9. Deactivation of the clock control
 *    ------------------------------------
 *
 * *  This switch deactivates the control of the incoming Sync and Reference
 *    Clock telegram like short term drift, adjustment factor calculation,
 *    increasing of local and reference times
 *
 * *  Notes:
 *         the control function is deactivated in the production
 *		   for A-Code simulation activate function now
 *
 **********************************************************************/
#if 0
#define SPL_OPTION_TEST_NO_SYNC_CHECK
#endif


/**
*********************************************************************
 *    10. Deactivation of the time stamp control
 *    ------------------------------------------
 *
 * *  This switch deactivates the control of the incoming telegram time
 *    stamps
 *
 * *  Notes:
 *         the control function is deactivated in the production
 *		   for A-Code simulation activate function now
 *
 **********************************************************************/
#if 0
#define SPL_OPTION_TEST_NO_AGE_CHECK
#endif


/**@name Low Level Profibus Drivers
 *    ------------------------------
*
 * *  Explicitely include (or exclude) the code of the available Profibus
 *        drivers.
*
 * *  Notes:
 *       1) Including a driver does not mean it shall be used.
 * 	 When using the simulator, the actual driver that is used
 * 	 depends on the instructions found ins the simulation file.
*
 * *     2) Each flag applies only to one source file (the corresponding
 *          module)
 **********************************************************************/
/**@{*/
/*ifak and softing aren't used longer*/
#define SPL_EXCLUDE_DRIVER_IFAK
#define SPL_EXCLUDE_DRIVER_SOFTING


#if defined(PC_ETHERNET) || defined(PPC_ETHERNET) || defined(VCU_TCP) || defined(ARM_ETHERNET) || defined(MPX_ETHERNET) || defined(X86_ETHERNET) || defined(VCU_SWITCH_TCP)
	#define SPL_INCLUDE_DRIVER_TCP
#else
	#define SPL_EXCLUDE_DRIVER_TCP
#endif

#if defined (PC_UDP) || defined(VCU_UDP) || defined(VCU_SWITCH_UDP)
	#define SPL_INCLUDE_DRIVER_UDP
#else
	#define SPL_EXCLUDE_DRIVER_UDP
#endif

#if defined(VCU_VIPCO) || defined(VCU_SWITCH_TCP) || defined(VCU_SWITCH_UDP)
	#define SPL_INCLUDE_DRIVER_VIPCO
#else
	#define SPL_EXCLUDE_DRIVER_VIPCO
#endif

#if defined(VCU_SWITCH_TCP)
    #define SPL_INCLUDE_DRIVER_SWITCH_TCP
#else
    #define SPL_EXCLUDE_DRIVER_SWITCH_TCP
#endif

#if defined(VCU_SWITCH_UDP)
    #define SPL_INCLUDE_DRIVER_SWITCH_UDP
#else
    #define SPL_EXCLUDE_DRIVER_SWITCH_UDP
#endif

#if defined(PC_HILSCHER) || defined (PPC_HILSCHER)
	#define SPL_INCLUDE_DRIVER_HILSCHER
#else
	#define SPL_EXCLUDE_DRIVER_HILSCHER
#endif

#if defined(PC_MEMORY) || defined(PPC_MEMORY) || defined(VCU_MEMORY) || defined(ARM_MEMORY) || defined(MPX_MEMORY) || defined(X86_MEMORY)
	#define SPL_INCLUDE_DRIVER_MEM
#else
	#define SPL_EXCLUDE_DRIVER_MEM
#endif

#if defined(ARM_ATPCU) || defined(MPX_ATPCU) || defined(X86_ATPCU)
	#define SPL_INCLUDE_DRIVER_ATPCU
#else
	#define SPL_EXCLUDE_DRIVER_ATPCU
#endif

/**@}*/


/**
*********************************************************************
 *    Cross Compare
 *
 * *  This switch makes functions available to get SPL data for cross
 *    comparison.
 *
 * *  Notes:
 *         This feature is only for the new GSP
 *
 **********************************************************************/
#if defined(SPL_PLATFORM_ARM) || defined(SPL_PLATFORM_PPC_LINUX) || defined(MOD_TEST)
#define SPL_CXCMP
#endif

#endif /*SPL_CONFIG_H_*/


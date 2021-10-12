/**********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: jkrupp %
 *                      %version: 15 %
 *                      %date_created: 2012-09-27 09:59 %
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  Revision 15  2012/09/27      jkrupp
 *		remove TSG part in define names
 *
 *  Revision 14  2012/09/18      ageck
 *		removed softing and ifak
 *
 *  Revision 13  2012/08/27      jkrupp
 *		rename for include and exclude TSG_DRIVER_SWITCH to DRIVER_TSG_SWITCH_TCP
 *		rename for include and exclude DRIVER_ETHERNET to DRIVER_TCP
 *		add for include and exclude DRIVER_UDP
 *		add for include and exclude DRIVER_TSG_SWITCH_UDP
 *
 *  Revision 12  2012/06/13     jkrupp
 *		add TSG_DRIVER_SWITCH
 *
 *  Undocumented changes after change from CVS to Synergy
 *
 *  $Log: spl_config_check.h,v $
 *  Revision 1.3  2005/06/23 06:00:20  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.2  2005/05/20 14:22:36  mkilianj
 *  converting to doxygen style comments
 *
 *  Revision 1.1  2004/11/26 10:57:10  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 **********************************************************************/
#ifndef SPL_CONFIG_CHECK_H_
#define SPL_CONFIG_CHECK_H_

/**
 * @name
 *  <b>Stub and diversification code options.</b>
 *
 *  A-Dummy is not allowed
 **********************************************************************/
/*@{*/
#if  defined(SPL_A_DUMMY)
     #error SPL_A_DUMMY is not supported
#endif

#if  defined(SPL_B_DUMMY) && defined(SPL_ACODE_ONLY)
     #error SPL_ACODE_ONLY is not possible together with SPL_B_DUMMY
#endif

#if  defined(SPL_AB_CODE) && defined(SPL_ACODE_ONLY)
#error Only one of the following options is allowed: SPL_ACODE_ONLY; SPL_AB_CODE
#endif

/*@}*/

/**
 * @name
 *  <b>Platform.</b>
 *
 *  Ensure at least one value is used.
 **********************************************************************/
/*@{*/

#if     !defined(SPL_PLATFORM_PC_GNU)       \
     && !defined(SPL_PLATFORM_VCU)          \
     && !defined(SPL_PLATFORM_PPC)          \
     && !defined(SPL_PLATFORM_LZB)	        \
     && !defined(SPL_PLATFORM_ARM)	        \
	 && !defined(SPL_PLATFORM_PPC_LINUX)	\
	 && !defined(SPL_PLATFORM_PC_LINUX)
     #error One PLATFORM must be defined in spl_config.h
#endif

#if defined(SPL_PLATFORM_PC_GNU)
    #if     defined(SPL_PLATFORM_VCU)    \
         || defined(SPL_PLATFORM_PPC)    \
         || defined(SPL_PLATFORM_LZB)    \
         || defined(SPL_PLATFORM_ARM)    \
         || defined(SPL_PLATFORM_PPC_LINUX) \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_VCU)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_PPC)       \
         || defined(SPL_PLATFORM_LZB)       \
         || defined(SPL_PLATFORM_ARM)       \
         || defined(SPL_PLATFORM_PPC_LINUX) \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_PPC)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_VCU)       \
         || defined(SPL_PLATFORM_LZB)       \
         || defined(SPL_PLATFORM_ARM)       \
         || defined(SPL_PLATFORM_PPC_LINUX) \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_LZB)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_VCU)       \
         || defined(SPL_PLATFORM_PPC)       \
         || defined(SPL_PLATFORM_ARM)       \
         || defined(SPL_PLATFORM_PPC_LINUX) \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_ARM)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_VCU)       \
         || defined(SPL_PLATFORM_PPC)       \
         || defined(SPL_PLATFORM_LZB)       \
         || defined(SPL_PLATFORM_PPC_LINUX) \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_PPC_LINUX)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_VCU)       \
         || defined(SPL_PLATFORM_PPC)       \
         || defined(SPL_PLATFORM_LZB)       \
         || defined(SPL_PLATFORM_ARM)       \
         || defined(SPL_PLATFORM_PC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif

#if defined(SPL_PLATFORM_PC_LINUX)
    #if     defined(SPL_PLATFORM_PC_GNU)    \
         || defined(SPL_PLATFORM_VCU)       \
         || defined(SPL_PLATFORM_PPC)       \
         || defined(SPL_PLATFORM_LZB)       \
         || defined(SPL_PLATFORM_ARM)       \
         || defined(SPL_PLATFORM_PPC_LINUX)
         #error More than one platform is defined in splconfig.h
    #endif
#endif
/*@}*/

/**
 * @name
 * <b>Safety level.</b>
 *
 * Only one setting is allowed.
 ***********************************************************************/
/*@{*/
/* Ensure that *both* settings are not defined */
#if defined(SPL_OPTION_SIL_4) && defined (SPL_OPTION_SIL_2) && defined (SPL_OPTION_SIL_0)
  #error Both SPL_OPTION_SIL_4, SPL_OPTION_SIL_2 and SPL_OPTION_SIL_0 are defined, please use just one of them.
#endif
#if defined(SPL_OPTION_SIL_4) && defined (SPL_OPTION_SIL_2)
  #error Both SPL_OPTION_SIL_4 and SPL_OPTION_SIL_2 are defined, please use just one of them.
#endif
#if defined(SPL_OPTION_SIL_4) && defined (SPL_OPTION_SIL_0)
  #error Both SPL_OPTION_SIL_4 and SPL_OPTION_SIL_0 are defined, please use just one of them.
#endif
#if defined(SPL_OPTION_SIL_2) && defined (SPL_OPTION_SIL_0)
  #error Both SPL_OPTION_SIL_2 and SPL_OPTION_SIL_0 are defined, please use just one of them.
#endif

/* Ensure that one setting is defined */
#if !defined(SPL_OPTION_SIL_4) && !defined (SPL_OPTION_SIL_2) && !defined (SPL_OPTION_SIL_0)
  #error None of SPL_OPTION_SIL_4 or SPL_OPTION_SIL_2 nor SPL_OPTION_SIL_0 is defined, please use one.
#endif

/*@}*/
/**
 * @name
 * <b>Endianness.</b>
 *
 * Only one setting is allowed.
 ***********************************************************************/
/*@{*/
/* Ensure that *both* settings are not defined */
#if defined(SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN) && defined (SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN)
  #error Both SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN and SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN are defined in spl_config.h
#endif

/* Ensure that one setting is defined */
#if !defined(SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN) && !defined (SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN)
  #error None of SPL_OPTION_PROCESSOR_IS_LITTLE_ENDIAN or SPL_OPTION_PROCESSOR_IS_BIG_ENDIAN are defined in spl_config.h
#endif
/*@}*/

/**
 * @name
 * <b>Debugging Traces.</b>
 *
 ***********************************************************************/
 /*@(*/
/* Ensure that *both* settings are not defined */
#if defined(SPL_OPTION_ENABLE_TRACES) && defined (SPL_OPTION_DISABLE_TRACES)
  #error Both SPL_OPTION_ENABLE_TRACES and SPL_OPTION_DISABLE_TRACES are defined in spl_config.h
#endif

/* Ensure that one setting is defined */
#if !defined(SPL_OPTION_ENABLE_TRACES) && !defined (SPL_OPTION_DISABLE_TRACES)
  #error None of SPL_OPTION_ENABLE_TRACES or SPL_OPTION_DISABLE_TRACES are defined in spl_config.h
#endif


/* If traces are enabled we need an output function */
#if defined(SPL_OPTION_ENABLE_TRACES)
    #if !defined(SPL_TRACE_OUTPUT_FUNCTION)
        #error In spl_config.h, SPL_OPTION_ENABLE_TRACES implies SPL_TRACE_OUTPUT_FUNCTION
    #endif
#endif
/*@}*/

/**
 * @name
 * <b>Dump functions.</b>
 *
 ***********************************************************************/
  /*@{*/
/* Ensure that *both* settings are not defined */
#if defined(SPL_INCLUDE_DUMP_FUNCTIONS) && defined (SPL_EXCLUDE_DUMP_FUNCTIONS)
  #error Both SPL_INCLUDE_DUMP_FUNCTIONS and SPL_EXCLUDE_DUMP_FUNCTIONS are defined in spl_config.h
#endif

/* Ensure that one setting is defined */
#if !defined(SPL_INCLUDE_DUMP_FUNCTIONS) && !defined (SPL_EXCLUDE_DUMP_FUNCTIONS)
  #error None of SPL_INCLUDE_DUMP_FUNCTIONS or SPL_EXCLUDE_DUMP_FUNCTIONS are defined in spl_config.h
#endif
/*@}*/

/**
 * @name
 * <b>MAX_CONNECTIONS.</b>
 *
 ***********************************************************************/
/*@{*/
#ifdef SPL_MAX_CONNECTIONS
   #if (SPL_MAX_CONNECTIONS < 1)
        #error Minimum value for SPL_MAX_CONNECTIONS in spl_config.h is 1
   #endif
#else
   #error SPL_MAX_CONNECTIONS must be defined in spl_config.h
#endif
/*@}*/

/**
 * @name
 * <b>SPL_MAX_FIFO_DEPTH.</b>
 *
 ***********************************************************************/
/*@{*/
#ifdef SPL_MAX_FIFO_DEPTH
   #if (SPL_MAX_FIFO_DEPTH < 1)
        #error Minimum value for SPL_MAX_FIFO_DEPTH in spl_config.h is 1
   #endif
#else
   #error SPL_MAX_FIFO_DEPTH must be defined in spl_config.h
#endif
/*@}*/

/**
 * @name
 * <b>Profibus Low Level Drivers.</b>
 *
 * Ensure that each of the drivers is INCLUDED or EXCLUDED.
 *
 ***********************************************************************/
/*@{*/
#if defined(SPL_INCLUDE_DRIVER_IFAK) 
    #error SPL_INCLUDE_DRIVER_IFAK no more supported
#endif

#if defined(SPL_INCLUDE_DRIVER_TCP) && defined (SPL_EXCLUDE_DRIVER_TCP)
    #error SPL_INCLUDE_DRIVER_TCP and SPL_EXCLUDE_DRIVER_TCP are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_TCP) &&  !defined (SPL_EXCLUDE_DRIVER_TCP)
    #error One of SPL_INCLUDE_DRIVER_TCP or SPL_EXCLUDE_DRIVER_TCP must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_UDP) && defined (SPL_EXCLUDE_DRIVER_UDP)
    #error SPL_INCLUDE_DRIVER_UDP and SPL_EXCLUDE_DRIVER_UDP are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_UDP) &&  !defined (SPL_EXCLUDE_DRIVER_UDP)
    #error One of SPL_INCLUDE_DRIVER_UDP or SPL_EXCLUDE_DRIVER_UDP must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_SOFTING) 
    #error SPL_INCLUDE_DRIVER_SOFTING no more supported
#endif

#if defined(SPL_INCLUDE_DRIVER_VIPCO) && defined (SPL_EXCLUDE_DRIVER_VIPCO)
    #error SPL_INCLUDE_DRIVER_VIPCO and SPL_EXCLUDE_DRIVER_VIPCO are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_VIPCO) &&  !defined (SPL_EXCLUDE_DRIVER_VIPCO)
    #error One of SPL_INCLUDE_DRIVER_VIPCO or SPL_EXCLUDE_DRIVER_VIPCO must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_HILSCHER) && defined (SPL_EXCLUDE_DRIVER_HILSCHER)
    #error SPL_INCLUDE_DRIVER_HILSCHER and SPL_EXCLUDE_DRIVER_HILSCHER are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_HILSCHER) &&  !defined (SPL_EXCLUDE_DRIVER_HILSCHER)
    #error One of SPL_INCLUDE_DRIVER_HILSCHER or SPL_EXCLUDE_DRIVER_HILSCHER must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_ATPCU) && defined (SPL_EXCLUDE_DRIVER_ATPCU)
    #error SPL_INCLUDE_DRIVER_ATPCU and SPL_EXCLUDE_DRIVER_ATPCU are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_ATPCU) &&  !defined (SPL_EXCLUDE_DRIVER_ATPCU)
    #error One of SPL_INCLUDE_DRIVER_ATPCU or SPL_EXCLUDE_DRIVER_ATPCU must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_SWITCH_TCP) && defined (SPL_EXCLUDE_DRIVER_SWITCH_TCP)
    #error SPL_INCLUDE_DRIVER_SWITCH_TCP and SPL_EXCLUDE_DRIVER_SWITCH_TCP are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_SWITCH_TCP) &&  !defined (SPL_EXCLUDE_DRIVER_SWITCH_TCP)
    #error One of SPL_INCLUDE_DRIVER_SWITCH_TCP or SPL_EXCLUDE_DRIVER_SWITCH_TCP must be defined in spl_config.h
#endif

#if defined(SPL_INCLUDE_DRIVER_SWITCH_UDP) && defined (SPL_EXCLUDE_DRIVER_SWITCH_UDP)
    #error SPL_INCLUDE_DRIVER_SWITCH_UDP and SPL_EXCLUDE_DRIVER_SWITCH_UDP are exclusive in spl_config.h
#endif

#if !defined(SPL_INCLUDE_DRIVER_SWITCH_UDP) &&  !defined (SPL_EXCLUDE_DRIVER_SWITCH_UDP)
    #error One of SPL_INCLUDE_DRIVER_SWITCH_UDP or SPL_EXCLUDE_DRIVER_SWITCH_UDP must be defined in spl_config.h
#endif
/*@}*/

/**
 * @name
 * <b>Base operation system</b>
 *
 * Ensure that only one base system is defined.
 *
 ***********************************************************************/
/*@{*/
#if defined(SPL_BS_TBSW) && defined (SPL_BS_CSS)
    #error SPL_BS_TBSW and SPL_BS_CSS are exclusive.
#endif

#if defined(SPL_BS_TBSW) && defined (SPL_BS_VXWORKS)
    #error SPL_BS_TBSW and SPL_BS_VXWORKS are exclusive.
#endif

#if defined(SPL_BS_CSS) && defined (SPL_BS_VXWORKS)
    #error SPL_BS_CSS and SPL_BS_VXWORKS are exclusive.
#endif


/*@}*/


#endif


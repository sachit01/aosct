/** @file                          
 *  defines the driver types for SPL.
 *                     
 *********************************************************************
 *                    Bombardier Transportation
 *
 *********************************************************************
 *
 *  Revision 9  2012/09/27      jkrupp
 *      removed TSG part in define names
 *  Revision 8  2012/09/10      ageck
 *        removed softing driver
 *  Revision 7  2012/08/27      jkrupp
 *        rename SPL_Driver_Ethernet to SPL_Driver_TCP
 *        rename SPL_Driver_TSG_SWITCH to SPL_Driver_TSG_SWITCH_TCP
 *        add SPL_Driver_UDP
 *        add SPL_Driver_TSG_SWITCH_UDP
 *        remove SPL_Driver_IFAK
 *
 *  Revision 6  2012/06/12      jkrupp
 *        add SPL_Driver_TSG_SWITCH
 *
 *  Undocumented changes after change from CVS to Synergy
 *
 **********************************************************************/
#ifndef PL_DRIVER_TYPES_H_
#define PL_DRIVER_TYPES_H_

/*************************************************************************************
**************************************************************************************
DO NOT ADD ANY INCLUDES OR IFDEFS TO THAT FILE TO KEEP APPLICATION INTERFACE SIMPLE!!!
*************************************************************************************
**************************************************************************************/
 
/** @name
 *       <b>Driver Types.</b>
 *****************************************************************************/
/*@{*/
/**
 *  Null driver (with empty functions). */
#define SPL_Driver_NULL         0U

/**
 * Profibus Softing driver for VCU. No more supported */
/* #define SPL_Driver_Softing          1U */

/**
 * Profibus Hilscher driver for PowerPC or PC. */
#define SPL_Driver_Hilscher         2U

/**
 * Profibus VIPCO driver for VCU. */
#define SPL_Driver_Vipco            4U

/**
 * Memory driver. */
#define SPL_Driver_MEM              5U

/**
 * Memory driver for LZB. */
#define SPL_Driver_MEM_LZB          6U

/**
 * TCP driver for PC, PowerPC and VCU. */
#define SPL_Driver_TCP              100U

/**
 * UDP driver for PC, PowerPC and VCU. */
#define SPL_Driver_UDP              101U

/**
 * Memory driver for ATPCU */
#define SPL_Driver_ATPCU            103U

/**
 * Driver with switch between TCP and Vipco driver */
#define SPL_Driver_SWITCH_TCP   104U

/**
 * Driver with switch between UDP and Vipco driver */
#define SPL_Driver_SWITCH_UDP   105U


/*@}*/

#endif /* PL_DRIVER_TYPES_H_ */

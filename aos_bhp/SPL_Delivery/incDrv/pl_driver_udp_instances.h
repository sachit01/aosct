/** @file 
  *	
*/
/**********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: ageck %
 *                      %version: 1 %
 *                      %date_created: 2012-09-16  %
 ***********************************************************************
 *
 *  Revision 1  2012/09/16      ageck
 *	reconstructed form previous version not checked in
 *
 **********************************************************************/

/**********************************************************************
 * Defines
 **********************************************************************/
#ifndef PL_DRIVER_UDP_H
#define PL_DRIVER_UDP_H

/*lint -efile(766,pl_driver_udp.c) -e750*/ /*we need interface for both instances*/
#define SPL_ETH_DRIVER_FIRST_INSTANCE
#include "pl_driver_udp.h"
/*lint -e961*/ /*redefinitions needed for same file used with different compiler setting*/
#undef SPL_ETH_DRIVER_FIRST_INSTANCE
/*lint +e961*/

#ifndef SPL_BS_CSS
#define SPL_ETH_DRIVER_SECOND_INSTANCE
#include "pl_driver_udp.h"
/*lint -e961*/ /*redefinitions needed for same file used with different compiler setting*/
#undef SPL_ETH_DRIVER_SECOND_INSTANCE
/*lint +e961 +e750*/
#endif
#endif

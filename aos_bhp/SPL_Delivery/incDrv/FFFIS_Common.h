/** @file 
 *  Include all the common includes.
 *
 */
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: jkrupp %
 *                      %version: 10 %
 *                      %date_created: 2013-09-16 15:04 %
 **********************************************************************
 *
 *  Revision    Date        Name        Description
 *  10          2013/09/16  jkrupp      remove header file for SPL_A_DUMMY
 *
 *  undocumented changes after switch from CVS to Synergy
 *
 *  $Log: FFFIS_Common.h,v $
 *  Revision 1.12  2005/06/24 12:26:35  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.11  2005/05/20 14:22:44  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.10  2005/04/05 14:28:23  wroewe
 *  remove fffis_time.h
 *
 *  Revision 1.9  2004/12/03 09:53:02  mjoost
 *  Start to fix some lint warnings and errors I got at the time starting lint in vxworks context
 *
 *  Revision 1.8  2004/11/26 10:53:08  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 *  Revision 1.7  2004/06/29 15:23:58  mjoost
 *  made compatible to Tornado environment
 *
 *  Revision 1.6  2004/06/23 14:11:09  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.5  2004/06/23 13:05:16  jdiezper
 *  Add FFFIS_Clock
 *
 *  Revision 1.4  2004/03/04 12:08:00  wroewe
 *  Add the FFFIS_Include
 *
 *  Revision 1.3  2004/03/04 08:50:57  jdiezper
 *  options.lnt
 *
 *  Revision 1.2  2004/02/26 14:04:28  jdiezper
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *
 *
 **********************************************************************/
#ifndef FFFIS_COMMON_H_
#define FFFIS_COMMON_H_

#include "spl_config.h"
#include "spl_config_check.h"

#include "FFFIS_Types.h"
#include "fffis_endianess.h"
#include "FFFIS_Assert.h"
#include "fffis_trace.h"
#include "fffis_string.h"
#include "fffis_clock.h"
#include "fffis_random.h"

#ifndef SPL_PLATFORM_LZB
#include "Assert_TBSW.h"
#endif

#endif

/****************************************************************************
*           (C) COPYRIGHT Bombardier , SWEDEN 2011
*           ======================================
*
*    The copyright to the computer program herein is the
*    property of Bombardier, Sweden. All rights reserved.
*    The program may be used and/or copied only with the
*    written permission from Bombardier, or in accordance
*    with the terms and conditions stipulated in the
*    agreement/contract under which the program has been
*    supplied.
*
*
*    MODULE NAME:  mmi_debug.h
*
*    REVISION:     1.0
*
*    PREPARED:     Bo Hermansson
*
*    DESCRIPTION:  Header file for MMI debug functions
*
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*    Rev   Date        Name      Measures
*    -----------------------------------------------------------------------
*    1.0  2011-08-25   Bo H	     Start
*    1.1  2017-08-22   Bo H	     Tailored for BHP
****************************************************************************/
#ifndef MMI_DEBUG_H
#define MMI_DEBUG_H

#include "mmi_types.h"


#ifdef DEBUG_CONSOLE
typedef enum {
  prfInfo = 0,
  prfRcv,
  prfTx,
  prfError,
  prfBegin,
  prfEnd
} prfT;

void prfInit(const char *title, int verboseI);
void prf(const prfT p, const char *fmt, ...);

void prtBundle(uint16 length, void *bundle);
void prtRBundle(uint16 length, void *bundle);

#endif // DEBUG_CONSOLE
 


#endif
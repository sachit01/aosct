/** @file 
 * 
 *     Provide macros for TBSW assertions.
 *
 */
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: alamb %
 *                      %version: 1 %
 *                      %date_created: Thu Nov 10 21:04:13 2005 %
 **********************************************************************/

#ifndef FFFIS_ASSERT_TBSW_H_
#define FFFIS_ASSERT_TBSW_H_

#ifdef SPL_BS_TBSW
/**
 * Assert Handler when TBSW is available
 *       
 *       @param SourceFileName         Variable  description.
 * 
 *       @param LineNumber             Variable  description.
 * 
 *       @param TheExpression          Variable  description.
 * 
 ******************************************************************************/
void TBSW_Assert_Handler    (const spCHAR * const SourceFileName,
                             const spINT          LineNumber,
                             const spCHAR * const TheExpression);

#endif /* SPL_BS_TBSW */

#endif /* FFFIS_ASSERT_TBSW_H_ */

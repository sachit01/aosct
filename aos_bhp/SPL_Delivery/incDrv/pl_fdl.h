/** @file 
 * Definition of FDL constants
 *
 */
/*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: alamb %
 *                      %version: 4 %
 *                      %date_created: Tue Feb 21 12:04:59 2006 %
 **********************cvs out of date********************************* 
 *                      $Author: jkusmira $
 *                      $Revision: 1.4 $
 *                      $Date: 2005/06/28 08:21:48 $
 *                      $Source: P://mpg/sl/pl/pl_fdl.h,v $
 *
 *  $Log: pl_fdl.h,v $
 *  Revision 1.4  2005/06/28 08:21:48  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.3  2005/05/20 14:23:13  mkilianj
 *  converting to doxygen style comments
 *
 *  Revision 1.2  2004/12/20 10:46:52  mjoost
 *  added the cast in order to in line with MISRA rules
 *
 *  Revision 1.1  2004/07/29 11:38:16  jdiezper
 *  Standard Profibus FDL defines
 *
 *
 **********************************************************************/
#ifndef PL_FDL_H_
#define PL_FDL_H_

/**
 * SAP for sending multicast on FDL */
#define FDL_MULTICAST_ADDRESS (spBYTE) 127

/**
 * Internally used value to represent 'Send data no acknolwedge' */
#define FDL_MODE_SDN (spBYTE) 1
/**
 * Internation representation of 'Send data with acknowledge */
#define FDL_MODE_SDA (spBYTE) 2

	/** Maximum SAP number. */
#define FDL_MAX_SAP		 	  64

#endif  /* PL_FDL_H_*/

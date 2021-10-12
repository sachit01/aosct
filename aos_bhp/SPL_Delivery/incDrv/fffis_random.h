/** 
 * @file Header file of Random Numbers Generator.
 * 
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: alamb %
 *                      %version: 4 %
 *                      %date_created: Fri May 05 15:53:50 2006 %
 **********************cvs out of date********************************* 
 *                      $Author: jkusmira $
 *                      $Revision: 1.3 $
 *                      $Date: 2005/06/27 07:19:16 $
 *                      $Source: P://mpg/sl/com/fffis_random.h,v $
 *
 *  $Log: fffis_random.h,v $
 *  Revision 1.3  2005/06/27 07:19:16  jkusmira
 *   Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.2  2005/05/20 14:22:45  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.1  2004/11/26 10:53:12  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 **********************************************************************/
#ifndef FFFIS_RANDOM_H_
#define FFFIS_RANDOM_H_

/**
 * Initialize the Random number generator with an initial
 *    seed.
 *
 * Should be used once at the beginning of the application.
 * The seed should be a pseudo-random value (the local clock).
 *
 * (Re)Initializing will start the same sequence.
 * 
 *     @param Seed                Variable description.
 ****************************************************************/
void FFFIS_Randomize (spUINT32 Seed);

/**
 * Returns a pseudo 32 bits random number.
 * 
 *    @return Result              Variable description.
 ****************************************************************/
spUINT32 FFFIS_Random32 (void);

#endif /* FFFIS_RANDOM */



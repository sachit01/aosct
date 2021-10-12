/** 
 * @file Provide macros for assertions.
 *
*/
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: jkrupp %
 *                      %version: 7.1.9 %
 *                      %date_created: 2013-05-22 13:58 %
 **********************************************************************
 *
 *  Revision 7.1.9      2013/05/22		 jkrupp
 *      add cast for function parameter __FILE__ in Current_Assert_Handler
 *
 *   Undocumented changes
 *
 *  Revision 1.12  2005/07/25 14:30:25  mczaprag
 *  LZB: turned off Lint warnings 917, 774, 506
 *
 *  Revision 1.11  2005/06/27 09:32:56  jkusmira
 *  Move function description to header file
 *
 *  Revision 1.10  2005/06/24 11:45:47  jkusmira
 *  Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.9  2005/06/20 09:38:36  wroewe
 *  add Fehlerbehandlung(0xDFFFu); for LZB
 *
 *  Revision 1.8  2005/05/20 14:22:43  mkilianj
 *  converting to doxygen style comments
 *  converting to doxygen style comments
 *
 *  Revision 1.7  2005/02/22 17:16:44  AGeck
 *      Replace define NULL by 0U in source file
 *      because of conflicts with NULL define in LZB and
 *      disapproved NULL define for C++
 *
 *  Revision 1.6  2004/11/26 10:53:08  jdiezper
 *  Global Project Traversal for separating Simulator from SPL Library.
 *
 *  Revision 1.5  2004/08/05 09:44:43  jdiezper
 *  Add a global prefix (sp) to all basic types (BYTE, INT...)
 *  Modify all variables and function prototypes accordingly.
 *
 *  Revision 1.4  2004/06/23 14:11:09  jdiezper
 *  Transform cplusplus comments in c comments for Tornado compatibility.
 *
 *  Revision 1.3  2004/05/13 15:02:14  mjoost
 *  Matthias
 *
 *  Revision 1.2  2004/02/26 14:04:28  jdiezper
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *  Update top header. Use valid CVS keywords for CVS meta-data expansion.
 *
 *
 **********************************************************************/
#ifndef FFFIS_ASSERT_H_
#define FFFIS_ASSERT_H_


#ifdef SPL_PLATFORM_LZB

#define F_PB_SL_ASSERT    ((USHORT) 0x5FFFu)  /* PB Safety layer assert */

/**
 *    @The function isn't a part of the SPL. The LZB application software
 *     delivers the function.
 *       
 *       @param FehlerNummer           error number.
 * 
 ******************************************************************************/
  extern void Fehlerbehandlung(spUINT16 FehlerNummer);

/**
 * @special Assert() code for LZB.
 ***************************************************************************/
  #define ASSERT(the_condition) \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
  do {                          \
    /*lint --e{917} */          \
    /*lint --e{774} */          \
    /*lint --e{506} */          \
      if ( ! (the_condition) )  \
      {                         \
          Fehlerbehandlung(F_PB_SL_ASSERT); \
      }                         \
  } while (0)                   \
/*lint +e717 */

  #define ASSERTUNCONDITIONAL \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */       \
  do {                        \
    /*lint --e{917} */        \
    /*lint --e{774} */        \
    /*lint --e{506} */        \
  	  Fehlerbehandlung(F_PB_SL_ASSERT); \
  } while (0)                 \
/*lint +e717 */
  
#else /* SPL_PLATFORM_LZB */

/**
  * @todo Typedef descritpion.
  ***************************************************************************/
typedef
        void (* AssertHandler) (const spCHAR * const SourceFileName,
                                const spINT          LineNumber,
                                const spCHAR * const TheExpression);

extern AssertHandler Current_Assert_Handler;

/**
 *  Default Assert Handler.
 *       
 *       @param SourceFileName         Name of the source file.
 * 
 *       @param LineNumber             Line number.
 * 
 *       @param TheExpression          Expression to be tested.
 * 
 ******************************************************************************/
void Default_Assert_Handler (const spCHAR * const SourceFileName,
                             const spINT          LineNumber,
                             const spCHAR * const TheExpression);

/**
 *  Setting Assert Handler.
 *       
 *       @param New_Assert_Handler         New Assert Handler.
 * 
 ******************************************************************************/
AssertHandler FFFIS_Set_Assert_Handler (AssertHandler New_Assert_Handler);

#ifndef SIMULATOR
  /**
   * Definition of ASSERT macro.
   ***************************************************************************/
  /**
   * @bug MISRA Rule 29 - Lint error 64 and MISRA Rule 77 - Lint note 918
   *      In generic product Assert Handler will be changed, defined macros in
   *      <stdio.h> like __FILE__ and __LINE__ will not be used.
   ***************************************************************************/
   #define ASSERT(the_condition)                            \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */         \
   do {                                                     \
        /*lint --e{917} */                                  \
        /*lint --e{774} */                                  \
        /*lint --e{506} */                                  \
       if ( ! (the_condition) )                             \
       {                                                    \
           /*lint --e(918) */                               \
           /*lint --e(64 */                                 \
           Current_Assert_Handler ((const spCHAR * const)__FILE__, __LINE__, (const spCHAR * const)#the_condition);  \
       }                                                    \
   } while (0)                                              \
/*lint +e717 */

#else
   #define ASSERT(the_condition)                            \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */         \
   do {                                                     \
       /*lint --e{917} */                                      \
       /*lint --e{774} */                                      \
       /*lint --e{506} */                                      \
       if ( ! (the_condition) )                                \
       {                                                       \
       	   TRACE((CH_TEST_SIM, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));  \
           TRACE((CH_TEST_SIM, "  ___ ___  _  _ ___ ___ _____ ___ ___  _  _   _   _\n"));  \
           TRACE((CH_TEST_SIM, " / __/ _  |  | |   |_ _|_   _|_ _/ _ )| )| | /_) | |\n"));  \
           TRACE((CH_TEST_SIM, "| (_| (_) | .` | |) | |  | |  | | (_) | .` |/ _ )| |__\n"));  \
           TRACE((CH_TEST_SIM, " (___(___/|_| _|___/___| |_| |___(___/|_| _/_/  _)____|\n"));  \
           TRACE((CH_TEST_SIM, "   _   ___ ___ ___ ___ _____      _ _ _ _\n"));  \
           TRACE((CH_TEST_SIM, "  /_) / __/ __| __| _ )_   _|    | | | | |\n"));  \
           TRACE((CH_TEST_SIM, " / _ )(__ )__ ) _||   / | |      |_|_|_|_|\n"));  \
           TRACE((CH_TEST_SIM, "/_/  _)___/___/___|_|_) |_|      (_|_|_|_)\n"));  \
           TRACE((CH_TEST_SIM, "File: %s(%d), condition: %s\n",__FILE__, __LINE__, #the_condition));  \
           TRACE((CH_TEST_SIM, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));  \
           Current_Assert_Handler (__FILE__, __LINE__, #the_condition);  \
       }                                                       \
   } while (0)                                                 \
/*lint +e717 */

#endif /* SIMULATOR */

#ifndef SIMULATOR
  /**
   * Definition of ASSERTUNCONDITIONAL macro.
   ***************************************************************************/
  /**
   * @bug MISRA Rule 29 - Lint error 64 and MISRA Rule 77 - Lint note 918
   *      In generic product Assert Handler will be changed, defined macros in
   *      <stdio.h> like __FILE__ and __LINE__ will not be used.
   ***************************************************************************/
   #define ASSERTUNCONDITIONAL \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */ \
   do {                                             \
       /*lint --e(918) */                           \
       /*lint --e{917} */                           \
       /*lint --e{774} */                           \
       /*lint --e{506} */                           \
       /*lint --e(64) */                            \
  	   Current_Assert_Handler ((const spCHAR * const)__FILE__, __LINE__, (const spCHAR * const)"Assert spFALSE");  \
   } while (0)                                      \
/*lint +e717 */

#else
   #define ASSERTUNCONDITIONAL \
/*lint -e717 do...while (0) MISRA 2004 rule 19.4 */ \
   do {                          \
       /*lint --e{917} */        \
  	   /*lint --e{774} */        \
       /*lint --e{506} */        \
       TRACE((CH_TEST_SIM, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));  \
       TRACE((CH_TEST_SIM, " _   _ _  _   ___ ___  _  _ ___ ___ _____ ___ ___  _  _   _   _\n"));  \
       TRACE((CH_TEST_SIM, "| | | |  | | / __/ _ )|  | |   )_ _|_   _|_ _/ _ )|  | | /_) | |\n"));  \
       TRACE((CH_TEST_SIM, "| |_| | .` || (_| (_) | .` | |) | |  | |  | | (_) | .` |/ _ )| |__\n"));  \
       TRACE((CH_TEST_SIM, "  ___/|_|(_| (___)___/|_|(_|___/___| |_| |___(___/|_|(_/_/  _)____|\n"));  \
       TRACE((CH_TEST_SIM, "   _   ___ ___ ___ ___ _____      _ _ _ _\n"));  \
       TRACE((CH_TEST_SIM, "  /_) / __/ __| __| _ )_   _|    | | | | |\n"));  \
       TRACE((CH_TEST_SIM, " / _ )(__ )__ ) _||   / | |      |_|_|_|_|\n"));  \
       TRACE((CH_TEST_SIM, "/_/  _)___/___/___|_|_) |_|      (_|_|_|_)\n"));  \
       TRACE((CH_TEST_SIM, "File: %s(%d)\n",__FILE__, __LINE__));  \
       TRACE((CH_TEST_SIM, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"));  \
       Current_Assert_Handler (__FILE__, __LINE__, "Unconditional");  \
   } while (0)                   \
/*lint +e717 */

#endif /* SIMULATOR */  
 
#endif /* SPL_PLATFORM_LZB */

  /**
   * Definition of ASSERTPOINTER macro.
   ***************************************************************************/
#define ASSERTPOINTER(the_pointer) ASSERT ( (the_pointer) != 0U)

#endif /* FFFIS_ASSERT_H_ */

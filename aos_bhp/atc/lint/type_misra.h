/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2014
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: 
*
* %name: type_misra.h %
* %version: 1 %
* %created_by: bhermans %
* %date_created: 2015-04-28 10:59 %
* %Creation date of original object: Fri Nov 17 13:53:54 2000 %
*
* Description:
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date       Sign     Change description
* 1.1      2014-02-04 azacher  add header, moves from vio_unit_client.* to vioh_client.*.
* 1.2      2014-02-04 azacher  
* 
*
*******************************************************************************/
#ifndef TYPES_MISRA_H
#define TYPES_MISRA_H





#ifdef __cplusplus

namespace VIOHnames {
extern "C" {

#endif





#ifndef NULL

    #ifdef __cplusplus
        /*lint -emacro((910),NULL)  to fulfil MISRA CPP 2008 RULE 4-10-2 */
        /*lint --emacro((910),NULL) */ /* to fulfil MISRA CPP 2008 RULE 4-10-2 */

        #define NULL 0UL 

    #else

        #define NULL ((void*) 0uL)

    #endif 

#endif

/* Rule 4-10-2 (Required) Literal zero (0) shall not be used as the null-pointer-constant */
/* NULL is not defined in C++ 2003, but expected in MISRA samples  */
/* C++ 2011 will use nullptr */



typedef char char_t;

/*typedef bool bool_t; */  /* conflict with vfw_boolh.h */

/*typedef unsigned char   uint8_t; */   /* standard types */

/*typedef unsigned short  uint16_t; */  /* standard types */

/*typedef unsigned long   uint32_t; */  /* standard types */





#ifdef __cplusplus

}       /* extern C*/

}       /* namespace*/

#endif



#endif /* TYPES_MISRA_H */


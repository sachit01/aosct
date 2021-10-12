#ifndef SPLHandler_h
#define SPLHandler_h
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The SPL Handler component deals with the interface between the OPC Agent and the AOS SW.
*  AbstractSPLHandler implements the core functionality of the component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-25    rquensel    SPL C-API
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

/** @file */

#if defined(__cplusplus)
extern "C"
{
#endif


/**
* Interface function called by the SPL Library
*
* @param [out] pBuff Pointer to a Profibus_Type buffer.
* @return 1 if data is available, 0 otherwise
*
*/
uint8_t _IOgetProfibusInputData(Profibus_Type* pBuff); //lint !e1960 Needed as glue towards the SPL library


/**
* Interface function called by the SPL Library
*
* @param [in] pBuff Pointer to a Profibus_Type buffer.
*
*/
void _IOsetProfibusOutputData(Profibus_Type* pBuff); //lint !e1960 Needed as glue towards the SPL library

#if defined(__cplusplus)
}
#endif

#endif

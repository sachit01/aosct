#ifndef ATPVersion_hpp
#define ATPVersion_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This name-space will maintain different version,subversion numbers of ATP.
*
******************************************************************************/
/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-11-08    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  // Note, this file is not intended for use outside atp_application.
  // Please use:
  // ATC::AbstractApplicationBase::corePtr()->getApplicationName(),
  // ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString()
  // And implement the virtual function AbstractATPApplication::validateDispatcherVersion() instead of accessing expectedDispatcherVersion directly

  //lint -esym(1923, ATP_VERSION_STRING, ATP_APPLICATION_NAME) defines are used in order to be able to concatenate strings.
#ifdef _SYS_INT
  #define ATP_VERSION_STRING  "1.58.0 (without LSSD)"  // Version format x.y.z. Major Version number = x, Minor Version = y, SubVersion = z
#else
  #define ATP_VERSION_STRING  "1.58.0 (with LSSD)"  // Version format x.y.z. Major Version number = x, Minor Version = y, SubVersion = z
#endif
#define ATP_APPLICATION_NAME "ATP"    // Application name

  static const char_t expectedDispatcherVersion[] = "1.47.0";
}

#endif

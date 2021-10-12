#ifndef DispatcherVersion_hpp
#define DispatcherVersion_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This name-space will maintain different version,subversion numbers of Dispatcher.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-11-08    rquensel     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DEFINES
******************************************************************************/
// Note, this file is not intended for use outside disp_application.
// Please use:
// ATC::AbstractApplicationBase::corePtr()->getApplicationName(),
// ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString()
//lint -esym(1923, DISPATCHER_VERSION_STRING, DISPATCHER_APPLICATION_NAME) defines are used in order to be able to concatenate strings.
#define DISPATCHER_VERSION_STRING  "1.47.0"        // Version format x.y.z. Major Version number = x, Minor Version = y, SubVersion = z
#define DISPATCHER_APPLICATION_NAME "Dispatcher"   // Application name

#endif

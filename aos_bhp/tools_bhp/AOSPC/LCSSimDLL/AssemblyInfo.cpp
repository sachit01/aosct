#include "stdafx.h"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AssemblyInfo.cpp %
*
*  %version:       8 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
*
*  DESCRIPTION: LCSSim DLL version number
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-11-19    Antbäck     File created
* 2014-03-07    Antbäck     Version changed to 1.1.0 (DLL file)
* 2014-03-24    Antbäck     Version changed to 1.2.0 (DLL file)
* 2014-03-31    Hidaji      Version changed to 1.3.0 (DLL file)
* 2014-04-03    Antbäck     Version changed to 1.4.0 (DLL file)
* 2014-07-01    Hidaji      Version changed to 1.5.0 (DLL file)
* 2014-12-16    Antbäck     Version changed to 1.5.1 (DLL file)
* 2014-12-18    Antbäck     Version changed to 1.6.0 (DLL file)
* 2015-01-29    Antbäck     Version changed to 1.6.1 (DLL file)
* 2015-02-26    Antbäck     Version changed to 2.0.0
* 2015-03-12    Antbäck     Version changed to 2.1.0 (Merge activity)
* 2015-03-12    Antbäck     Added description to file header
* 2017-02-27    Marlundg    Version changed to 3.0.0 (DLL file)
* 2017-03-24    Marlundg    Version changed to 3.0.1 (DLL file)
* 2017-05-12    NSyed       Version changed to 3.0.2 (DLL file)
* 2017-05-05    Marlundg    Version changed to 3.0.3 (DLL file)
* 2017-06-07    Marlundg    Version changed to 3.0.4 (DLL file)
* 2017-06-26    Marlundg    Version changed to 3.0.5 (DLL file)
* 2017-07-12    Marlundg    Version changed to 3.0.6 (DLL file)
* 2017-09-15    adgupta     Version changed to 3.0.7 (DLL file)
* 2018-01-23    nsyed       Version changed to 3.0.8 (DLL file)
* 2018-02-13    Marlundg    Version changed to 3.0.9 (DLL file)
* 2018-06-28    prsrivas    Version changed to 3.0.10 (DLL file)
* 2018-11-15    Marlundg    Version changed to 3.0.12 (DLL file)
* 2018-01-22    Maralund    Version changed to 3.0.14 (DLL file)
* 2019-10-24    bhermans    Version changed to 3.0.15 (DLL file)
* 2020-05-12    bhermans    Version changed to 3.0.16 (DLL file)
*******************************************************************************/

using namespace System;
using namespace System::Reflection;
using namespace System::Runtime::CompilerServices;
using namespace System::Runtime::InteropServices;
using namespace System::Security::Permissions;

//
// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly:AssemblyTitleAttribute("LCSSimDLL")];
[assembly:AssemblyDescriptionAttribute("")];
[assembly:AssemblyConfigurationAttribute("")];
[assembly:AssemblyCompanyAttribute("Bombardier Transportation")];
[assembly:AssemblyProductAttribute("LCSSimDLL")];
[assembly:AssemblyCopyrightAttribute("Copyright (c) Bombardier Transportation 2020")];
[assembly:AssemblyTrademarkAttribute("")];
[assembly:AssemblyCultureAttribute("")];

//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version
//      Build Number
//      Revision
//
// You can specify all the value or you can default the Revision and Build Numbers
// by using the '*' as shown below:

[assembly:AssemblyVersionAttribute("3.1.0.*")];

[assembly:ComVisible(false)];

[assembly:CLSCompliantAttribute(true)];

[assembly:SecurityPermission(SecurityAction::RequestMinimum, UnmanagedCode = true)];

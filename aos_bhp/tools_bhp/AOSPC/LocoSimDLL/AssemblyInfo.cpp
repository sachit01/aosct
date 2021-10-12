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
*  %version:       7 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-13 15:06 %
*
*  DESCRIPTION: LocoSim DLL version number 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-03-07    Antbäck     Imported to AOS-PC
*                           Version changed to 1.1.0 (DLL file)
* 2014-03-10    Antbäck     Version changed to 1.2.0 (DLL file)
* 2014-03-24    Antbäck     Version changed to 1.3.0 (DLL file)
* 2014-04-03    Antbäck     Version changed to 1.4.0 (DLL file)
* 2014-04-10    Hidaji      Version changed to 1.5.0 (DLL file)
* 2014-04-14    Hidaji      Version changed to 1.6.0 (DLL file)
* 2015-02-26    Antbäck     Version changed to 2.0.0
* 2015-03-12    Antbäck     Added description to file header
* 2016-10-04    Marlundg    Version changed to 2.1.0
* 2016-12-09    Marlundg    Version changed to 2.1.1
* 2016-03-10    Marlundg    Version changed to 2.1.2
* 2016-03-20    Marlundg    Version changed to 2.1.3
* 2017-07-10    akushwah    Version changed to 2.1.4
* 2017-07-13    akushwah    Version changed to 2.1.5
* 2017-12-13    NSyed       Version changed to 2.1.6
* 2017-12-13    Marlundg    Version changed to 2.1.7
* 2018-03-02    Marlundg    Version changed to 2.1.8
* 2018-11-18    BHermans    Version changed to 2.2.0
* 2018-12-18    BHermans    Version changed to 2.2.1
* 2019-04-11    BHermans    Version changed to 2.2.2
* 2020-01-23    BHermans    Version changed to 2.3.1
* 2020-01-30    BHermans    Version changed to 2.3.2
* 2020-02-20    BHermans    Version changed to 2.3.3
* 2020-04-15    BHermans    Version changed to 2.3.4
* 2020-05-14    BHermans    Version changed to 2.3.5
* 2020-06-10    BHermans    Version changed to 2.3.6
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
[assembly:AssemblyTitleAttribute("LocoSimDLL")];
[assembly:AssemblyDescriptionAttribute("")];
[assembly:AssemblyConfigurationAttribute("")];
[assembly:AssemblyCompanyAttribute("Bombardier Transportation")];
[assembly:AssemblyProductAttribute("LocoSimDLL")];
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

[assembly:AssemblyVersionAttribute("2.3.7.*")];

[assembly:ComVisible(false)];

[assembly:CLSCompliantAttribute(true)];

[assembly:SecurityPermission(SecurityAction::RequestMinimum, UnmanagedCode = true)];

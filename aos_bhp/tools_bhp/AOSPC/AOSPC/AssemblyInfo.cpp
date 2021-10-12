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
*  %version:       16 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-13 15:04 %
*
*  DESCRIPTION: AOS-PC top level version number
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
* 2013-11-26    Bo H        Version changed to 1.0.1
* 2013-12-03    Antbäck     Version changed to 1.0.3
* 2013-12-09    Antbäck     Version changed to 1.0.4
* 2014-03-07    Antbäck     Version changed to 1.1.0
* 2014-03-10    Antbäck     Version changed to 1.2.0
* 2014-03-12    Antbäck     Version changed to 1.3.0
* 2014-03-24    Antbäck     Version changed to 1.4.0
* 2014-04-03    Antbäck     Version changed to 1.5.0
* 2014-08-14    Bo H        Version changed to 1.6.0 because of LCSSim CheckBox LowBattery corrected
* 2014-12-16    Antbäck     Version changed to 1.6.1
* 2015-02-26    Antbäck     Version changed to 2.0.0
* 2015-03-12    Antbäck     Added description to file header
*                           Merge of AOS 3.5.2 and 3.5.3 activities
*                           Version changed to 2.1.0
* 2016-10-16    Marlundg    Version changed to 2.1.1
* 2016-12-09    Marlundg    Version changed to 2.1.3
* 2017-02-27    Marlundg    Version changed to 2.1.4
* 2017-03-10    Marlundg    Version changed to 2.1.5
* 2017-03-20    Marlundg    Version changed to 2.1.6
* 2017-03-24    Marlundg    Version changed to 2.1.7
* 2017-05-05    NSyed       Version changed to 2.1.8
* 2017-05-15    Marlundg    Version changed to 2.1.9
* 2017-06-07    Marlundg    Version changed to 2.1.10
* 2017-06-14    akushwah    Version changed to 2.1.11
* 2017-06-26    marlundg    Version changed to 2.1.12
* 2017-07-10    akushwah    Version changed to 2.1.13
* 2017-07-12    marlundg    Version changed to 2.1.14
* 2017-07-13    akushwah    Version changed to 2.1.15
* 2017-09-15    adgupta     Version changed to 2.1.16
* 2017-09-15    Marlundg    Version changed to 2.1.17
* 2018-03-12    Marlundg    Version changed to 2.1.18
* 2018-06-28    prsrivas    Version changed to 2.1.19
* 2018-11-15    marlundg    Version changed to 2.1.22
* 2018-11-19    marlundg    Version changed to 2.1.23
* 2018-12-18    bhermans    Version changed to 2.1.25
* 2019-01-22    maralund    Version changed to 2.1.26
* 2019-01-25    akushwah    Version changed to 2.1.27
* 2019-03-18    akushwah    Version changed to 2.1.30
* 2019-04-05    akushwah    Version changed to 2.1.31
* 2019-04-12    bhermans    Version changed to 2.1.32
* 2019-10-24    bhermans    Version changed to 2.2.3
* 2019-11-13    bhermans    Version changed to 2.2.4
* 2020-01-23    bhermans    Version changed to 2.2.5
* 2020-01-30    bhermans    Version changed to 2.2.6
* 2020-02-20    bhermans    Version changed to 2.2.7
* 2020-04-15    ikari       Version changed to 2.2.8
* 2020-05-12    bhermans    Version changed to 2.2.9
* 2020-05-14    bhermans    Version changed to 2.2.10
* 2020-06-10    bhermans    Version changed to 2.2.12
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
[assembly:AssemblyTitleAttribute("AOSPC")];
[assembly:AssemblyDescriptionAttribute("")];
[assembly:AssemblyConfigurationAttribute("")];
[assembly:AssemblyCompanyAttribute("Bombardier Transportation")];
[assembly:AssemblyProductAttribute("AOSPC")];
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

[assembly:AssemblyVersionAttribute("2.3.0.*")];

[assembly:ComVisible(false)];

[assembly:CLSCompliantAttribute(true)];

[assembly:SecurityPermission(SecurityAction::RequestMinimum, UnmanagedCode = true)];

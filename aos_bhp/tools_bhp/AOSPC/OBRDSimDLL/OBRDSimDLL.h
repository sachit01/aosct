#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OBRDSimDLL.h %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2018-10-11 12:40 %
*
*  DESCRIPTION: OBRD Simulator Logic
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-11    marlundg    File created
*
*******************************************************************************/
#include <time.h>

#include "stdafx.h"
#include "Windows.h"

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;
using namespace System::IO;
using namespace System::Runtime::InteropServices;
using namespace System::Reflection;

#include "OBRDSimTypes.h"
#include "ATPOBRDCom.h"
#include "../LocoSimDLL/LocoConsts.h"

namespace OBRDSimDLL {

  public ref class OBRDSimulation
  {

  public:

    OBRDSimulation(String^ fileName, String^ obrdSimFileName);
    void Tick();
    void SaveParametersToIniFile();
    void SaveTrackParamsToIniFile();
    void UpdatePath(array<unsigned short> ^path);
    void UpdateAOSStatus(unsigned short trackId, unsigned long position, unsigned long long timeStamp, bool locoTowardsLeg1, bool locoLeading);
    void UpdateBrakePressure(int brakePressureInKpaIn);
    void UpdateSafetyParams();
    bool GetMissingTrackFault(void);

    // General Parameters
    EnumSimulationMode mdlSimMode;
    unsigned short mdlTrainLength;
    unsigned short mdlTimestampDelay;
    unsigned char mdlVSIMLastCarBP;

    // Periodicity in seconds between status-reports sent to AOS.
    unsigned short mdlStatusReportPeriodicity;
    
    // Error injection parameters
    EnumFaultInjection mdlFaultInjection;
    short mdlPosErrorOffset;
    unsigned char mdlOverrideLastCarBP;

    // Enables communication from OBRSSim to AOS
    bool mdlEnableComWithATP;
    
    // Track parameters
    unsigned short            mdlNumberOfTracks;
    array<unsigned short>^    mdlTrackId;
    array<unsigned long>^     mdlTrackLength; // centimeters

    // Communication Parameters
    unsigned short mdlSiteId;
    String ^ mdlReceiverId;
    String ^ mdlSenderId;
    unsigned short mdlLocoId;
    unsigned char mdlMajorVersion;
    unsigned char mdlMinorVersion;

    // Debug info
    unsigned short    guiRejectFromATPCnt;
    array<String^>^   guiRejectFromATPHeader;
    array<String^>^   guiRejectFromATP;

    unsigned short    guiProtocolFromATPCnt;
    array<String^>^   guiProtocolFromATPHeader;
    array<String^>^   guiProtocolFromATP;

    unsigned short    guiStatusToATPCnt;
    array<String^>^   guiStatusToATPHeader;
    array<String^>^   guiStatusToATP;

    unsigned short    guiProtocolToATPCnt;
    array<String^>^   guiProtocolToATPHeader;
    array<String^>^   guiProtocolToATP;

    // Connection status to ATP
    bool  atpConnected;


    // DLLVersion
    String^     DLLVersion;

  private:

    void RunOBRDSimulation();
    void CalculateTrainRearPos(posItem &rearPosition);
    void ResetRealtimeData();

    // OBRD Low-level protocol-stack
    ATPOBRDCom^ atpOBRDCom;

    unsigned short int  aosOBRDPort;

    // Current state
    OBRDStateEnum       currOBRDState;
    unsigned char       currLastCarBrakePressure;
    unsigned char       previousLastCarBrakePressure;

    // Init files
    String^             iniFileName;
    String^             iniObrdSimFileName;

    // Communication towards AOS
    IPAddress^          aosIp;

    // Help variables
    time_t              previousTime;
    bool                frontPosUpdated;
    posItem             previousRearPosition;

    // Flags to define is enough data is available from LCS to calculate Rear-position
    bool                frontPosValid;
    bool                pathPosValid;

    // Current active Path
    array<pathItem>^ currentPath;

    // Recent Train front positions
    array<posItem>^ recentFrontPositions;
    Byte currentPosIndex;

    // Loco is leading if true
    bool                locoLeading;

    // Re-connection timeout in seconds
    const Byte reConnectTimeout = 5U;

    // Tracks in track-list doesn't contain tracks in path
    bool pathFault;
  };
}

#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OBRDSimDLL.cpp %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2018-10-11 12:40 %
*
*  DESCRIPTION: OBRD Simulator Logic. 
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
#include "stdafx.h"
#include "OBRDSimDLL.h"
#include "ATPOBRDCom.h"

/***********************************************************************************************************
* Function:     OBRDSimulation
* Description:  Constructor
***********************************************************************************************************/
OBRDSimDLL::OBRDSimulation::OBRDSimulation(String^ fileName, String^ obrdSimFileName)
{
  char  tmpStr[100];
  
  // Remember ini-filenames
  iniFileName = fileName;
  iniObrdSimFileName = obrdSimFileName;
  
  // Create path and front-pos list
  currentPath = gcnew array<pathItem>(pathArraySize);
  recentFrontPositions = gcnew array<posItem>(frontPosArraySize);

  // GUI data
  guiRejectFromATPCnt = 0;
  guiRejectFromATPHeader = gcnew array<String^>(50);
  guiRejectFromATP = gcnew array<String^>(50);

  guiProtocolFromATPCnt = 0;
  guiProtocolFromATPHeader = gcnew array<String^>(50);
  guiProtocolFromATP = gcnew array<String^>(50);

  guiStatusToATPCnt = 0;
  guiStatusToATPHeader = gcnew array<String^>(50);
  guiStatusToATP = gcnew array<String^>(50);

  guiProtocolToATPCnt = 0;
  guiProtocolToATPHeader = gcnew array<String^>(50);
  guiProtocolToATP = gcnew array<String^>(50);

  guiRejectFromATPHeader[guiRejectFromATPCnt++] = "Reject Reason";
  guiRejectFromATPHeader[guiRejectFromATPCnt++] = "Major Prot Version";
  guiRejectFromATPHeader[guiRejectFromATPCnt++] = "Minor Prot Version";

  guiProtocolFromATPHeader[guiProtocolFromATPCnt++] = "Major Prot Version";
  guiProtocolFromATPHeader[guiProtocolFromATPCnt++] = "Minor Prot Version";

  guiStatusToATPHeader[guiStatusToATPCnt++] = "Track ID";
  guiStatusToATPHeader[guiStatusToATPCnt++] = "Position in track";
  guiStatusToATPHeader[guiStatusToATPCnt++] = "Timestamp";
  guiStatusToATPHeader[guiStatusToATPCnt++] = "Last Car brake pressure";

  guiProtocolToATPHeader[guiProtocolToATPCnt++] = "Major Protocol Version";
  guiProtocolToATPHeader[guiProtocolToATPCnt++] = "Minor Protocol Version";

  // Reset temporary data
  ResetRealtimeData();

  previousTime = 0;
  currLastCarBrakePressure = 0;
  previousLastCarBrakePressure = 0;

  // Read AOSPC data-file.
  char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();

  mdlSimMode = (EnumSimulationMode)GetPrivateProfileIntA("AOSPC", "SimulationMode", SimulationSil, tmpIniFile);
  GetPrivateProfileStringA("AOSPC", "DISPIP", "192.168.2.12", tmpStr, sizeof(tmpStr), tmpIniFile);
  
  aosIp = SimulationSil == mdlSimMode ? IPAddress::Parse("127.0.0.1") : IPAddress::Parse(Marshal::PtrToStringAnsi((IntPtr)tmpStr));
  
  aosOBRDPort = GetPrivateProfileIntA("OBRDSim", "AOSOBRDPortToConnect", 30151, tmpIniFile);
  mdlTrainLength = GetPrivateProfileIntA("OBRDSim", "TrainLength", 100, tmpIniFile);
  mdlTimestampDelay = GetPrivateProfileIntA("OBRDSim", "TimeStampDelay", 20, tmpIniFile);
  mdlVSIMLastCarBP = GetPrivateProfileIntA("OBRDSim", "LastCarBrakePressureVSIM", 75, tmpIniFile);
  mdlOverrideLastCarBP = GetPrivateProfileIntA("OBRDSim", "LastCarBrakePressureError", 50, tmpIniFile);
  mdlStatusReportPeriodicity = GetPrivateProfileIntA("OBRDSim", "StatusReportPeriodicity", 5, tmpIniFile);
  mdlFaultInjection = (EnumFaultInjection)GetPrivateProfileIntA("OBRDSim", "FaultInjection", 0, tmpIniFile);
  mdlEnableComWithATP = GetPrivateProfileIntA("OBRDSim", "EnableCommunicationWithAOS", 0, tmpIniFile) != 0 ? true : false;
  mdlPosErrorOffset = GetPrivateProfileIntA("OBRDSim", "PosErrorOffset", 50, tmpIniFile);

  mdlSiteId = GetPrivateProfileIntA("OBRDSim", "SiteId", 1, tmpIniFile);

  GetPrivateProfileStringA("OBRDSim", "ReceiverID", "RecId_1", tmpStr, sizeof(tmpStr), tmpIniFile);
  mdlReceiverId = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

  GetPrivateProfileStringA("OBRDSim", "SenderID", "SndId_1", tmpStr, sizeof(tmpStr), tmpIniFile);
  mdlSenderId = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

  mdlLocoId = GetPrivateProfileIntA("OBRDSim", "LocoID", 2, tmpIniFile);

  mdlMajorVersion = GetPrivateProfileIntA("OBRDSim", "ProtocolMajorVersion", 1, tmpIniFile);
  mdlMinorVersion = GetPrivateProfileIntA("OBRDSim", "ProtocolMinorVersion", 0, tmpIniFile);

  // Create the ATP OBRD Communication object
  atpOBRDCom = gcnew ATPOBRDCom(aosIp, aosOBRDPort, mdlSiteId, mdlReceiverId, mdlSenderId, mdlLocoId);
  
  // Read Tracks data-file.
  mdlTrackId = gcnew array<unsigned short>(maxTracks);
  mdlTrackLength = gcnew array<unsigned long>(maxTracks);

  char *tmpObrdSimfileName = (char *)Marshal::StringToHGlobalAnsi(obrdSimFileName).ToPointer();

  mdlNumberOfTracks = 0;
  bool endOfTracks = false;

  pathFault = false;

  // Read as long as there are more tracks defined in ini-file
  for (int i = 0; (i < maxTracks) && (!endOfTracks); i++)
  {
    String^ trackIdParam("TrackId_");
    String^ trackLengthParam("Length_");

    trackIdParam += (i + 1);
    trackLengthParam += (i + 1);

    // Parameter-names
    char *tmpTrackIdParam = (char *)Marshal::StringToHGlobalAnsi(trackIdParam).ToPointer();
    char *tmpTrackLengthParam = (char *)Marshal::StringToHGlobalAnsi(trackLengthParam).ToPointer();

    // Values
    unsigned short tmpTrackIdValue = GetPrivateProfileIntA("TrackList", tmpTrackIdParam, 0, tmpObrdSimfileName);
    unsigned long tmpTrackLengthValue = GetPrivateProfileIntA("TrackList", tmpTrackLengthParam, 0, tmpObrdSimfileName);

    // Free temporary buffer again
    Marshal::FreeHGlobal(IntPtr(tmpTrackIdParam));
    Marshal::FreeHGlobal(IntPtr(tmpTrackLengthParam));

    // If no 'TrackId_XX' was found then stop to parse (no more tracks to load!)
    if (tmpTrackIdValue != 0U)
    {
      ++mdlNumberOfTracks;

      mdlTrackId[i] = tmpTrackIdValue;
      mdlTrackLength[i] = tmpTrackLengthValue;
    }
    else
    {
      endOfTracks = true;
    }
  }

  // Get DLL version
  DLLVersion = " - v" +
    Assembly::GetExecutingAssembly()->GetName()->Version->Major.ToString() + "." +
    Assembly::GetExecutingAssembly()->GetName()->Version->Minor.ToString() + "." +
    Assembly::GetExecutingAssembly()->GetName()->Version->Build.ToString();

  // Free temporary buffer again
  Marshal::FreeHGlobal(IntPtr(tmpIniFile));
  Marshal::FreeHGlobal(IntPtr(tmpObrdSimfileName));
}

/***********************************************************************************************************
* Function:     SaveTrainCompToIniFile
* Description:  Saves the current model values to the [OBRDSim] chapter in the ini-file.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::SaveParametersToIniFile()
{
  char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();

  char *tmpTrainLength = (char *)Marshal::StringToHGlobalAnsi(" " + mdlTrainLength).ToPointer();
  char *tmpTimestampDelay = (char *)Marshal::StringToHGlobalAnsi(" " + mdlTimestampDelay).ToPointer();
  char *tmpVSIMLastCarBP = (char *)Marshal::StringToHGlobalAnsi(" " + mdlVSIMLastCarBP).ToPointer();
  char *tmpOverrideLastCarBP = (char *)Marshal::StringToHGlobalAnsi(" " + mdlOverrideLastCarBP).ToPointer();
  char *tmpStatusReportPeriodicity = (char *)Marshal::StringToHGlobalAnsi(" " + mdlStatusReportPeriodicity).ToPointer();
  char *tmpFaultInjection = (char *)Marshal::StringToHGlobalAnsi(" " + (Byte)mdlFaultInjection).ToPointer();
  char *tmpEnableComWithATP = (char *)Marshal::StringToHGlobalAnsi(" " + (mdlEnableComWithATP ? 1 : 0)).ToPointer();
  char *tmpPosErrorOffset = (char *)Marshal::StringToHGlobalAnsi(" " + mdlPosErrorOffset).ToPointer();
  char *tmpSiteId = (char *)Marshal::StringToHGlobalAnsi(" " + mdlSiteId).ToPointer();
  char *tmpReceiverId = (char *)Marshal::StringToHGlobalAnsi(" " + mdlReceiverId).ToPointer();
  char *tmpSenderId = (char *)Marshal::StringToHGlobalAnsi(" " + mdlSenderId).ToPointer();
  char *tmpLocoId = (char *)Marshal::StringToHGlobalAnsi(" " + mdlLocoId).ToPointer();
  char *tmpMajorVersion = (char *)Marshal::StringToHGlobalAnsi(" " + mdlMajorVersion).ToPointer();
  char *tmpMinorVersion = (char *)Marshal::StringToHGlobalAnsi(" " + mdlMinorVersion).ToPointer();

  WritePrivateProfileStringA("OBRDSim", "TrainLength", tmpTrainLength, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "TimestampDelay", tmpTimestampDelay, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "LastCarBrakePressureVSIM", tmpVSIMLastCarBP, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "LastCarBrakePressureError", tmpOverrideLastCarBP, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "StatusReportPeriodicity", tmpStatusReportPeriodicity, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "FaultInjection", tmpFaultInjection, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "EnableCommunicationWithAOS", tmpEnableComWithATP, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "PosErrorOffset", tmpPosErrorOffset, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "SiteId", tmpSiteId, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "ReceiverID", tmpReceiverId, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "SenderID", tmpSenderId, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "LocoID", tmpLocoId, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "ProtocolMajorVersion", tmpMajorVersion, tmpIniFile);
  WritePrivateProfileStringA("OBRDSim", "ProtocolMinorVersion", tmpMinorVersion, tmpIniFile);
  
  // Free temporary buffer again
  Marshal::FreeHGlobal(IntPtr(tmpTrainLength));
  Marshal::FreeHGlobal(IntPtr(tmpTimestampDelay));
  Marshal::FreeHGlobal(IntPtr(tmpVSIMLastCarBP));
  Marshal::FreeHGlobal(IntPtr(tmpOverrideLastCarBP));
  Marshal::FreeHGlobal(IntPtr(tmpStatusReportPeriodicity));
  Marshal::FreeHGlobal(IntPtr(tmpFaultInjection));
  Marshal::FreeHGlobal(IntPtr(tmpEnableComWithATP));
  Marshal::FreeHGlobal(IntPtr(tmpPosErrorOffset));
  Marshal::FreeHGlobal(IntPtr(tmpSiteId));
  Marshal::FreeHGlobal(IntPtr(tmpReceiverId));
  Marshal::FreeHGlobal(IntPtr(tmpSenderId));
  Marshal::FreeHGlobal(IntPtr(tmpLocoId));
  Marshal::FreeHGlobal(IntPtr(tmpMajorVersion));
  Marshal::FreeHGlobal(IntPtr(tmpMinorVersion));
}

/***********************************************************************************************************
* Function:     SaveTrackParamsToIniFile
* Description:  Saves the current TracksParams in the Tracks data-file.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::SaveTrackParamsToIniFile()
{
  // Save Tracks data-file.
  char *tmpObrdSimfileName = (char *)Marshal::StringToHGlobalAnsi(iniObrdSimFileName).ToPointer();

  // Clear old values
  WritePrivateProfileStringA("TrackList", NULL, NULL, tmpObrdSimfileName);

  // Save as many vehicles that are defined
  for (int i = 0; i < mdlNumberOfTracks; i++)
  {
    String^ trackIdParam("TrackId_");
    String^ trackLengthParam("Length_");

    trackIdParam += (i + 1);
    trackLengthParam += (i + 1);

    // Parameter-names
    char *tmpTrackIdParam = (char *)Marshal::StringToHGlobalAnsi(trackIdParam).ToPointer();
    char *tmpTrackLengthParam = (char *)Marshal::StringToHGlobalAnsi(trackLengthParam).ToPointer();

    // Values
    char *tmpTrackIdValue = (char *)Marshal::StringToHGlobalAnsi(" " + mdlTrackId[i].ToString()).ToPointer();
    char *tmpTrackLengthValue = (char *)Marshal::StringToHGlobalAnsi(" " + (mdlTrackLength[i]).ToString()).ToPointer();

    WritePrivateProfileStringA("TrackList", tmpTrackIdParam, tmpTrackIdValue, tmpObrdSimfileName);
    WritePrivateProfileStringA("TrackList", tmpTrackLengthParam, tmpTrackLengthValue, tmpObrdSimfileName);

    Marshal::FreeHGlobal(IntPtr(tmpTrackIdParam));
    Marshal::FreeHGlobal(IntPtr(tmpTrackLengthParam));
    Marshal::FreeHGlobal(IntPtr(tmpTrackIdValue));
    Marshal::FreeHGlobal(IntPtr(tmpTrackLengthValue));
  }
}

/***********************************************************************************************************
* Function:     Tick
* Description:  Tick shall be called each cycle to execute the logic of the OBRD simulator.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::Tick()
{
  RunOBRDSimulation();
}

/***********************************************************************************************************
* Function:     UpdateSafetyParams
* Description:  Updates all safety parameters in communication layer.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::UpdateSafetyParams()
{
  atpOBRDCom->UpdateSafetyParams(mdlSiteId, mdlReceiverId, mdlSenderId, mdlLocoId);
}

/***********************************************************************************************************
* Function:     UpdatePath
* Description:  Replaces the path used for rear-position calculations.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::UpdatePath(array<unsigned short> ^path)
{
  if (mdlEnableComWithATP)
  {
    for (int i = 0; i < pathArraySize; i++)
    {
      currentPath[i].trackId = path[i];

      try
      {
        // Get the track-length for a certain track from the model.
        currentPath[i].length = mdlTrackLength[Array::IndexOf(mdlTrackId, currentPath[i].trackId)];
      }
      catch (...)
      {
        // Missing tracks in track-ist compared to path
        pathFault = true;
      }
    }

    // The path is received and valid to be used for calculations
    pathPosValid = true;
  }
}

/***********************************************************************************************************
* Function:     GetMissingTrackFault
* Description:  Gets a missing track fault
***********************************************************************************************************/
bool OBRDSimDLL::OBRDSimulation::GetMissingTrackFault(void)
{
  bool currentPathFault = pathFault;

  // Reset Path fault
  pathFault = false;

  return currentPathFault;
}

/***********************************************************************************************************
* Function:     UpdateAOSStatus
* Description:  Updates a new position with time-stamp, loco-direction and if loco is leading.
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::UpdateAOSStatus(unsigned short trackId, unsigned long position, unsigned long long timeStamp, bool locoTowardsLeg1, bool locoLeading)
{

  this->locoLeading = locoLeading;

  if (currentPosIndex == (recentFrontPositions->Length - 1))
  {
    currentPosIndex = 0;

    // The array is full and valid to be used for calculations
    frontPosValid = true;
  }
  else
  {
    ++currentPosIndex;
  }

  recentFrontPositions[currentPosIndex].trackId = trackId;
  recentFrontPositions[currentPosIndex].position = position;
  recentFrontPositions[currentPosIndex].timeStamp = timeStamp;

  Byte pathIndex = 0;

  if (pathPosValid)
  {
    // Find pathIndex to know which track to update with proper loco-orientation.
    while (currentPath[pathIndex].trackId != trackId)
    {
      ++pathIndex;
    }

    currentPath[pathIndex].locoTowardsLeg1 = locoTowardsLeg1;
  }

  // Indicates that a new position has arrived this cycle
  frontPosUpdated = true;
}

/***********************************************************************************************************
* Function:     UpdateBrakePressure
* Description:  Updates the brake pressure to send in the Unit Status message
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::UpdateBrakePressure(int brakePressureInKpaIn)
{
  static const int maxLastCarBrakePressure = 1250;

  const int brakePressureInKpa = min(maxLastCarBrakePressure, brakePressureInKpaIn);

  currLastCarBrakePressure = (brakePressureInKpa * 145) / 1000; // Convert kpa to psig
}

/***********************************************************************************************************
* Function:     RunOBRDSimulation
* Description:  Process the OBRD simulation
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::RunOBRDSimulation()
{
  time_t currentTime;
  time(&currentTime);
  bool injectCRCError = false;

  // Order to Disconnect?
  if (!mdlEnableComWithATP && atpConnected)
  {
    atpOBRDCom->Disconnect();
  }

  atpConnected = atpOBRDCom->Connected();

  // Disable communication with ATP?
  if (!atpConnected)
  {
    currOBRDState = OBRDStateNotConnected;
  }

  // Error-injection CRC-error
  if (FICRCFailure == mdlFaultInjection)
  {
    injectCRCError = true;
  }

  // State-machine to simulate OBRD-Solution
  switch (currOBRDState)
  {
    case OBRDStateNotConnected:
    {
      // Clear the Last sent Debug data
      ResetRealtimeData();

      // Enable communication with AOS? --> Make a new retry every reConnectTimeout seconds.
      if (mdlEnableComWithATP && (currentTime >= previousTime + reConnectTimeout))
      {
        atpOBRDCom->Connect();

        previousTime = currentTime;

        currOBRDState = OBRDStateConnectToAtp;
      }
    }
      break;

    case OBRDStateConnectToAtp:
    {    

      // Connected ? -> Perform ProtocolVerification
      if (atpConnected)
      {
        atpOBRDCom->SendProtocolVersion(mdlMajorVersion, mdlMinorVersion, injectCRCError);

        unsigned short guiIdx = 0;

        guiProtocolToATP[guiIdx++] = Convert::ToString(mdlMajorVersion);
        guiProtocolToATP[guiIdx++] = Convert::ToString(mdlMinorVersion);

        currOBRDState = OBRDStateProtocolVerification;
      }
      // Retry to connect every second
      else if (currentTime != previousTime)
      {
        currOBRDState = OBRDStateNotConnected;
      }
    }
      break;

    case OBRDStateProtocolVerification:
    {
      unsigned short receivedRnid = 0;

      if (atpOBRDCom->ReceiveData(receivedRnid))
      {

        // Check which message that was received
        switch (receivedRnid)
        {
          case RNID_PROTOCOL_PACKET:
          {
            Byte tmpMajor = 0;
            Byte tmpMinor = 0;

            if (atpOBRDCom->ReceiveProtocolVersion(tmpMajor, tmpMinor))
            {
              unsigned short guiIdx = 0;

              guiProtocolFromATP[guiIdx++] = Convert::ToString(tmpMajor);
              guiProtocolFromATP[guiIdx++] = Convert::ToString(tmpMinor);

              // Start the normal state by sending out the first status immediately.
              previousTime = 0;
              currOBRDState = OBRDStateNormal;
            }
          }
          break;
        
          case RNID_REJECT_PACKET:
          {
            Byte tmpReject = 0;
            Byte tmpMajor = 0;
            Byte tmpMinor = 0;

            if (atpOBRDCom->ReceiveRejection(tmpReject, tmpMajor, tmpMinor))
            {
              unsigned short guiIdx = 0;

              guiRejectFromATP[guiIdx++] = Convert::ToString(tmpReject);
              guiRejectFromATP[guiIdx++] = Convert::ToString(tmpMajor);
              guiRejectFromATP[guiIdx++] = Convert::ToString(tmpMinor);

              // Error-injection to ignore a message-rejection from AOS, i e go to Normal-state anyway.
              if (FIIgnoreRejection == mdlFaultInjection)
              {
                // Start the normal state by sending out the first status immediately.
                previousTime = 0;
                currOBRDState = OBRDStateNormal;
              }
              else
              {
                atpOBRDCom->Disconnect();
                currOBRDState = OBRDStateNotConnected;
              }
            }
          }
          break;

          default:
            break;
        }
      }
      // Timeout after reConnectTimeout seconds, and try to re-connect/re-send protocol-version.
      else if ((currentTime >= (previousTime + reConnectTimeout)))
      {
        previousTime = currentTime;

        currOBRDState = OBRDStateConnectToAtp;
      }
    }
      break;

    case OBRDStateNormal:
    {
      posItem currentRearPosition = { 0, 0, 0 };
      Byte brakePressureToSend;

      if ((mdlSimMode == SimulationVSim) || (mdlSimMode == SimulationEmd))
      {
        brakePressureToSend = mdlVSIMLastCarBP;
      }
      else if (mdlFaultInjection == FIOverrideBrakePressure)
      {
        brakePressureToSend = mdlOverrideLastCarBP;
      }
      else
      {
        // Change the brake pressure in steps rather than abruptly
        int diff = (int) (char) (currLastCarBrakePressure - previousLastCarBrakePressure);
        int step = diff / 7;
        if (step == 0)
        {
          if (diff > 0)
          {
            step = 1;
          }
          else if (diff < 0)
          {
            step = -1;
          }
        }

        brakePressureToSend = previousLastCarBrakePressure + step;
        previousLastCarBrakePressure = brakePressureToSend;
      }

      currentRearPosition.timeStamp = getUTCTime();

      // Calculate & Send status report with a specified periodic interval. 
      // Sync when a new front-position has arrived ( every 0.5 sec) to get accurate values.
      if ((currentTime >= (previousTime + mdlStatusReportPeriodicity)) && ((frontPosValid && frontPosUpdated) || !frontPosValid))
      {

        // Is data valid for rear-position calculation?
        if (pathPosValid && frontPosValid)
        {
          // Do the actual rear-end calculation.
          CalculateTrainRearPos(currentRearPosition);

          // Error-injection to keep old rear-position (but it must exist a proper previousRearPosition)
          // Update only the time-stamp
          if ((FIFreezePOS == mdlFaultInjection) && (previousRearPosition.trackId != 0))
          {
            currentRearPosition.trackId = previousRearPosition.trackId;
            currentRearPosition.position = previousRearPosition.position;
          }
          // Error-injection to simulate faulty GPS reading, track is set to 0.
          else if(FIFailedToReadGPS == mdlFaultInjection)
          {
            currentRearPosition.trackId = 0;
          }
        }

        // Send Status Report to AOS
        atpOBRDCom->SendStatusReport(currentRearPosition, brakePressureToSend, injectCRCError);

        unsigned short guiIdx = 0;

        guiStatusToATP[guiIdx++] = Convert::ToString(currentRearPosition.trackId);
        guiStatusToATP[guiIdx++] = Convert::ToString(static_cast<unsigned int>(currentRearPosition.position));
        guiStatusToATP[guiIdx++] = Convert::ToString(currentRearPosition.timeStamp);
        guiStatusToATP[guiIdx++] = Convert::ToString(brakePressureToSend);

        previousRearPosition = currentRearPosition;
        previousTime = currentTime;
      }
    }
      break;

    default:
      break;
  }

  // Reset flag for front-position updated
  frontPosUpdated = false;
}
  
/***********************************************************************************************************
* Function:     CalculateTrainRearPos
* Description:  Calculate the Train Rear position from:
*               Track,Track-length, Path, Front-train Pos, Train-length and DelayTime 
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::CalculateTrainRearPos(posItem &rearPosition)
{
  Byte posIndex = currentPosIndex;
  Byte pathIndex = 0;

  unsigned long long frontPosTimeStamp = 0;
  unsigned long remainingTrainLength = mdlTrainLength * 100; // In cm

  posItem currentFrontPos;
  
  // Error-injection to simulate +/- offset for rear-end calculation
  if (FIPosErrorOffset == mdlFaultInjection)
  {
    // The error-offset shall not make the train-length negative.
    if ((static_cast<signed long long>(remainingTrainLength) + (mdlPosErrorOffset * 100)) < 0)
    {
      // Set to 1 cm to be valid for further calculations
      remainingTrainLength = 1;
    }
    else
    {
      remainingTrainLength += (mdlPosErrorOffset * 100);
    }
  }

  // Sanity check, shouldn't happen. Pre-condition for this calculation is that the recentFrontPositions-array is full.
  if (recentFrontPositions[posIndex].trackId != 0)
  {
    // Find Front Position by using the delay (in steps of 500ms) to calculate the proper index where position is stored. 
    currentFrontPos.trackId = recentFrontPositions[(frontPosArraySize + posIndex - mdlTimestampDelay) % frontPosArraySize].trackId;
    currentFrontPos.position = recentFrontPositions[(frontPosArraySize + posIndex - mdlTimestampDelay) % frontPosArraySize].position;
    currentFrontPos.timeStamp = recentFrontPositions[(frontPosArraySize + posIndex - mdlTimestampDelay) % frontPosArraySize].timeStamp;

    // Find proper pathIndex to know which track was last passed.
    while (currentPath[pathIndex].trackId != currentFrontPos.trackId)
    {
      ++pathIndex;
    }

    // If loco is leading the TIMS shall report the calculated rear position.
    if (locoLeading)
    {   
      // See if train length fits within current track, otherwise look at previous track.
      while (remainingTrainLength > 0)
      {
        // Calculate the length of track that is behind the train.
        unsigned long trackLengthPassed = currentPath[pathIndex].locoTowardsLeg1 ? currentFrontPos.position : currentPath[pathIndex].length - currentFrontPos.position;

        // If the remaining length of the train fits within this track, then the calculation is finished.
        if ((static_cast<signed long long>(trackLengthPassed) - remainingTrainLength) >= 0)
        {
          // Finished
          rearPosition.trackId = currentFrontPos.trackId;

          // Add or subtract from position depends on the loco-orientation
          rearPosition.position = currentPath[pathIndex].locoTowardsLeg1 ? currentFrontPos.position - remainingTrainLength : currentFrontPos.position + remainingTrainLength;
          rearPosition.timeStamp = currentFrontPos.timeStamp;

          remainingTrainLength = 0;
        }
        // Needs to go back to previous track to get the rear-position.
        else
        {
          remainingTrainLength -= trackLengthPassed;

          // Rear end is further back in path 
          if (pathIndex != 0)
          {
            pathIndex -= 1;
          }
          else
          {
            // Error, Rear-position is outside tracks in path.
          }

          // Set trackId and max position for last passed track
          currentFrontPos.trackId = currentPath[pathIndex].trackId;
          currentFrontPos.position = currentPath[pathIndex].locoTowardsLeg1 ? currentPath[pathIndex].length : 0U;
        }
      }
    }
    // If Loco is reversing, the front-position (with proper delay) will be reported to AOS
    else
    {
      rearPosition = currentFrontPos;
    }
  }
}

/***********************************************************************************************************
* Function:     ResetRealtimeData
* Description:  Reset all temporary data
*
***********************************************************************************************************/
void OBRDSimDLL::OBRDSimulation::ResetRealtimeData()
{
  atpConnected = false;
  frontPosUpdated = false;
  frontPosValid = false;
  pathPosValid = false;
  
  // Start-state
  currOBRDState = OBRDStateNotConnected;

  locoLeading = true;

  previousRearPosition.trackId = 0;
  previousRearPosition.position = 0;

  currentPosIndex = 0U;

  for (int i = 0; i < frontPosArraySize; i++)
  {
    recentFrontPositions[currentPosIndex].trackId = 0;
    recentFrontPositions[currentPosIndex].position = 0;
    recentFrontPositions[currentPosIndex].timeStamp = 0;
  }

  guiProtocolToATP[0] = "";
  guiProtocolToATP[1] = "";

  guiProtocolFromATP[0] = "";
  guiProtocolFromATP[1] = "";

  guiRejectFromATP[0] = "";
  guiRejectFromATP[1] = "";
  guiRejectFromATP[2] = "";

  guiStatusToATP[0] = "";
  guiStatusToATP[1] = "";
  guiStatusToATP[2] = "";
  guiStatusToATP[3] = "";
}





#pragma once
#include "StdAfx.h"
#include "LCSSimDLL.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LCSSimDLL.cpp-4.1.4:c++:arn_006#2 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LCSSimDLL.cpp %
*
*  %version:       4.1.4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
*
*  DESCRIPTION:
*   
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-10-26    Antbäck     File created
* 2013-12-03    Antbäck     Set ATOMode only at stand still
*                           Update usage of PI controller and I value
* 2013-03-27    Antbäck     Added panto simulation from file
* 2014-03-31    Hidaji      Fixed a bug in sending sndAlarmImpactRedOp2
* 2014-07-03    Hidaji      Changed roof => Center to make it uniform with site data representation
* 2014-12-18    Antbäck     Reworked handling of LowBatt, added handling for CurrentStatus byte
* 2015-01-29    Antbäck     Corrected handling of driver intervention
* 2017-02-27    Marlundg    Adapted for BHP Project
*
*******************************************************************************/

#include "emp_message.hpp"

// Defines
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#define min(a,b)            (((a) < (b)) ? (a) : (b))

/******************************************************************************
* Function:     InitSimulatedValues
* Description:  Initializes the values that are simulated
******************************************************************************/
void LCSSimDLL::LCSSimulation::InitSimulatedValues()
{
    currATODrivingMode = ATODrivingModeNotAsserted;
    currFreeRollingStatus = NormalMode;
    currBlueFlagStatus = BlueFlagStatusInactive;
    currLCSATOReady = LCSATONotReady;
    currECPBSequenceStatus = ECPBSequenceStatusUnknown;
    currReadyForPrecisionStop = ReadyForPrecisionStopNoAction;
    currBrakeSystemInUse = BrakeSystemInUseECPB;
    currECPBOperatingMode = ECPBOperatingModeRun;

    trainConfigScanCounter = 0;
}


/*****************************************************************************************
* Function:     InitNonSimulatedValues
* Description:  Initializes the values that are NOT simulated (i e only manually changed)
******************************************************************************************/
void LCSSimDLL::LCSSimulation::InitNonSimulatedValues(char *iniFile)
{

    currATOModeCabinSelectorStatus = ATOModeCSSUndefined;
    currBlueFlagRequest = BlueFlagNotRequested;
    currADSETA = 0;
    currADSETAStatus = AdsEtaStatusNotDefined;
    currHandlingDone = HandlingDoneUndefined;
    currTrainIntegrityStatusECPB = TrainIntegrityStatusECPBConfirmed;
    currPercentageOfOpBrakesECPB = GetPrivateProfileIntA("LCSSim", "PercentageOfOpBrakesECPB", 95, iniFile);
    currLastCarBrakePressure = GetPrivateProfileIntA("LCSSim", "LastCarBrakePressure", 0, iniFile);

    // GPS-Pos
    
    //Front GPS Position
    currGPSPosLocoFrnt = gcnew array<unsigned short>(bytesForGPSFrontPos);
    for (int i = 0; i < bytesForGPSFrontPos; i++)
    {
      currGPSPosLocoFrnt[i] = 0;
    }

    //Rear GPS Position
    currGPSPosLastCarUnit = gcnew array<unsigned short>(bytesForGPSFrontPos);
    for (int i = 0; i < bytesForGPSFrontPos; i++)
    {
      currGPSPosLastCarUnit[i] = 0;
    }

    currVersionADSMap = 0x0101;
    currLocoSysFault = 0x0000;
        

    //Path
    currTrackIds = gcnew array<UInt16>(maxPathTracks);
    currTrackIDSpeedChange = gcnew array<UInt16>(maxSpeedChange);
    currPosInTrackSpeedChange = gcnew array<signed long>(maxSpeedChange);
    currNewSpeedSpeedChange = gcnew array<unsigned char>(maxSpeedChange);

    currNumTracks = 0;
    for (int i = 1; i < maxPathTracks; i++)
    {
      currTrackIds[i] = 0;
    }

    currSpeedBeginPath = 0;
    currNumSpeedChanges = 0;
    for (int i = 1; i < maxSpeedChange; i++)
    {
      currTrackIDSpeedChange[i] = 0;
      currPosInTrackSpeedChange[i] = 0;
      currNewSpeedSpeedChange[i] = 0;
    }

    currNextTragetTrackId = 0;
    currNextTargetPos = 0;
    currReqTOANextTarget = 0;

    //ATP Command
    currECPBTrainCompReq = NotAsserted;
    currHoldingBrake = NotAsserted;
    currTrainWeight = 0;

    //Movement Authority
    currEndOfMATrkId = 0;
    currEndOfMAPos = 0;
    currMADirection = Undefined;
    currSpeed = 0;
    currMAMargin = 0;
}


/******************************************************************************
* Function:     InitModules
* Description:  
******************************************************************************/
void LCSSimDLL::LCSSimulation::InitModules(void)
{
    atpCom->Init();

    /* TODO BHP: Reuse communication code with LocoSim and VSim when needed
   
    if (useLocoSim)
    {
        lsCom->Init();
    }
    else if (useVSIM)
    {
        vsimCom->Init();
    }
    */
}

/***********************************************************************************************************
* Function:     SaveTrainCompToIniFile
* Description:  Saves the applied ECPBTrainComposition values to the [LCSTrainComp] chapter in the ini-file.
***********************************************************************************************************/
void LCSSimDLL::LCSSimulation::SaveTrainCompToIniFile()
{
  char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
  char *tmpCurrVehDetectedPosUnknown = (char *)Marshal::StringToHGlobalAnsi(" " + currVehDetectedPosUnknown.ToString()).ToPointer();
  char *tmpCurrVehNotDetected = (char *)Marshal::StringToHGlobalAnsi(" " + currVehNotDetected.ToString()).ToPointer();

  // Clear old values
  WritePrivateProfileStringA("LCSTrainComp", NULL, NULL, tmpIniFile);

  WritePrivateProfileStringA("LCSTrainComp", "DetectedByECPPosUnknown", tmpCurrVehDetectedPosUnknown, tmpIniFile);
  WritePrivateProfileStringA("LCSTrainComp", "NotDetectedByECP", tmpCurrVehNotDetected, tmpIniFile);

  // Save as many vehicles that are defined
  for (int i = 0; i < currNumberOfVehicles; i++)
  {
    String^ roadNumberParam("RoadNumber_");
    String^ vehicleTypeParam("VehicleType_");

    roadNumberParam += (i + 1);
    vehicleTypeParam += (i + 1);

    // Parameter-names
    char *tmpRoadNumberParam = (char *)Marshal::StringToHGlobalAnsi(roadNumberParam).ToPointer();
    char *tmpVehicleTypeParam = (char *)Marshal::StringToHGlobalAnsi(vehicleTypeParam).ToPointer();
    
    // Values
    char *tmpCurrRoadNumber = (char *)Marshal::StringToHGlobalAnsi(" " + currRoadNumber[i].ToString()).ToPointer();
    char *tmpCurrVehicleType = (char *)Marshal::StringToHGlobalAnsi(" " + ((unsigned short)(currVehicleType[i])).ToString()).ToPointer();

    WritePrivateProfileStringA("LCSTrainComp", tmpRoadNumberParam, tmpCurrRoadNumber, tmpIniFile);
    WritePrivateProfileStringA("LCSTrainComp", tmpVehicleTypeParam, tmpCurrVehicleType, tmpIniFile);
  }
}

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LCSSimDLL::LCSSimulation::Tick(void)
{
    CallATPCom();

    // Initialize simulation values if just connected
    if (!atpConnected && atpCom->atpConnected && simulationEnabled)
    {
      InitSimulatedValues();
    }

    atpConnected = atpCom->atpConnected;

    /* TODO BHP: Reuse communication code with LocoSim and VSim when needed

    if (useLocoSim)
    {
        CallLSCom();
        locoSimConnected = lsCom->LSConnected;
    }
    else
    {
        locoSimConnected = false;
    }

    if (useVSIM)
    {
        CallVSIMCom();
        vsimConnected = vsimCom->VSIMConnected;
    }
    else
    {
        vsimConnected = VSIMCon_Undef;
    }
    */

    // Execute one sample of LCS
    RunLCSSimulation();
}


/******************************************************************************
* Function:     RunLCSSimulation
* Description:
******************************************************************************/
void LCSSimDLL::LCSSimulation::RunLCSSimulation(void)
{
    // Check if simulation is just turned on, if so, re-initialize the simulated values again.
    if (!oldSimulationEnabled && simulationEnabled)
    {
      InitSimulatedValues();
    }

    oldSimulationEnabled = simulationEnabled;
    
    // State-machine to simulate LCS behavior - depending on incoming message
    switch (currLCSState)
    {

    // Normal state
    case LCSStateNormal:

        if (atpCom->recMessageTypesFromATP->find(LCSMessageTypeFromATPAOSStatus) != atpCom->recMessageTypesFromATP->end())
        {
            // Receiving an AOS-Status message shall result in a Train-Status message from LCS.
            atpCom->lcsMessageTypesToATP->insert(LCSMessageTypeToATPTrainStatus);

            // Simulate certain values
            if (simulationEnabled)
            {
              // Reflect the BlueFlag status from AOS
              currBlueFlagStatus = (BlueFlagNotActive == atpCom->recBlueFlag) ? BlueFlagStatusInactive : BlueFlagStatusActivated;
            }

            // If the OBRD-simulator is attached -> Update Front-position
            if (nullptr != theObrdSim)
            {
              theObrdSim->UpdateAOSStatus(
                atpCom->recTrackIdFrontOfTrain,
                atpCom->recPositionOnTrackFrontOfTrain,
                atpCom->recSystemTime,
                atpCom->recTrainOrientationOnFrontTrack == TrainOrientationLeadLocoToLeg1 ? true : false,
                atpCom->recTravelDirection == TravelDirectionLocoLeading ? true : false );
            }
        }

        if (atpCom->recMessageTypesFromATP->find(LCSMessageTypeFromATPATPCommand) != atpCom->recMessageTypesFromATP->end())
        {
            // Simulate certain values
            if (simulationEnabled)
            {
                // Check if Train Composition is requested
                if (Requested == atpCom->recvECPBTrainCompReq)
                {
                    if (ECPBSequenceStatusUnknown == currECPBSequenceStatus)
                    {
                        // Simulate that ECPB is scanning the configuration
                        currECPBSequenceStatus = ECPBSequenceStatusScanning;
                    } 
                    else if (ECPBSequenceStatusConfigKnown == currECPBSequenceStatus)
                    {
                        // Next Message to send is ECPB Train Composition Message
                        atpCom->lcsMessageTypesToATP->insert(LCSMessageTypeToATPTrainComposition);
                    }
                }
    

                // Update the Train Weight from AOS 
                if (atpCom->recvTrainWeight != 0)
                {
                  currEstTrainWeightTons = atpCom->recvTrainWeight;
                }
 
            }
        }

        if (atpCom->recMessageTypesFromATP->find(LCSMessageTypeFromATPPath) != atpCom->recMessageTypesFromATP->end())
        {
          // If the OBRD-simulator is attached -> Update Path
          if (nullptr != theObrdSim)
          {
            theObrdSim->UpdatePath(atpCom->recvTrackIds);
          }
        }

        if (atpCom->recMessageTypesFromATP->find(LCSMessageTypeFromATPRclInformation) != atpCom->recMessageTypesFromATP->end())
        {
          // Receiving an RCL information message shall result in a RCL Status message from LCS.
          // and RCL status message shall be sent in immediately after each Train status message it is contolled by ATP.
          atpCom->lcsMessageTypesToATP->insert(LCSMessageTypeToATPRclStatus);

          // Simulate certain values
          if (simulationEnabled)
          {
            // Check if Aos Operational Mode is undefined.
            if (AosOperationalModeUndefined == atpCom->recvAosOperationalMode)
            {
              // Simulate that Handling Done is Undefined.
              currHandlingDone = HandlingDoneUndefined;
            }
          }
        }

        // Simulation Processing

        // Set ConfigKnown after a certain time if started Scanning
        if (simulationEnabled && (ECPBSequenceStatusScanning == currECPBSequenceStatus))
        {
            if (cyclesToScan > trainConfigScanCounter)
            {
                trainConfigScanCounter++;
            }
            else
            {
                // TrainsStatus - ECPB Sequence Status = Configuration Known
                currECPBSequenceStatus = ECPBSequenceStatusConfigKnown;

                // Next Message to send is ECPB Train Composition Message
                atpCom->lcsMessageTypesToATP->insert(LCSMessageTypeToATPTrainComposition);
            }
        }

        break;

    default:
        break;
    }
}
/******************************************************************************
* Function:     CallATPCom
* Description:  
******************************************************************************/
void LCSSimDLL::LCSSimulation::CallATPCom(void)
{
    // Forward com link flag
    atpCom->atpLinkEnabled = comLinksEnabled;

    atpCom->sndATOModeCabinSelectorStatus = currATOModeCabinSelectorStatus;
    atpCom->sndATODrivingMode = currATODrivingMode;
    atpCom->sndFreeRollingStatus = currFreeRollingStatus;
    atpCom->sndBlueFlagStatus = currBlueFlagStatus;
    atpCom->sndBlueFlagRequest = currBlueFlagRequest;
    atpCom->sndADSETAStatus = currADSETAStatus;
    atpCom->sndADSETA = currADSETA;
    atpCom->sndLCSATOReady = currLCSATOReady;
    atpCom->sndECPBSequenceStatus = currECPBSequenceStatus;
    atpCom->sndReadyForPrecisionStop = currReadyForPrecisionStop;
    atpCom->sndBrakeSystemInUse = currBrakeSystemInUse;
    atpCom->sndECPBOperatingMode = currECPBOperatingMode;
    atpCom->sndTrainIntegrityStatusECPB = currTrainIntegrityStatusECPB;
    atpCom->sndPercentageOpBreaksECPB = currPercentageOfOpBrakesECPB;
    atpCom->sndLastCarBrakePressure = currLastCarBrakePressure;
    // GPS-Pos

    //Front GPS Position
    for (int i = 0; i < bytesForGPSFrontPos; i++)
    {
      atpCom->sndGPSPosLocoFrnt[i] = currGPSPosLocoFrnt[i];
    }

    //Rear GPS Position
    for (int i = 0; i < bytesForGPSFrontPos; i++)
    {
      atpCom->sndGPSPosLastCarUnit[i] = currGPSPosLastCarUnit[i];
    }

    atpCom->sndVersionADSMap = currVersionADSMap;
    atpCom->sndLocoPenaltyBrakeActive = currLocoPenaltyBrkActive;
    atpCom->sndLocoSysFault = currLocoSysFault;
    atpCom->sndAutoDrivingSystem = currADSBits;

    //ECPB Train Composition
    atpCom->sndNumberOfVehicles = currNumberOfVehicles;
    atpCom->sndVehDetectedPosUnknown = currVehDetectedPosUnknown;
    atpCom->sndVehNotDetected = currVehNotDetected;
    for (int i = 0; i < atpCom->sndNumberOfVehicles; i++)
    {
      atpCom->sndRoadNumber[i] = currRoadNumber[i];
      atpCom->sndVehicleType[i] = currVehicleType[i];
    }

    atpCom->sndHandlingDone = currHandlingDone;
    // Call one "tick"
    atpCom->Tick();

    // TODO BHP: Received data to simulation, ie copy from atpCom to LCSSim (some parts can also be handled in simulation logic)

}

/******************************************************************************
* Function:     CallLSCom
* Description:  
******************************************************************************/
void LCSSimDLL::LCSSimulation::CallLSCom(void)
{
    // Forward com link flag
    lsCom->locosimLinkEnabled = comLinksEnabled;

    // TODO BHP: Setup data for LS communication when needed
    // eg lsCom->sndDirection    = currDrivDirection;

    // Call one "tick"
    lsCom->Tick();

    // TODO BHP: Copy Received data to simulation when needed
    // eg currSpeed = lsCom->recSpeed;

    simATOSwitchPos         = lsCom->recATOSwitchPos;
    simATOSwitchPosValid    = lsCom->recATOSwitchPosValid;
    simSpeed                = lsCom->recSpeed;
    simSpeedValid           = lsCom->recSpeedValid;
    simBrakesApplied        = lsCom->recBrakesApplied;
    simBrakesAppliedValid   = lsCom->recBrakesAppliedValid;
}

/******************************************************************************
* Function:     CallVSIMCom
* Description:  
******************************************************************************/
void LCSSimDLL::LCSSimulation::CallVSIMCom(void)
{
    // Forward com link flag
    // Not used here !!

    // TODO BHP: Setup data for LS communication when needed
    // eg vsimCom->sndDirection    = currDrivDirection;


    // Call one "tick"
    vsimCom->Tick();

    // TODO BHP: Copy Received data to simulation when needed
    // eg currSpeed = vsimCom->recSpeed;

    simATOSwitchPos         = vsimCom->recATOSwitchPos;
    simATOSwitchPosValid    = vsimCom->recATOSwitchPosValid;
    simSpeed                = vsimCom->recSpeed;
    simSpeedValid           = vsimCom->recSpeedValid;
    simBrakesApplied        = vsimCom->recBrakesApplied;
    simBrakesAppliedValid   = vsimCom->recBrakesAppliedValid;
}




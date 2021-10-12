#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LCSSimDLL.h %
*
*  %version:       9 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
*
*  DESCRIPTION: 
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
* 2013-11-26    Bo H        Read atoPort from ini-file
* 2013-12-03    Antbäck     Corrected initialisation of PI controller
* 2014-03-13    Antbäck     Added DLL version
* 2014-03-17    Hidaji      Added sendToATOInter
* 2014-03-27    Antbäck     Added panto simulation from file
* 2014-04-03    Antbäck     Added comLinksEnabled
* 2014-12-18    Antbäck     Added handling for CurrentStatus byte
* 2015-01-29    Antbäck     Corrected handling of driver intervention
* 2015-02-26    Antbäck     Publish piControl
* 2017-02-27    Marlundg    Adapted for BHP Project
*
*******************************************************************************/

#include "stdafx.h"
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

#include <msclr\marshal.h>

#include "LCSSimConsts.h"
#include "ATPCom.h"
#include "LocoSimCom.h"
#include "VSIMCom.h"

namespace LCSSimDLL {
     
    public ref class TrackListItem
    {
    public:
        int                 Number;
        String^             Name;
        int                 Length;     // [m]
    };

    public ref class LCSSimulation
    {
    public:

        // Enable communication links flag
        bool  comLinksEnabled;

        // Simulation of values enabled
        bool  simulationEnabled;
        bool  oldSimulationEnabled;

        // ************************************************************
        // TODO BHP: Fill up with Simulation BHP/EMD variables

        // Current modes/states
        LCSStateEnum    currLCSState;

        // End of simulation variables
        // ************************************************************


        // ************************************************************
        // Input data to simulation

        // TODO BHP: Fill up data to simulation BHP/EMD variables
        ATOModeCabinSelectorStatusEnum  currATOModeCabinSelectorStatus;
        ATODrivingModeEnum              currATODrivingMode;
        FreeRollingStatusEnum           currFreeRollingStatus;
        BlueFlagStatusEnum              currBlueFlagStatus;
        BlueFlagRequestEnum             currBlueFlagRequest;
        AdsEtaStatusEnum                currADSETAStatus;
        unsigned int                    currADSETA;
        LCSATOReadyEnum                 currLCSATOReady;
        ECPBSequenceStatusEnum          currECPBSequenceStatus;
        ReadyForPrecisionStopEnum       currReadyForPrecisionStop;
        BrakeSystemInUseEnum            currBrakeSystemInUse;
        ECPBOperatingModeEnum           currECPBOperatingMode;
        TrainIntegrityStatusECPBEnum    currTrainIntegrityStatusECPB;
        unsigned char                   currPercentageOfOpBrakesECPB;
        unsigned char                   currLastCarBrakePressure;

        //Front GPS Position
        array<unsigned short>^ currGPSPosLocoFrnt;

        //Rear GPS Position
        array<unsigned short>^ currGPSPosLastCarUnit;

        unsigned short                  currVersionADSMap;
        unsigned int                    currEstTrainWeightTons;
        LocoPenaltyBrkActiveEnum        currLocoPenaltyBrkActive;
        unsigned int                    currLocoSysFault;
        unsigned int                    currADSBits;

        // ECPB Train Composition Simulated variables
        unsigned short                  currNumberOfVehicles;
        unsigned short                  currVehDetectedPosUnknown;
        unsigned short                  currVehNotDetected;
        array<unsigned short>^          currRoadNumber;
        array<VehicleTypeEnum>^         currVehicleType;
        
        // ATP Command Simulation variables
        CommandMessageEnum              currECPBTrainCompReq;
        CommandMessageEnum              currHoldingBrake;
        unsigned long                   currTrainWeight;
        
        //MA Simulation variables
        unsigned short                  currEndOfMATrkId;
        signed long                     currEndOfMAPos;
        MADirectionEnum                 currMADirection;
        unsigned short                  currSpeed;
        unsigned short                  currMAMargin;
        
        //Path Simulation variables
        unsigned short                  currNumTracks;
        array<UInt16>^                  currTrackIds;
        unsigned char                   currSpeedBeginPath;
        unsigned char                   currNumSpeedChanges;
        array<UInt16>^                  currTrackIDSpeedChange;
        array<signed long>^             currPosInTrackSpeedChange;
        array<unsigned char>^           currNewSpeedSpeedChange;
        unsigned short                  currNextTragetTrackId;
        signed long                     currNextTargetPos;
        unsigned long                   currReqTOANextTarget;

       // Location Handling 
        HandlingDoneEnum          currHandlingDone;

        // End of input data to simulation
        // ************************************************************

        // ************************************************************
        // Internal variables available for GUI
        bool                atpConnected;
        bool                locoSimConnected;
        VSIMComStatusEnum   vsimConnected;

        // Components available only for "debug" purposes
        ATPCom^     atpCom;
        LocoSimCom^ lsCom;
        VSIMCom^    vsimCom;
        bool        useLocoSim;
        bool        useVSIM;

        // DLLVersion
        String^     DLLVersion;

        // End of internal variables available for GUI
        // ************************************************************

        /******************************************************************************
        * Function:     Constructor
        * Description:  
        ******************************************************************************/
        LCSSimulation(String^ fileName, OBRDSimDLL::OBRDSimulation^ obrdSim)
        {
            // Store incoming fields
            iniFileName = fileName;

            theObrdSim = obrdSim;

            // Read ini-file data
            char            tmpStr[1000];
            char            *tmpIniFile = (char *) Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
                        
            // LocoSim
            useLocoSim  = GetPrivateProfileIntA("LCSSim", "UseLocoSim", 1, tmpIniFile) != 0 ? true : false;
            GetPrivateProfileStringA("LCSSim", "LocoSimIP", "127.0.0.1", tmpStr, sizeof(tmpStr), tmpIniFile);
            locoSimIP   = Marshal::PtrToStringAnsi((IntPtr)tmpStr);
            locoSimPort = GetPrivateProfileIntA("LCSSim", "LocoSimPort", 55190, tmpIniFile);

            // ATP
            lcsPort = GetPrivateProfileIntA("LCSSim", "LCSPort", 30150, tmpIniFile);

            // VSIM
            useVSIM     = GetPrivateProfileIntA("LCSSim", "UseVSIM", 0, tmpIniFile) != 0 ? true : false;
            GetPrivateProfileStringA("LCSSim", "VSIMIP", "127.0.0.1", tmpStr, sizeof(tmpStr), tmpIniFile);
            vsimIP      = Marshal::PtrToStringAnsi((IntPtr)tmpStr);
            vsimSpdPort = GetPrivateProfileIntA("LCSSim", "VSIMSpdPort", 40207, tmpIniFile);
            vsimCmdPort = GetPrivateProfileIntA("LCSSim", "VSIMCmdPort", 40208, tmpIniFile);
            
            // LCSTrainComp
            currRoadNumber = gcnew array<unsigned short>(maxVehicles);
            currVehicleType = gcnew array<VehicleTypeEnum>(maxVehicles);

            currVehDetectedPosUnknown = GetPrivateProfileIntA("LCSTrainComp", "DetectedByECPPosUnknown", 0, tmpIniFile);
            currVehNotDetected = GetPrivateProfileIntA("LCSTrainComp", "NotDetectedByECP", 0, tmpIniFile);
            
            currNumberOfVehicles = 0;
            bool endOfVehicles = false;

            for (int i = 0; (i < maxVehicles) && (!endOfVehicles); i++)
            {             
              String^ roadNumber("RoadNumber_");
              String^ vehicleType("VehicleType_");
              
              roadNumber += (i + 1);
              vehicleType += (i + 1);

              // Parameter-names
              char *tmpRoadNumberParam = (char *)Marshal::StringToHGlobalAnsi(roadNumber).ToPointer();
              char *tmpVehicleTypeParam = (char *)Marshal::StringToHGlobalAnsi(vehicleType).ToPointer();

              // Values
              unsigned short tmpRoadNumber = GetPrivateProfileIntA("LCSTrainComp", tmpRoadNumberParam, 0, tmpIniFile);
              unsigned short tmpVehicleType = GetPrivateProfileIntA("LCSTrainComp", tmpVehicleTypeParam, VehicleTypeNotDefined, tmpIniFile);

              // If no 'VehicleTypeXX' was found then stop to parse 'RoadNumberXX' and 'VehicleTypeXX'
              if (tmpVehicleType != VehicleTypeNotDefined)
              {
                ++currNumberOfVehicles;

                currRoadNumber[i] = tmpRoadNumber;

                switch (tmpVehicleType)
                {
                case 0:
                  currVehicleType[i] = VehicleTypeUnknown;
                  break;
                case 1:
                  currVehicleType[i] = VehicleTypeLocomotive;
                  break;
                case 2:
                  currVehicleType[i] = VehicleTypeCar;
                  break;
                default:
                  currVehicleType[i] = VehicleTypeUnknown;
                }
              }
              else
              {
                endOfVehicles = true;
              }
            }

            atpCom = gcnew ATPCom(lcsPort);
            atpConnected = false;
            if (useLocoSim)
            {
                lsCom = gcnew LocoSimCom(locoSimIP, locoSimPort);
                locoSimConnected = false;
            }
            else if (useVSIM)
            {
                vsimCom = gcnew VSIMCom(vsimIP, vsimSpdPort, vsimCmdPort);
                vsimConnected = vsimCom->VSIMConnected;
            }

            // Init all simulated and non simulated values
            InitNonSimulatedValues(tmpIniFile);
            InitSimulatedValues();

            comLinksEnabled = true;
            simulationEnabled = true;
            oldSimulationEnabled = true;

            // Get DLL version
            DLLVersion = " - v" +
                Assembly::GetExecutingAssembly()->GetName()->Version->Major.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Minor.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Build.ToString();
        }

        void InitNonSimulatedValues(char *iniFile);
        void InitSimulatedValues();
        void InitModules(void);

        void SaveTrainCompToIniFile();
        
        void Tick(void);

    private:
        // Ini-file settings
        String^         locoSimIP;
        int             locoSimPort;
        int             lcsPort;
        String^         vsimIP;
        int             vsimSpdPort;
        int             vsimCmdPort;
        String^         iniFileName;
       
        // Simulation input, to handle LocoSim vs VSIM
        ATOModeEnum     simATOSwitchPos;
        bool            simATOSwitchPosValid;
        int             simSpeed;           // [mm/s]
        bool            simSpeedValid;
        bool            simBrakesApplied;
        bool            simBrakesAppliedValid;

        unsigned short    trainConfigScanCounter;

        // Reference to the OBRDSimulation (if provided in constructor)
        OBRDSimDLL::OBRDSimulation^ theObrdSim;

        // Internal functions
        void RunLCSSimulation(void);
        void CallATPCom(void);
        void CallLSCom(void);
        void CallVSIMCom(void);
    };
}


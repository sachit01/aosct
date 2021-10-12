#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoIO.h %
*
*  %version:       8 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-13 15:06 %
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
* 2013-04-21    Antbäck     File created
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-04-10    Hidaji      Added driveDirInSpeed
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    Penaltybreak changed to Servicebreak in GUI text
* 2017-03-10    Marlundg    Support for new I/O
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Net;
using namespace System::Net::Sockets;

#include "LocoConsts.h"
#include "LocoParams.h"

#include "VIOHSimConnection.h"
#include "CODSimConnection.h"
#include "OPCSimConnection.h"

namespace LocoSimDLL {

    /// Summary for LocoIO
    public ref class LocoIO 
    {
    public:

        // Inputs to LocoIO (from LocoSim)
        bool AOSEB1;
        bool AOSEB2;
        bool AOSEmerBrakeActive;
        bool AOSPenaltyBreak;
        bool AOSTCO;
        bool AOSATPOk;
        bool AOSBuzzer;
        bool AOSLamp;
        bool AOSPowerOff;
        bool ATPReady;
        bool regBaliseIDSent;

        // Received I/Os from AOS
        array<bool>^    receivedOutputValuesA;
        array<bool>^    receivedOutputValuesB;
       
        // I/O Values/Health to be sent to AOS
        array<bool>^    inputValuesToSend;
        array<bool>^    inputHealthToSend;
        array<Int16>^   analogInputValuesToSend;

        array<String^>^ outputNames;

        EnumDriveDir    driveDirInSpeed;

        int inputAutoBPvalueInKpa;
         
        // Constructor
        // ===========
        LocoIO(LocoParams^ lP, OBRDSimDLL::OBRDSimulation^ obrdSim)
        {
            int i;

            // Keep pointer to parameters
            locoParams = lP;

            theObrdSim = obrdSim;

            // Create dynamic objects
            receivedOutputValuesA = gcnew array<bool>(RECEIVED_OUTPUT_SIZE);
            receivedOutputValuesB = gcnew array<bool>(RECEIVED_OUTPUT_SIZE);

            inputValuesToSend = gcnew array<bool>(INPUT_SIZE);
            inputHealthToSend = gcnew array<bool>(INPUT_SIZE);
            analogInputValuesToSend = gcnew array<Int16>(ANALOG_INPUT_SIZE);

            outputNames = gcnew array<String^>(RECEIVED_OUTPUT_SIZE);

            viohSimConnection = gcnew VIOHSimConnection();
            codSimConnection = gcnew CODSimConnection();
            opcSimConnection = gcnew OPCSimConnection();

            // Initialize values
            AOSEB1 = false;
            AOSEB2 = false;
            AOSEmerBrakeActive = false;
            AOSPenaltyBreak = false;
            AOSTCO = false;
            AOSATPOk = false;
            AOSBuzzer = false;
            AOSLamp = false;
            AOSPowerOff = false;

            driveDirInSpeed = DDForward;

            for (i = 0; i < receivedOutputValuesA->Length; i++)
            {
                // Create strings for GUI
                outputNames[i] = (i + 1).ToString() + ": ";
                if ((i + 1) == lP->mapOutputEB1)
                {
                    outputNames[i] += "Emergency Brake 1";
                }
                else if ((i + 1) == lP->mapOutputEB2)
                {
                    outputNames[i] += "Emergency Brake 2";
                }
                else if ((i + 1) == lP->mapOutputEmerBrakeActive)
                {
                    outputNames[i] += "Emergency Brake Active";
                }
                else if ((i + 1) == lP->mapOutputPenaltyBreak)
                {
                    outputNames[i] += "Service Break";
                }
                else if ((i + 1) == lP->mapOutputTCO)
                {
                    outputNames[i] += "Traction Cut Off";
                }
                else if ((i + 1) == lP->mapOutputATPOk)
                {
                    outputNames[i] += "ATP OK";
                }
                else if ((i + 1) == lP->mapOutputBuzzer)
                {
                    outputNames[i] += "Buzzer";
                }
                else if ((i + 1) == lP->mapOutputLamp)
                {
                    outputNames[i] += "Lamp";
                }
                else if ((i + 1) == lP->mapOutputPowerOff)
                {
                    outputNames[i] += "Power Off";
                }
                else 
                {
                    outputNames[i] += "-";
                }

                // Set default value on signal
                receivedOutputValuesA[i] = false;
            }

            // Variables
            inputIsolationA = false;
            inputIsolationB = false;
            inputDriveDir       = DDUndef;
            inputATOSwitch      = ATOSwitchUndef;
            inputCabin1         = false;
            inputCabin2         = false;
            inputLCSReady       = false;
            inputAOSOff         = false;
            inputEmerStopActive = false;
            inputTcoFeedback = false;
            inputNonControlUnit = false;
            inputEmerBrakeCutOut1A = false;
            inputEmerBrakeCutOut1B = false;
            inputEmerBrakeCutOut2A = false;
            inputEmerBrakeCutOut2B = false;
            dxLastSecond        = 0;

            ATPReady = false;
            regBaliseIDSent = false;

            // ATP1 FF
            ATP1ToFFActive  = false;
        }

        void Init(void);
        void Tick(enum TBSystemStatus  systemStatus,
          double          lSpeed,
          double          lAcc,
          EnumDriveDir    lDriveDir,
          EnumATOMODE     lATOSwitch,
          bool            lCabin1,
          bool            lCabin2,
          bool            lLCSReady,
          bool            lAOSOff,
          bool            inputEmerStopActive,
          bool            lATP1ToFFClicked,
          bool            lNcu,
          bool            lEb1a,
          bool            lEb1b,
          bool            lEb2a,
          bool            lEb2b,
          bool            lIsolA,
          bool            lIsolB,
          bool            lRoadM,
          bool            lRailM,
          bool            bpAutomatic,
          bool            bpManual,
          int             bp1valueEntered,
          int             bp2valueEntered,
          bool            lTcoFeedback
        );

        /**********************************************************
        * Function:     clearOutputs
        * Description: When starting the ATP connection, the LocoIO
        * outputs receive buffer should  be cleared
        **********************************************************/
        void clearOutputBuffer(void)
        {
            int i;
            for (i = 0; i < receivedOutputValuesA->Length; i++)
            {
                receivedOutputValuesA[i] = false;
            }
            for (i = 0; i < receivedOutputValuesB->Length; i++)
            {
                receivedOutputValuesB[i] = false;
            }

            // Initialize the read output values
            AOSEB1 = false;
            AOSEB2 = false;
            AOSEmerBrakeActive = false;
            AOSPenaltyBreak = false;
            AOSTCO = false;
            AOSATPOk = false;
            AOSBuzzer = false;
            AOSLamp = false;
            AOSPowerOff = false;

              // Clear ATPReady flag received from CODSim when Reg balise sent to simulated ATP
            ATPReady = false;
            regBaliseIDSent = false;

        }

        void locoSimApplyButtonPressed();
        void SetAOSConnectionActive(bool status);
    protected:

        VIOHSimConnection^  viohSimConnection;
        CODSimConnection^   codSimConnection;
        OPCSimConnection^   opcSimConnection;

        // Last cycle Input variables
        // Inputs to AOS (i.e. generated by LocoSim)
        bool                    inputIsolationA;
        bool                    inputIsolationB;
        EnumDriveDir            inputDriveDir;
        EnumATOMODE             inputATOSwitch;
        bool                    inputCabin1;
        bool                    inputCabin2;
        bool                    inputLCSReady;
        bool                    inputAOSOff;
        bool                    inputEmerStopActive;
        bool                    inputTcoFeedback;
        bool                    inputNonControlUnit;
        bool                    inputEmerBrakeCutOut1A;
        bool                    inputEmerBrakeCutOut1B;
        bool                    inputEmerBrakeCutOut2A;
        bool                    inputEmerBrakeCutOut2B;
        bool                    inputRoadM;
        bool                    inputRailM;
        int                     dxLastSecond;

        ////Brake Pressure tab
        //bool                    inputNoBrake;
        //bool                    inputSBApplied;
        //bool                    inputEBApplied;
        //bool                    inputAutomaticSim;
        //bool                    inputManualSim;
          int                     inputAutoBPvalueToSend;
          int                     inputManualBP1valueToSend;
          int                     inputManualBP2valueToSend;



        // ATP1FF handling
        bool            ATP1ToFFActive;

        //Create only Consoles
        bool            consolesOnly;

        void SetInputValue(Inputs Signal, bool NewState);
        void SetInputHealth(Inputs Signal, bool NewState);

        void SetAnalogInputValue(AnalogInputs Signal, int NewState);

        bool GetOutputValueA(Outputs Signal);
        bool GetOutputValueB(Outputs Signal);

        void SendInputData(
            bool                        forceUpdate,
            bool                        lIsolationA,
            bool                        lIsolationB,
            EnumDriveDir                lDriveDir, 
            EnumATOMODE                 lATOSwitch,
            bool                        lCabin1,
            bool                        lCabin2,
            bool                        lLCSReady,
            bool                        lAOSOff,
            bool                        lEmerStopActive,
            bool                        lNonControlUnit,
            bool                        lEmerBrakeCutOut1A,
            bool                        lEmerBrakeCutOut1B,
            bool                        lEmerBrakeCutOut2A,
            bool                        lEmerBrakeCutOut2B,
            bool                        lTcoFeedback,
            bool                        lRoadM,
            bool                        lRailM,
            bool                        bpAutomatic,
            bool                        bpManual,
            int                         bp1valueEntered,
            int                         bp2valueEntered
            );

        bool ReadOutputData(void);

        void SendSDUData(double lSpeed, EnumDriveDir  lDriveDir, double lAcc);

        bool ReadOPCSimATPReady(void);

        void SendRegBaliseData(void);

        // Destructor
        // ==========
        ~LocoIO()
        {
        }
    private:
        LocoParams^ locoParams;
        bool aosConnectionActive;

        // Reference to the OBRDSimulation (if provided in constructor)
        OBRDSimDLL::OBRDSimulation^ theObrdSim;
    };

}
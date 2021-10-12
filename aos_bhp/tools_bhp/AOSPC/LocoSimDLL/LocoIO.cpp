#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoIO.cpp-9:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoIO.cpp %
*
*  %version:       9 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-24 10:08 %
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
* 2013-04-22    Antbäck     File created
* 2013-07-19    Hidaji      Set the telegram status for VDX to noFailure
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2014-04-10    Hidaji      Added driveDirInSpeed, so that the driving direction changes only if there is speed 
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    AOSPC must send initial message to ATP VIOH when ATP started.
* 2017-03-10    Marlundg    Support for new I/O and Sim-connections other than localhost
* 2020-01-30    Bhermans    Corrected sending of default RegBalise in SIL
*
*******************************************************************************/
static const unsigned char STX = 0x02;
static const unsigned char ETX = 0x01;

static const unsigned char NID_MSG_SIM_OUTPUTS  = 128;
static const unsigned char NID_MSG_SIM_INPUTS   = 1;

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include <timeapi.h>
#include "LocoIO.h"

/******************************************************************************
* Function:     Init
* Description:  
******************************************************************************/
void LocoSimDLL::LocoIO::Init(void)
{
   inputAutoBPvalueToSend = 0;
   inputManualBP1valueToSend = 0;
   inputManualBP2valueToSend = 0;

    // Connect to local-host if in SIL, otherwise to dispatcher.
  IPAddress^ ATP;

    
    if((locoParams->SimMode == SimulationEmd) || (locoParams->SimMode == SimulationVSim))
    {
      consolesOnly = TRUE;
      ATP = IPAddress::Parse(locoParams->DISPIP);
    }
    else if (locoParams->SimMode == SimulationHil)
    {
      ATP = IPAddress::Parse(locoParams->DISPIP);
      consolesOnly = FALSE;
    }
    else
    {
      ATP = IPAddress::Parse("127.0.0.1");
      consolesOnly = FALSE;
    }

    opcSimConnection->Connect(ATP, locoParams->OPCSimPortToConnect);

    if (!consolesOnly)
    {
      viohSimConnection->Connect(ATP, locoParams->VIOHSimPortToConnect);
      codSimConnection->Connect(ATP, locoParams->CODSimPortToConnect);
      

      // Read I/O first
      ReadOutputData();

      // Setup Input data
      SetInputValue(locoParams->mapInputIsolationA, 0);
      SetInputValue(locoParams->mapInputIsolationB, 0);
      SetInputValue(locoParams->mapInputATOManual, 0);
      SetInputValue(locoParams->mapInputATOSupervised, 0);
      SetInputValue(locoParams->mapInputATOAutomatic, 0);
      SetInputValue(locoParams->mapInputCab1, 0);
      SetInputValue(locoParams->mapInputCab2, 0);
      SetInputValue(locoParams->mapInputForward, 0);
      SetInputValue(locoParams->mapInputReverse, 0);
      SetInputValue(locoParams->mapInputLCSReady, 0);
      SetInputValue(locoParams->mapInputEmerStopActive, 0);
      SetInputValue(locoParams->mapInputATPOff, 0);
      SetInputValue(locoParams->mapInputRoadM, 0);
      SetInputValue(locoParams->mapInputRailM, 0);
      SetInputValue(locoParams->mapInputTcoFeedback, 0);
      SetInputValue(locoParams->mapInputNonControlUnit, 0);
      SetInputValue(locoParams->mapInputEmerBrakeCutOut1A, 0);
      SetInputValue(locoParams->mapInputEmerBrakeCutOut1B, 0);
      SetInputValue(locoParams->mapInputEmerBrakeCutOut2A, 0);
      SetInputValue(locoParams->mapInputEmerBrakeCutOut2B, 0);
      //Set default Analog Values
      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor1, 0);
      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor2, 0);

      // Send default input-data
      SendInputData(true, false, false, DDNeutral, ATOSwitchMan, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, 0, 0);

      // Send default movement data
      SendSDUData(0.0, DDNeutral, 0.0);
    }
}


/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::LocoIO::Tick(enum TBSystemStatus  systemStatus,
                  double        lSpeed,
                  double        lAcc,
                  EnumDriveDir  lDriveDir, 
                  EnumATOMODE   lATOSwitch,
                  bool          lCabin1,
                  bool          lCabin2,
                  bool          lLCSReady,
                  bool          lAOSOff,
                  bool          lEmerStopActive,
                  bool          lATP1ToFFClicked,
                  bool          lNcu,
                  bool          lEb1a,
                  bool          lEb1b,
                  bool          lEb2a,
                  bool          lEb2b,
                  bool          lIsolA,
                  bool          lIsolB,
                  bool          lRoadM,
                  bool          lRailM,
                  bool          bpAutomatic,
                  bool          bpManual,
                  int           bp1valueEntered,
                  int           bp2valueEntered,
                  bool          lTcoFeedback
    )
{
    static DWORD lastTimeSendSDUData;
    static DWORD lastTimeSendReciveIOData = 0;
    static bool firstMessageIsReceived = false;

#ifdef MAYBE_TO_BE_ADDED_LATER
    // Toggle FF request to ATP1 when requested
    if (lATP1ToFFClicked)
    {
        ATP1ToFFActive = !ATP1ToFFActive;
    }
    // Clear when system is shutted down again
    if (TBSystemPendingStartUp == systemStatus)
    {
        ATP1ToFFActive = false;
    }
#endif 
    if (!consolesOnly)
    {
      // Time to check console data
      if ((timeGetTime() - lastTimeSendReciveIOData) >= 100)
      {
        lastTimeSendReciveIOData = timeGetTime();

        // Connect if not connected

        // Connect to localhost if in SIL, otherwise to dispatcher.
        IPAddress^ ATP = SimulationSil == locoParams->SimMode ? IPAddress::Parse("127.0.0.1"): IPAddress::Parse(locoParams->DISPIP);

        viohSimConnection->Connect(ATP, locoParams->VIOHSimPortToConnect);
        codSimConnection->Connect(ATP, locoParams->CODSimPortToConnect);


        // Check if new I/O data is available
        if (ReadOutputData())
        {
          // Flag to know if remote knows the IP/port to us.
          // (ATP must receive at least one message to know where to send IO-outputs)
          firstMessageIsReceived = true;
        }

        // Send data if changed (or if ATP is just turned on it needs a startup-message)
        SendInputData(systemStatus == TBSystemRunning && !firstMessageIsReceived,
          lIsolA,
          lIsolB,
          lDriveDir,
          lATOSwitch,
          lCabin1,
          lCabin2,
          lLCSReady,
          lAOSOff,
          lEmerStopActive,
          lNcu,
          lEb1a,
          lEb1b,
          lEb2a,
          lEb2b,
          lTcoFeedback,
          lRoadM,
          lRailM,
          bpAutomatic,
          bpManual,
          bp1valueEntered,
          bp2valueEntered
        );
      }

      // Time to send new movement data?
      if ((timeGetTime() - lastTimeSendSDUData) >= locoParams->MovementTelegramFreq)
      {
        lastTimeSendSDUData = timeGetTime();
        SendSDUData(lSpeed, lDriveDir, lAcc);
      }
    }

    if (systemStatus == TBSystemRunning)
    {
      if (firstMessageIsReceived)
      {
        if (ATPReady)
        {
          if (!regBaliseIDSent)
          {
            // Send the registration balise ID once when the ATP system status turns to running
            SendRegBaliseData();
            regBaliseIDSent = true;
          }
        }
        else
        {
          ATPReady = ReadOPCSimATPReady();
        }
      }
    }
    else
    {
      ATPReady = false;
      regBaliseIDSent = false;

      // Reset the flag that a message is received from ATP.
      firstMessageIsReceived = false;
    }
}

/******************************************************************************
* Function:     locoSimApplyButtonPressed
* Description: Invoked when the Apply/Save button is pressed in the LocoSim-Params-tab
******************************************************************************/
void LocoSimDLL::LocoIO::locoSimApplyButtonPressed()
{
  // Apply button in LocoSim->Params tab is pressed

  // Send the registration balise ID message
  if (locoParams->SimMode != SimulationVSim)
  {
    SendRegBaliseData();
  }
}

/******************************************************************************
* Function:     SetInputValue
* Description:  
******************************************************************************/
void LocoSimDLL::LocoIO::SetInputValue(Inputs Signal, bool NewState)
{
    if ((Signal > 0) && (Signal < InputEnd))
    {
      inputValuesToSend[Signal-1] = NewState;
    }
}


/******************************************************************************
* Function:     SetAnalogInputValue
* Description:
******************************************************************************/
void LocoSimDLL::LocoIO::SetAnalogInputValue(AnalogInputs Signal, int NewState)
{
  if ((Signal > 0) && (Signal < AnalogInputEnd))
  {
    analogInputValuesToSend[Signal - 1] = NewState;
  }
}

/******************************************************************************
* Function:     SetInputHealth
* Description:
******************************************************************************/
void LocoSimDLL::LocoIO::SetInputHealth(Inputs Signal, bool NewState)
{
  if ((Signal > 0) && (Signal < InputEnd))
  {
    inputHealthToSend[Signal - 1] = NewState;
  }
}

/******************************************************************************
* Function:     SetAOSConnectionActive
* Description: Set AOS active flag as seen from the console connections
******************************************************************************/
void LocoSimDLL::LocoIO::SetAOSConnectionActive(bool status)
{
  this->aosConnectionActive = status;
}

/******************************************************************************
* Function:     GetOutputValueA
* Description:  
******************************************************************************/
bool LocoSimDLL::LocoIO::GetOutputValueA(Outputs Signal)
{
    if ((Signal > 0) && (Signal < OutputEnd))
    {
        return receivedOutputValuesA[Signal-1];
    }

    return false;
}

/******************************************************************************
* Function:     GetOutputValueB
* Description:
******************************************************************************/
bool LocoSimDLL::LocoIO::GetOutputValueB(Outputs Signal)
{
  if ((Signal > 0) && (Signal < OutputEnd))
  {
    return receivedOutputValuesB[Signal - 1];
  }

  return false;
}

/******************************************************************************
* Function:     ReadOPCSimATPReady
* Description:
******************************************************************************/
bool LocoSimDLL::LocoIO::ReadOPCSimATPReady(void)
{
  static bool ret = false;

  static DWORD lastCheck = 0;

  if (lastCheck + CHECK_ATP_TIMER < timeGetTime())
  {
    ret = opcSimConnection->ReadATPReady();
    if (!ret)
    {
      // Send the RegBaliseData until received ready signal from ATP 
      SendRegBaliseData();
    }
    lastCheck = timeGetTime();
  }

  return ret;
}

/******************************************************************************
* Function:     ReadOutputData
* Description:
******************************************************************************/
bool LocoSimDLL::LocoIO::ReadOutputData(void)
{
  if (viohSimConnection->ReadAOSOutputIO(receivedOutputValuesA, receivedOutputValuesB))
  {
    if ((locoParams->mapOutputEB1 > 0) &&
      (locoParams->mapOutputEB1 <= RECEIVED_OUTPUT_SIZE))
    {
      AOSEB1 = receivedOutputValuesA[locoParams->mapOutputEB1 - 1];
    }

    if ((locoParams->mapOutputEB2 > 0) &&
      (locoParams->mapOutputEB2 <= RECEIVED_OUTPUT_SIZE))
    {
      AOSEB2 = receivedOutputValuesA[locoParams->mapOutputEB2 - 1];
    }

    if ((locoParams->mapOutputEmerBrakeActive > 0) &&
      (locoParams->mapOutputEmerBrakeActive <= RECEIVED_OUTPUT_SIZE))
    {
      AOSEmerBrakeActive = receivedOutputValuesA[locoParams->mapOutputEmerBrakeActive - 1];
    }

    if ((locoParams->mapOutputPenaltyBreak > 0) &&
      (locoParams->mapOutputPenaltyBreak <= RECEIVED_OUTPUT_SIZE))
    {
      AOSPenaltyBreak = receivedOutputValuesA[locoParams->mapOutputPenaltyBreak - 1];
    }

    if ((locoParams->mapOutputTCO > 0) &&
      (locoParams->mapOutputTCO <= RECEIVED_OUTPUT_SIZE))
    {
      AOSTCO = receivedOutputValuesA[locoParams->mapOutputTCO - 1];
    }

    if ((locoParams->mapOutputATPOk > 0) &&
      (locoParams->mapOutputATPOk <= RECEIVED_OUTPUT_SIZE))
    {
      AOSATPOk = receivedOutputValuesA[locoParams->mapOutputATPOk - 1];
    }

    if ((locoParams->mapOutputBuzzer > 0) &&
      (locoParams->mapOutputBuzzer <= RECEIVED_OUTPUT_SIZE))
    {
      AOSBuzzer = receivedOutputValuesA[locoParams->mapOutputBuzzer - 1];
    }

    if ((locoParams->mapOutputLamp > 0) &&
      (locoParams->mapOutputLamp <= RECEIVED_OUTPUT_SIZE))
    {
      AOSLamp = receivedOutputValuesA[locoParams->mapOutputLamp - 1];
    }

    if ((locoParams->mapOutputPowerOff > 0) &&
      (locoParams->mapOutputPowerOff <= RECEIVED_OUTPUT_SIZE))
    {
      AOSPowerOff = receivedOutputValuesA[locoParams->mapOutputPowerOff - 1];
    }

    // 
    return true;
  }
  else
  {
      // Not able to receive anything
      return false;
  }
}


/******************************************************************************
* Function:     SendInputData
* Description:
******************************************************************************/
void LocoSimDLL::LocoIO::SendInputData(
        bool                            forceUpdate,
        bool                            lIsolationA,
        bool                            lIsolationB,
        EnumDriveDir                    lDriveDir,
        EnumATOMODE                     lATOSwitch,
        bool                            lCabin1,
        bool                            lCabin2,
        bool                            lLCSReady,
        bool                            lAOSOff,
        bool                            lEmerStopActive,
        bool                            lNonControlUnit,
        bool                            lEmerBrakeCutOut1A,
        bool                            lEmerBrakeCutOut1B,
        bool                            lEmerBrakeCutOut2A,
        bool                            lEmerBrakeCutOut2B,
        bool                            lTcoFeedback,
        bool                            lRoadM,
        bool                            lRailM,
        bool                            bpAutomatic,
        bool                            bpManual,
        int                             bp1valueEntered,
        int                             bp2valueEntered
)
{
    bool writeToAos = false;
    //Raw value 819 corresponds with 4 mA for a 4-20 mA sensor 
    int minRawValue = 819;
    //Raw value 4095 corresponds with 20 mA for a 4-20 mA sensor 
    int maxRawValue = 4095;

    int minRangeBrakePressure = locoParams->minimumBrakePressureRange;
    int maxRangeBrakePressure = locoParams->maximumBrakePressureRange;


    int RangeofBrakePressure = (maxRangeBrakePressure - minRangeBrakePressure);

    //handle the rate of pressure decrease for every 100ms
    int ebPressureDecreaseper100ms = (locoParams->ebBrakePressureDecreaseRate) / 10;
    int sbPressureDecreaseper100ms = (locoParams->sbBrakePressureDecreaseRate) / 10;
    int pressureIncrease100ms = (locoParams->pressureIncreaseRate) / 10;
 
    if (bpAutomatic && (!bpManual))
    {
      
      //Check for EB
      if (!(receivedOutputValuesA[locoParams->mapOutputEB1 - 1]) || !(receivedOutputValuesA[locoParams->mapOutputEB2 - 1]))
      {
        //calculate the EB value
        int finalBPvalueInKpa = (((locoParams->noBrakeAppliedPressure) * (locoParams->emergencyBrakeAppliedPercentage)) / 100);
        if (inputAutoBPvalueInKpa > finalBPvalueInKpa)
        {
          inputAutoBPvalueInKpa -= ebPressureDecreaseper100ms;
          // Pressure cannot drop below the final brake pressure
          if (inputAutoBPvalueInKpa < finalBPvalueInKpa)
          {
            inputAutoBPvalueInKpa = inputAutoBPvalueInKpa;
          }
        }
        else
        {
            //DO Nothing
        }
      }
      //Check for SB
      else if (!(receivedOutputValuesA[locoParams->mapOutputPenaltyBreak - 1]))
      {
        //Calculate the SB value
        int finalBPvalueInKpa = (((locoParams->noBrakeAppliedPressure) * (locoParams->serviceBrakeAppliedPercentage)) / 100);
        if (inputAutoBPvalueInKpa > finalBPvalueInKpa)
        {
          inputAutoBPvalueInKpa -= sbPressureDecreaseper100ms;
          // Pressure cannot drop below the final brake pressure
          if (inputAutoBPvalueInKpa < finalBPvalueInKpa)
          {
            inputAutoBPvalueInKpa = inputAutoBPvalueInKpa;
          }
        }
        else
        {
          //DO Nothing
        } 
      }
      else
      {
        //Calculate the No brake Value
        if (inputAutoBPvalueInKpa < locoParams->noBrakeAppliedPressure)
        {
          inputAutoBPvalueInKpa += pressureIncrease100ms;
          // Pressure cannot increase above no brake pressure applied value
          if (inputAutoBPvalueInKpa > locoParams->noBrakeAppliedPressure)
          {
            inputAutoBPvalueInKpa = inputAutoBPvalueInKpa;
          }
        }
        else
        {
          //Nothing
        }
      }

      //Raw value to send to AOS in range of 0 - 4095 
      inputAutoBPvalueToSend = minRawValue + (maxRawValue - minRawValue) * (inputAutoBPvalueInKpa - minRangeBrakePressure) / RangeofBrakePressure;

      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor1, inputAutoBPvalueToSend);
      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor2, inputAutoBPvalueToSend);

      // If the OBRD-simulator is attached -> Update brake pressure
      if (nullptr != theObrdSim)
      {
        theObrdSim->UpdateBrakePressure(inputAutoBPvalueInKpa - minRangeBrakePressure);
      }

      writeToAos = true;
    }
    else
    {
      if (bpManual)
      { 
        //convert the simulated 0-1379 kPa value to 0-4095 Analog Value
        int tempSensor1Value = 0;
        int tempSensor2Value = 0;

        //Brake pressure Sensor 1
        if (bp1valueEntered <= maxRangeBrakePressure)
        {
          tempSensor1Value = bp1valueEntered;
        }
        else
        {
          tempSensor1Value = maxRangeBrakePressure;
        }

        //Brake pressure Sensor 2
        if (bp2valueEntered <= maxRangeBrakePressure)
        {
          tempSensor2Value = bp2valueEntered;
        }
        else
        {
          tempSensor2Value = maxRangeBrakePressure;
        }
        
        //Input value
        inputManualBP1valueToSend = minRawValue + (maxRawValue - minRawValue) * (tempSensor1Value - minRangeBrakePressure) / RangeofBrakePressure;
        //Input value
        inputManualBP2valueToSend = minRawValue + (maxRawValue - minRawValue) * (tempSensor2Value - minRangeBrakePressure) / RangeofBrakePressure;

        // If the OBRD-simulator is attached -> Update brake pressure
        if (nullptr != theObrdSim)
        {
          theObrdSim->UpdateBrakePressure(tempSensor1Value - minRangeBrakePressure);
        }
      }
      else
      {
        inputManualBP1valueToSend = maxRawValue;
        inputManualBP2valueToSend = maxRawValue;
      }
      
      writeToAos = true;
      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor1, inputManualBP1valueToSend);
      SetAnalogInputValue(locoParams->mapInputBrakePressureSensor2, inputManualBP2valueToSend);
    }


    // Updating the inputValuesToSend from GUI

    // Cabin1
    if (inputCabin1 != lCabin1)
    {
        inputCabin1 = lCabin1;
        writeToAos = true;

        SetInputValue(locoParams->mapInputCab1, lCabin1);
    }

    // Cabin2
    if (inputCabin2 != lCabin2)
    {
      inputCabin2 = lCabin2;
      writeToAos = true;

      SetInputValue(locoParams->mapInputCab2, lCabin2);
    }

    // LCSReady
    if (inputLCSReady != lLCSReady)
    {
        inputLCSReady = lLCSReady;
        writeToAos = true;

        SetInputValue(locoParams->mapInputLCSReady, lLCSReady);
    }

    // ATPOff
    if (inputAOSOff != lAOSOff)
    {
      inputAOSOff = lAOSOff;
      writeToAos = true;

      SetInputValue(locoParams->mapInputATPOff, lAOSOff);
    }

    // DriverEB
    if (inputEmerStopActive != lEmerStopActive)
    {
        inputEmerStopActive = lEmerStopActive;
        writeToAos = true;

        SetInputValue(locoParams->mapInputEmerStopActive, lEmerStopActive);
    }

    // lDriveDir
    if (inputDriveDir != lDriveDir)
    {
        inputDriveDir = lDriveDir;
        writeToAos = true;

        switch(lDriveDir)
        {
        case DDForward:
            SetInputValue(locoParams->mapInputForward, true);
            SetInputValue(locoParams->mapInputReverse, false);
            break;
        case DDReverse:
            SetInputValue(locoParams->mapInputForward, false);
            SetInputValue(locoParams->mapInputReverse, true);
            break;
        case DDUndef:
        case DDNeutral:
        default:
            SetInputValue(locoParams->mapInputForward, false);
            SetInputValue(locoParams->mapInputReverse, false);
            break;
        }
    }

    // ATOSwitch
    if (inputATOSwitch != lATOSwitch)
    {
        inputATOSwitch = lATOSwitch;
        writeToAos = true;

        switch(lATOSwitch)
        {
        case ATOSwitchAuto:
            SetInputValue(locoParams->mapInputATOAutomatic, true);
            SetInputValue(locoParams->mapInputATOManual, false);
            SetInputValue(locoParams->mapInputATOSupervised, false);
            break;

        case ATOSwitchMan:
            SetInputValue(locoParams->mapInputATOAutomatic, false);
            SetInputValue(locoParams->mapInputATOManual, true);
            SetInputValue(locoParams->mapInputATOSupervised, false);
            break;

        case ATOSwitchSupv:
            SetInputValue(locoParams->mapInputATOAutomatic, false);
            SetInputValue(locoParams->mapInputATOManual, false);
            SetInputValue(locoParams->mapInputATOSupervised, true);
            break;

        case ATOSwitchUndef:
            break;

        default:
            SetInputValue(locoParams->mapInputATOAutomatic, false);
            SetInputValue(locoParams->mapInputATOManual, false);
            SetInputValue(locoParams->mapInputATOSupervised, false);
            break;
        }
    }

    // TCO Feedback
    if (inputTcoFeedback != lTcoFeedback)
    {
        inputTcoFeedback = lTcoFeedback;
        writeToAos = true;

        SetInputValue(locoParams->mapInputTcoFeedback, lTcoFeedback);
    }

    // NonControlUnit
    if (inputNonControlUnit != lNonControlUnit)
    {
        inputNonControlUnit = lNonControlUnit;
        writeToAos = true;

        SetInputValue(locoParams->mapInputNonControlUnit, lNonControlUnit);
    }

    // EmerBrakeCutOut1A
    if (inputEmerBrakeCutOut1A != lEmerBrakeCutOut1A)
    {
        inputEmerBrakeCutOut1A = lEmerBrakeCutOut1A;
        writeToAos = true;

        SetInputValue(locoParams->mapInputEmerBrakeCutOut1A, lEmerBrakeCutOut1A);
    }

    // EmerBrakeCutOut1B
    if (inputEmerBrakeCutOut1B != lEmerBrakeCutOut1B)
    {
        inputEmerBrakeCutOut1B = lEmerBrakeCutOut1B;
        writeToAos = true;

        SetInputValue(locoParams->mapInputEmerBrakeCutOut1B, lEmerBrakeCutOut1B);
    }

    // EmerBrakeCutOut2A
    if (inputEmerBrakeCutOut2A != lEmerBrakeCutOut2A)
    {
        inputEmerBrakeCutOut2A = lEmerBrakeCutOut2A;
        writeToAos = true;

        SetInputValue(locoParams->mapInputEmerBrakeCutOut2A, lEmerBrakeCutOut2A);
    }

    // EmerBrakeCutOut2B
    if (inputEmerBrakeCutOut2B != lEmerBrakeCutOut2B)
    {
        inputEmerBrakeCutOut2B = lEmerBrakeCutOut2B;
        writeToAos = true;

        SetInputValue(locoParams->mapInputEmerBrakeCutOut2B, lEmerBrakeCutOut2B);
    }

    // Isolation Switch inputs
    if (inputIsolationA != lIsolationA)
    {
      inputIsolationA = lIsolationA;
      writeToAos = true;

      SetInputValue(locoParams->mapInputIsolationA, lIsolationA);
    }
    if (inputIsolationB != lIsolationB)
    {
      inputIsolationB = lIsolationB;
      writeToAos = true;

      SetInputValue(locoParams->mapInputIsolationB, lIsolationB);
    }

    // RoadM
    if (inputRoadM != lRoadM)
    {
      inputRoadM = lRoadM;
      writeToAos = true;

      SetInputValue(locoParams->mapInputRoadM, lRoadM);
    }

    // RailM
    if (inputRailM != lRailM)
    {
      inputRailM = lRailM;
      writeToAos = true;

      SetInputValue(locoParams->mapInputRailM, lRailM);
    }

#ifdef MAYBE_TO_BE_ADDED_LATER
    // Handle self test toggle bit
    // Invert every second
    System::DateTime now = System::DateTime::Now;
    if (now.Second != dxLastSecond) 
    {
        dxLastSecond = now.Second;
        writeToDx = true;

        if (inputDataFromDxA.selfTestToggleBit == 0)
            inputDataFromDxA.selfTestToggleBit = 1;
        else
            inputDataFromDxA.selfTestToggleBit = 0;

        /* To simulate FatalFailure in ATP1, set transmissionvalid for dx input data to false */        
        inputDataFromDxA.transmissionValid = ATP1ToFFActive ? false : true;
    }
#endif

    // Anything changed since last time? -> Send to AOS
    if (writeToAos || forceUpdate)
    {
        viohSimConnection->WriteAOSInputIO(inputValuesToSend, inputHealthToSend, analogInputValuesToSend);
    }
}



/******************************************************************************
* Function:     SendSDUData
* Description:  Prepare and send speed data to ATP
*               
******************************************************************************/
void LocoSimDLL::LocoIO::SendSDUData(double lSpeed, EnumDriveDir lDriveDir, double lAcc)
{    
    // A reverse driving direction will cause a negative speed 
    short speed = DDReverse == lDriveDir ? static_cast<short>(-lSpeed) : static_cast<unsigned short>(lSpeed);

    // Convert from cm/s2 to 0.01 cm/s2
    short acc = static_cast<unsigned short>(lAcc) * 100;

    // Convert from % to 0.01 %
    unsigned short sensorMin = static_cast<unsigned short>(locoParams->SensorMinError) * 100;

    // Convert from % to 0.01 %
    unsigned short sensorMax = static_cast<unsigned short>(locoParams->SensorMaxError) * 100;

    codSimConnection->WriteAOSMovement(speed, acc, sensorMin, sensorMax);
}

/******************************************************************************
* Function:     SendRegBaliseData
* Description:  Prepare and send registration balise ID
*
******************************************************************************/
void LocoSimDLL::LocoIO::SendRegBaliseData(void)
{
// Connect to localhost if in SIL, otherwise to dispatcher.
  IPAddress^ IpATP = SimulationSil == locoParams->SimMode ? IPAddress::Parse("127.0.0.1") : IPAddress::Parse(locoParams->DISPIP);

  opcSimConnection->Connect(IpATP, locoParams->OPCSimPortToConnect);
  opcSimConnection->WriteAOSRegBaliseID(locoParams->RegDefaultBaliseId);
}


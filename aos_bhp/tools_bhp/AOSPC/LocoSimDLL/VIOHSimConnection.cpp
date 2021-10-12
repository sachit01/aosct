#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  VIOHSimConnection.cpp-3:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          VIOHSimConnection.cpp %
*
*  %version:       3 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-10 15:30 %
*
*  DESCRIPTION:	Implementation of concrete class to create and handle communication with 
*               VIOHSim according to AOS Simulator-Interface Interflo 150.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-04    Marlundg    File created
*
*******************************************************************************/
#include "VIOHSimConnection.h"

static const unsigned char NID_MSG_SIM_OUTPUTS = 128;

static const unsigned char NID_MSG_SIM_INPUTS                           = 1;
static const unsigned char NID_MSG_SIM_INPUTS_SIZE                      = 25;
static const unsigned char NID_MSG_SIM_INPUTS_NID_MESSAGE_TYPE_OFFSET   = 0;
static const unsigned char NID_MSG_SIM_INPUTS_B_INPUT_VALUES_OFFSET     = 1;
static const unsigned char NID_MSG_SIM_INPUTS_B_VIU_HEALTHSTATE_OFFSET  = 5;
static const unsigned char NID_MSG_SIM_INPUTS_B_ANALOG_INPUT_VALUES_OFFSET  = 9;


/******************************************************************************
* Function:     ReadAOSOutputIO
* Description:
******************************************************************************/
bool LocoSimDLL::VIOHSimConnection::ReadAOSOutputIO(array<bool>^ valuesA, array<bool>^ valuesB)
{
    bool retVal = ReadData();

    if (retVal)
    {
        System::Array::ConstrainedCopy(outputValuesA, 0, valuesA, 0, RECEIVED_OUTPUT_SIZE);
        System::Array::ConstrainedCopy(outputValuesB, 0, valuesB, 0, RECEIVED_OUTPUT_SIZE);
    }
    else
    {
        retVal = false;
    }

    return retVal;
}

/******************************************************************************
* Function:     WriteAOSInputIO
* Description:
******************************************************************************/
bool LocoSimDLL::VIOHSimConnection::WriteAOSInputIO(array<bool>^ values, array<bool>^ health, array<Int16>^ analogvalues)
{
    array<Byte>^ dataBuffer;

    dataBuffer = gcnew array<Byte>(NID_MSG_SIM_INPUTS_SIZE);

    int nrOfValues = values->Length;
    int nrOfHealth = health->Length;
    int nrofAnalogvalues = analogvalues->Length;

    dataBuffer[NID_MSG_SIM_INPUTS_NID_MESSAGE_TYPE_OFFSET] = NID_MSG_SIM_INPUTS;

    // Calculate correct bit to set in bitmask for values
    for (int i = 0; i < nrOfValues; i++)
    {
        dataBuffer[NID_MSG_SIM_INPUTS_B_INPUT_VALUES_OFFSET + ((31 - i) / 8)] |= values[i] ? 1 << i % 8 : 0;
    }

    // Calculate correct bit to set in bitmask for health
    for (int i = 0; i < nrOfHealth; i++)
    {
        dataBuffer[NID_MSG_SIM_INPUTS_B_VIU_HEALTHSTATE_OFFSET + ((31 - i) / 8)] |= health[i] ? 1 << i % 8 : 0;
    }

    // Calculate the analog Inputs value and fill the data buffer
    for (int i = 0; i < nrofAnalogvalues; i++)
    {
      dataBuffer[NID_MSG_SIM_INPUTS_B_ANALOG_INPUT_VALUES_OFFSET + 2*i] |= (analogvalues[i] >> 8) & 0xFF;
      dataBuffer[NID_MSG_SIM_INPUTS_B_ANALOG_INPUT_VALUES_OFFSET + (2*i+1)] |= (analogvalues[i]) & 0xFF;
    }

    // Add link-layer and send message on socket
    return (SendData(NID_MSG_SIM_INPUTS_SIZE, dataBuffer));
}

/******************************************************************************
* Function:     InterpretApplicationData
* Description:
******************************************************************************/
bool LocoSimDLL::VIOHSimConnection::InterpretApplicationData(array<Byte>^ buffer, unsigned int size)
{
    unsigned int i = 0;
    bool retVal = true;
    WORD tmpOutput = 0;

    // Create storage for incoming data if not already done
    if (nullptr == outputValuesA)
    {
        outputValuesA = gcnew array<bool>(RECEIVED_OUTPUT_SIZE);
    }

    if (nullptr == outputValuesB)
    {
        outputValuesB = gcnew array<bool>(RECEIVED_OUTPUT_SIZE);
    }

    // Parse & store information from received messages
    switch (buffer[i++])
    {

    case NID_MSG_SIM_OUTPUTS:

        tmpOutput += buffer[i++] << 8;
        tmpOutput += buffer[i++];

        for (int i = 0; i < RECEIVED_OUTPUT_SIZE; i++)
        {
            outputValuesA[i] = ((tmpOutput & (1 << i)) != 0);
        }

        tmpOutput = 0;

        tmpOutput += buffer[i++] << 8;
        tmpOutput += buffer[i++];

        for (int i = 0; i < RECEIVED_OUTPUT_SIZE; i++)
        {
            outputValuesB[i] = ((tmpOutput & (1 << i)) != 0);
        }

        break;

    default:
        retVal = false;
        break;
    }

    return retVal;
}

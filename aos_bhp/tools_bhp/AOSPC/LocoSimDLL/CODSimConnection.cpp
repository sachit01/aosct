#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  CODSimConnection.cpp-2:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          CODSimConnection.cpp %
*
*  %version:       2 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2016-10-18 15:26 %
*
*  DESCRIPTION:	Implementation of concrete class to create and handle communication with
*			    CODSim according to AOS Simulator-Interface Interflo 150.
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
#include "CODSimConnection.h"

static const unsigned char NID_MSG_SIM_MOVEMENT                           = 10;
static const unsigned char NID_MSG_SIM_MOVEMENT_SIZE                      = 9;

static const unsigned char NID_MSG_SIM_MOVEMENT_NID_MESSAGE_TYPE_OFFSET   = 0;
static const unsigned char NID_MSG_SIM_MOVEMENT_V_SIM_OFFSET              = 1;
static const unsigned char NID_MSG_SIM_MOVEMENT_A_SIM_OFFSET              = 3;
static const unsigned char NID_MSG_SIM_MOVEMENT_N_SENSOR_MIN_ERROR_OFFSET = 5;
static const unsigned char NID_MSG_SIM_MOVEMENT_N_SENSOR_MAX_ERROR_OFFSET = 7;


/******************************************************************************
* Function:     WriteAOSMovement
* Description:
******************************************************************************/
bool LocoSimDLL::CODSimConnection::WriteAOSMovement(short speed, short acc, unsigned short sensorMin, unsigned short sensorMax)
{
    array<Byte>^ dataBuffer;

    dataBuffer = gcnew array<Byte>(NID_MSG_SIM_MOVEMENT_SIZE);

    dataBuffer[NID_MSG_SIM_MOVEMENT_NID_MESSAGE_TYPE_OFFSET] = NID_MSG_SIM_MOVEMENT;

    dataBuffer[NID_MSG_SIM_MOVEMENT_V_SIM_OFFSET] = (htons(speed) & 0xff);
    dataBuffer[NID_MSG_SIM_MOVEMENT_V_SIM_OFFSET + 1] = (htons(speed) & 0xff00) >> 8;

    dataBuffer[NID_MSG_SIM_MOVEMENT_A_SIM_OFFSET] = (htons(acc) & 0xff);
    dataBuffer[NID_MSG_SIM_MOVEMENT_A_SIM_OFFSET + 1] = (htons(acc) & 0xff00) >> 8;

    dataBuffer[NID_MSG_SIM_MOVEMENT_N_SENSOR_MIN_ERROR_OFFSET] = (htons(sensorMin) & 0xff);
    dataBuffer[NID_MSG_SIM_MOVEMENT_N_SENSOR_MIN_ERROR_OFFSET + 1] = (htons(sensorMin) & 0xff00) >> 8;

    dataBuffer[NID_MSG_SIM_MOVEMENT_N_SENSOR_MAX_ERROR_OFFSET] = (htons(sensorMax) & 0xff);
    dataBuffer[NID_MSG_SIM_MOVEMENT_N_SENSOR_MAX_ERROR_OFFSET + 1] = (htons(sensorMax) & 0xff00) >> 8;

    // Add link-layer and send message on socket
    return (SendData(NID_MSG_SIM_MOVEMENT_SIZE, dataBuffer));
}

/******************************************************************************
* Function:     InterpretApplicationData
* Description:
******************************************************************************/
bool LocoSimDLL::CODSimConnection::InterpretApplicationData(array<Byte>^ buffer, unsigned int size)
{
    // No incoming messages from CODSim

    return true;
}

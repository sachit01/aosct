#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  OPCSimConnection.cpp-3:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OPCSimConnection.cpp %
*
*  %version:       1 %
*
*  %created_by:    mmirzaei %
*
*  %date_created:  2018-09-12 11:00 %
*
*  DESCRIPTION:	Implementation of concrete class to create and handle communication with
*               OPCSim according to AOS Simulator-Interface Interflo 150.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-09-12    mmirzaei    File created
*
*******************************************************************************/
#include "OPCSimConnection.h"

static const unsigned char NID_MSG_SIM_REG_BALISE_ID = 20;
static const unsigned char NID_MSG_SIM_REG_BALISE_ID_SIZE = 3;
static const unsigned char NID_MSG_SIM_REG_BALISE_ID_MESSAGE_TYPE_OFFSET = 0;
static const unsigned char NID_MSG_SIM_REG_BALISE_ID_VALUE_OFFSET = 1;

static const unsigned char NID_MSG_SIM_ATP_READY = 21;

/******************************************************************************
* Function:     ReadATPReady
* Description:
******************************************************************************/
bool LocoSimDLL::OPCSimConnection::ReadATPReady()
{
  return ReadData();
}

/******************************************************************************
* Function:     WriteAOSRegBalieID
* Description:
******************************************************************************/
bool LocoSimDLL::OPCSimConnection::WriteAOSRegBaliseID(UInt16 baliseID)
{
  array<Byte>^ dataBuffer;

  dataBuffer = gcnew array<Byte>(NID_MSG_SIM_REG_BALISE_ID_SIZE);

  dataBuffer[NID_MSG_SIM_REG_BALISE_ID_MESSAGE_TYPE_OFFSET] = NID_MSG_SIM_REG_BALISE_ID;

  dataBuffer[NID_MSG_SIM_REG_BALISE_ID_VALUE_OFFSET] = (htons(baliseID)&0xFF);
  dataBuffer[NID_MSG_SIM_REG_BALISE_ID_VALUE_OFFSET+1] = (htons(baliseID)&0xFF00)>>8;

  // Add link-layer and send message on socket
  return (SendData(NID_MSG_SIM_REG_BALISE_ID_SIZE, dataBuffer));
}

/******************************************************************************
* Function:     InterpretApplicationData
* Description:
******************************************************************************/
bool LocoSimDLL::OPCSimConnection::InterpretApplicationData(array<Byte>^ buffer, unsigned int size)
{
  (void)size;
  unsigned int i = 0;
  bool retVal = true;

  // Parse & store information from received messages
  switch (buffer[i++])
  {
  case NID_MSG_SIM_ATP_READY:
    // Great, we got the ATP ready signal.
    break;

  default:
    retVal = false;
    break;
  }

  return retVal;
}

#pragma once
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
#include "AbstractSimConnection.h"


namespace LocoSimDLL {

  public ref class OPCSimConnection : AbstractSimConnection
  {

  public:
    OPCSimConnection() {}

    bool ReadATPReady();

    // Writes input-data to simulator
    bool WriteAOSRegBaliseID(UInt16 baliseID);

  protected:

    // Parses messages related to OPCSim
    bool InterpretApplicationData(array<Byte>^ buffer, unsigned int size) override;
  };
}
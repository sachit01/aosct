#pragma once
#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          CODSimConnection.h %
*
*  %version:       2 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2016-10-18 15:27 %
*
*  DESCRIPTION:	Definition of concrete class to create and handle communication with
*               CODSim according to AOS Simulator-Interface Interflo 150.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-04    Marlundg	File created
*
*******************************************************************************/
#include "AbstractSimConnection.h"


namespace LocoSimDLL {

    public ref class CODSimConnection : AbstractSimConnection
    {

    public:
        CODSimConnection() {}

        // Writes movement data to simulator
        bool WriteAOSMovement(short speed, short acc, unsigned short sensorMin, unsigned short sensorMax);

    protected:

        // Parses messages related to CODSim
        bool InterpretApplicationData(array<Byte>^ buffer, unsigned int size) override;

    };
}
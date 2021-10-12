#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          VIOHSimConnection.h %
*
*  %version:       3 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-10 15:30 %
*
*  DESCRIPTION:	Definition of concrete class to create and handle communication with 
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
* 2016-10-04    Marlundg	File created
*
*******************************************************************************/
#include "AbstractSimConnection.h"


namespace LocoSimDLL {

    public ref class VIOHSimConnection : AbstractSimConnection
    {

    public:
        VIOHSimConnection() {}

        // Checks if anything to read, returns true if any output-data was received.
        bool ReadAOSOutputIO(array<bool>^ outputA, array<bool>^ outputB);

        // Writes input-data to simulator
        bool WriteAOSInputIO(array<bool>^ values, array<bool>^ health, array<Int16>^ analogvalues);

    protected:

        // Parses messages related to VIOHSim
        bool InterpretApplicationData(array<Byte>^ buffer, unsigned int size) override;

    private:
        array<bool>^    outputValuesA;
        array<bool>^    outputValuesB;
    };
}
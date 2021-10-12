#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          ATPCom.h %
*
*  %version:       7 %
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
* 2014-03-17    Hidaji      Added sendToATOInterval
* 2014-04-03    Antbäck     Added atoLinkEnabled
* 2014-04-03    Antbäck     Removed used of static variables
* 2017-02-27    Marlundg    Adapted for BHP Project
* 2017-03-24    Marlundg    LCS Simulator is now TCP Server
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;
using namespace System::IO;

#include "LCSSimConsts.h"
#include <set>

namespace LCSSimDLL {

public ref class ATPCom
{
public:

    bool                atpLinkEnabled;
    bool                atpConnected;
    bool                newAOSTrainCompRec;
    bool                newATPComdRec;
    bool                newMARec;
    bool                newWarningCurveRec;
    bool                newPathRec;
    bool                newRclInfoRec;

    // Data from ATP BHP/EMP
    ATOModeCabinSelectorEnum    recATOModeCabinSelector;
    TrainIdlingEnum             recTrainIdling;
    unsigned short              recTrackIdFrontOfTrain;
    unsigned long               recPositionOnTrackFrontOfTrain;
    TrainOrientationEnum        recTrainOrientationOnFrontTrack;
    unsigned short              recTrackIdRearOfTrain;
    unsigned long               recPositionOnTrackRearOfTrain;
    TrainOrientationEnum        recTrainOrientationOnRearTrack;
    TravelDirectionEnum         recTravelDirection;
    unsigned short              recAOSVehicleSpeed;
    BlueFlagEnum                recBlueFlag;
    unsigned long               recSystemTime;
    LimitedSupModeEnum          recLimSupMode;

    unsigned short              recNumberOfVehicles;
    array<UInt16>^              recRoadNumber;
    array<VehicleTypeEnum>^     recVehicleType;

    typedef std::set<LCSMessageTypeFromATPEnum> LCSMessageTypeFromATPSet;
    LCSMessageTypeFromATPSet*  recMessageTypesFromATP;

    DateTime^                   recTimeStamp;

    CommandMessageEnum          recvECPBTrainCompReq;
    CommandMessageEnum          recvHoldingBrake;
    unsigned long               recvTrainWeight;

    unsigned short              recvEndOfMATrkId;
    unsigned long               recvEndOfMAPos;
    MADirectionEnum             recvMADirection;
    unsigned short              recvMAMargin;

    unsigned short              recvNumCurvePoints;
    array<UInt16>^              recvTrackIDWarningCurve;
    array<signed long>^         recvPosInTrackWarningCurve;
    array<unsigned short>^      recvSpeedWarningCurve;

    unsigned short              recvNumTracks;
    array<unsigned short>^      recvTrackIds;
    unsigned short              recvSpeedBeginPath;
    unsigned char               recvNumSpeedChanges;
    array<unsigned short>^      recvTrackIDSpeedChange;
    array<signed long>^         recvPosInTrackSpeedChange;
    array<unsigned short>^      recvNewSpeedSpeedChange;
    unsigned short              recvNextTragetTrackId;
    unsigned long               recvNextTargetPos;
    unsigned long               recvReqTOANextTarget;
    unsigned short              recvTCCVerADSMap;

    AosOperationalModeEnum      recvAosOperationalMode;
    AosInterventionEnum         recvAosInterventionApplied;
    AllowedTrainMovementEnum    recvAllowedTrainMovement;
    signed long                 recvDtgForward;
    signed long                 recvDtgReverse;
    RclTrainOrientationEnum     recvRclTrainOrientation;
    unsigned short              recvCurrentCeilingSpeed;



    // Data to ATP BHP/EMP
    ATOModeCabinSelectorStatusEnum  sndATOModeCabinSelectorStatus;
    ATODrivingModeEnum              sndATODrivingMode;
    FreeRollingStatusEnum           sndFreeRollingStatus;
    BlueFlagStatusEnum              sndBlueFlagStatus;
    BlueFlagRequestEnum             sndBlueFlagRequest;
    AdsEtaStatusEnum                sndADSETAStatus;
    unsigned int                    sndADSETA;
    LCSATOReadyEnum                 sndLCSATOReady;
    ECPBSequenceStatusEnum          sndECPBSequenceStatus;
    ReadyForPrecisionStopEnum       sndReadyForPrecisionStop;
    BrakeSystemInUseEnum            sndBrakeSystemInUse;
    ECPBOperatingModeEnum           sndECPBOperatingMode;
    TrainIntegrityStatusECPBEnum    sndTrainIntegrityStatusECPB;
    unsigned char                   sndPercentageOpBreaksECPB;
    unsigned char                   sndLastCarBrakePressure;

    // Location Handling 
    HandlingDoneEnum                sndHandlingDone;

    //Front GPS Position
    array<unsigned short>^ sndGPSPosLocoFrnt;

    //Rear GPS Position
    array<unsigned short>^ sndGPSPosLastCarUnit;

    unsigned short                  sndVersionADSMap;
    unsigned char                   sndLocoPenaltyBrakeActive;
    unsigned int                    sndLocoSysFault;
    unsigned int                    sndAutoDrivingSystem;

    unsigned short                  sndNumberOfVehicles;
    unsigned short                  sndVehDetectedPosUnknown;
    unsigned short                  sndVehNotDetected;
    array<UInt16>^                  sndRoadNumber;
    array<VehicleTypeEnum>^         sndVehicleType;

    // Data used for BHP/EMP

    typedef std::set<LCSMessageTypeToATPEnum> LCSMessageTypeToATPSet;
    LCSMessageTypeToATPSet*         lcsMessageTypesToATP;

    DateTime^               sndTimeStamp;

    // Log data for GUI
    int                     guiFromATPCnt;
    array<String^>^         guiFromATPHeader;
    array<String^>^         guiFromATP;
    int                     guiToATPCnt;
    array<String^>^         guiToATPHeader;
    array<String^>^         guiToATP;
    array<String^>^         dbgStrings;
    int                     dbgStringsCnt;


    ATPCom(int lcsPortNr)
    {
        atpSocket  = nullptr;
        LcsPort = lcsPortNr;
        recRoadNumber       = gcnew array<UInt16>(maxVehicles);
        sndRoadNumber       = gcnew array<UInt16>(maxVehicles);
        sndVehicleType      = gcnew array<VehicleTypeEnum>(maxVehicles);
        recVehicleType      = gcnew array<VehicleTypeEnum>(maxVehicles);
        recvTrackIds              = gcnew array<UInt16>(maxPathTracks);
        recvTrackIDSpeedChange    = gcnew array<UInt16>(maxSpeedChange);
        recvPosInTrackSpeedChange = gcnew array<signed long>(maxSpeedChange);
        recvNewSpeedSpeedChange   = gcnew array<unsigned short>(maxSpeedChange);
        recvTrackIDWarningCurve = gcnew array<unsigned short>(maxSpeedCurvePoints);
        recvPosInTrackWarningCurve = gcnew array<signed long>(maxSpeedCurvePoints);
        recvSpeedWarningCurve = gcnew array<unsigned short>(maxSpeedCurvePoints);
        sndGPSPosLocoFrnt = gcnew array<unsigned short>(bytesForGPSFrontPos);
        sndGPSPosLastCarUnit = gcnew array<unsigned short>(bytesForGPSRearPos);
        recMessageTypesFromATP = new LCSMessageTypeFromATPSet();
        lcsMessageTypesToATP = new LCSMessageTypeToATPSet();

        // GUI data
        guiFromATPCnt       = 0;
        guiFromATPHeader    = gcnew array<String^>(50);
        guiFromATP          = gcnew array<String^>(50);
        guiToATPCnt         = 0;
        guiToATPHeader      = gcnew array<String^>(50);
        guiToATP            = gcnew array<String^>(50);
        dbgStrings          = gcnew array<String^>(50);
        dbgStringsCnt       = 0;
    }

    void Init(void);
    void Tick(void);

protected:

    // Sockets used
    int             LcsPort;

    Socket^         atpSocket;
    Socket^         lcsServerSocket;

    // ID to be used in guiFromATP logging-vector.
    int guiFromATPIdx;

    // Receive buffer for incoming messages 
    array<Byte>^ recvBuffer;
    unsigned int recvBufferSize;
    unsigned int recBufCnt;

    // Internal functions
    bool ReadFromATP(void);
    void SendToATP(void);
    void atpClearData(void);
    bool InterpretATPBuffer(LCSMessageTypeFromATPEnum id, unsigned char *buffer, int length);
    bool AssembleATPData(unsigned char *sendBuffer, unsigned short  &appBufappMsgId, unsigned short &appMsgLen);
};
}

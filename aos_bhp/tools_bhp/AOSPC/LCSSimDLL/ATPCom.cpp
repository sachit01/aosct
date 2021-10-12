#pragma once
#include "StdAfx.h"
#include "ATPCom.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  ATPCom.cpp-11:c++:arn_006#1 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          ATPCom.cpp %
*
*  %version:       11 %
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
* 2013-11-28    Bo H        Panto status to ATO corrected
* 2014-03-17    Hidaji      Execute SendToATO() independent from ReadFromATO and based on time intervals
* 2014-04-03    Antbäck     Handle atoLinkEnabled
* 2014-04-03    Antbäck     Removed used of static variables
* 2014-07-02    Hidaji      Added "Operability OK" to guiToATP
* 2014-12-16    Antbäck     Changes to improve UDP stability as detected in Win7
* 2014-12-18    Antbäck     Allow reuse of addresses on this socket
* 2017-02-27    Marlundg    Adapted for BHP Project
* 2017-03-24    Marlundg    LCS Simulator is now TCP Server
* 2017-05-12    NSyed       Added support for TrainComposition Message
* 2017-05-15    Marlundg    INADDR_ANY to communicate with any connecting LCS
* 2017-05-15    Marlundg    Accept data in EMP/ClassD sequence error and timing error (error is shown)
*
*******************************************************************************/

#include "emp_message.hpp"
#include "class_d_transceiver.hpp"

// Unmanaged classes need to be outside ATPCom-managed class 
static ATP::TG::EmpMsg           empMsg;
static ClassD::TransceiverClassD classDMsg;

/******************************************************************************
* Function:     Init
* Description:  Set up listening port for socket connection
******************************************************************************/
void LCSSimDLL::ATPCom::Init(void)
{
    atpClearData();
    newAOSTrainCompRec = false;
    newATPComdRec = false;
    newMARec = false;
    newWarningCurveRec = false;
    newPathRec = false;
    newRclInfoRec = false;

    // Setup GUI data
    guiFromATPHeader[guiFromATPCnt++] = "Current EMP Status";
    guiFromATPHeader[guiFromATPCnt++] = "Last EMP Error";
    guiFromATPHeader[guiFromATPCnt++] = "Current Class-D Status";
    guiFromATPHeader[guiFromATPCnt++] = "Last Class-D Error";
    guiFromATPHeader[guiFromATPCnt++] = "";
    guiFromATPHeader[guiFromATPCnt++] = "ATO Mode Cabin Selector";
    guiFromATPHeader[guiFromATPCnt++] = "Train Idling";
    guiFromATPHeader[guiFromATPCnt++] = "Track ID Front";
    guiFromATPHeader[guiFromATPCnt++] = "Position on Track Front";
    guiFromATPHeader[guiFromATPCnt++] = "Train Orientation Front";
    guiFromATPHeader[guiFromATPCnt++] = "Track ID Rear";
    guiFromATPHeader[guiFromATPCnt++] = "Position on Track Rear";
    guiFromATPHeader[guiFromATPCnt++] = "Train Orientation Rear";
    guiFromATPHeader[guiFromATPCnt++] = "Travel Direction";
    guiFromATPHeader[guiFromATPCnt++] = "Vehicle Speed";
    guiFromATPHeader[guiFromATPCnt++] = "Blue Flag";
    guiFromATPHeader[guiFromATPCnt++] = "System Time";
    guiFromATPHeader[guiFromATPCnt++] = "Limited Supervisor Mode";


    guiToATPHeader[guiToATPCnt++] = "ATO Mode Cabin Selector Status";
    guiToATPHeader[guiToATPCnt++] = "ATO Driving Mode";
    guiToATPHeader[guiToATPCnt++] = "Traction Mode";
    guiToATPHeader[guiToATPCnt++] = "BlueFlag Status";
    guiToATPHeader[guiToATPCnt++] = "BlueFlag Request";
    guiToATPHeader[guiToATPCnt++] = "ADS ETA Status";
    guiToATPHeader[guiToATPCnt++] = "ADS ETA";
    guiToATPHeader[guiToATPCnt++] = "LCS ATO Ready";
    guiToATPHeader[guiToATPCnt++] = "ECPB Sequence Status";
    guiToATPHeader[guiToATPCnt++] = "Ready for precision stop";
    guiToATPHeader[guiToATPCnt++] = "Brake System in use";
    guiToATPHeader[guiToATPCnt++] = "ECPB operating mode";
    guiToATPHeader[guiToATPCnt++] = "Train integrity status ECPB";
    guiToATPHeader[guiToATPCnt++] = "Percentage Brakes ECPB";
    guiToATPHeader[guiToATPCnt++] = "Last car brake pressure";
    guiToATPHeader[guiToATPCnt++] = "GPS Position Loco Front";
    guiToATPHeader[guiToATPCnt++] = "GPS Position Last Car";
    guiToATPHeader[guiToATPCnt++] = "Version of ADS map";
    guiToATPHeader[guiToATPCnt++] = "Loco Penalty Break Active";
    guiToATPHeader[guiToATPCnt++] = "Loco System Faults";
    guiToATPHeader[guiToATPCnt++] = "ADS Status";

    for (int i = 0; i < maxVehicles; i++)
    {
        recRoadNumber[i] = 0;
        recVehicleType[i] = VehicleTypeUnknown;
    }

    try
    {
        lcsServerSocket = gcnew Socket(AddressFamily::InterNetwork,
            SocketType::Stream,
            ProtocolType::Tcp);

        // Bind the listening socket to the port
        Net::IPAddress^ hostIP = gcnew Net::IPAddress(INADDR_ANY);
        Net::IPEndPoint^ ep = gcnew IPEndPoint(hostIP, LcsPort);
        lcsServerSocket->Bind(ep);

        // Start listening
        lcsServerSocket->Blocking = false;
        lcsServerSocket->Listen(1);

        // Allocate space for 5 messages and reset the index/count variable
        recvBufferSize = classDMsg.getClassDMessageMaxTotalLen() * 5;
        recvBuffer = gcnew array<Byte>(recvBufferSize);
        recBufCnt = 0;

    }
    catch (...)
    {
    }

}

/******************************************************************************
* Function:     Tick
* Description:
******************************************************************************/
void LCSSimDLL::ATPCom::Tick(void)
{
    // If link not enabled, close and return
    if (atpLinkEnabled)
    {
        // Check for new connections from AOS Client
        try
        {
            Socket^ tmpSocket;

            tmpSocket = lcsServerSocket->Accept();

            // Someone waiting ...
            if (false == atpConnected)
            {
                atpSocket = tmpSocket;
                atpSocket->Blocking = false;
                atpConnected = true;
            }
            else
            {
                // Close socket, already got connection 
                tmpSocket->Close();
            }
        }
        catch (...)
        {
            // No one waiting
        }
    }
    else
    {
        // Link disabled
        if (atpSocket)
        {
            atpSocket->Close();
        }
        atpClearData();
        return;
    }

    ReadFromATP();
    SendToATP();
}

/******************************************************************************
* Function:     ReadFromATP
* Description:
******************************************************************************/
bool LCSSimDLL::ATPCom::ReadFromATP(void)
{
    recMessageTypesFromATP->clear();

    // Read from LCS socket
    if (nullptr != atpSocket)
    {
        ATP::TG::EMPParseError empParseError;
        ClassD::ErrorType   classDParseError;

        // Check if still connected
        if (!((atpSocket->Poll(1000, SelectMode::SelectRead) && (atpSocket->Available == 0)) || !atpSocket->Connected))
        {
            try
            {
                const unsigned int availableSize = recvBufferSize - recBufCnt;
                if (availableSize > 0)
                {
                  int receiveResult = atpSocket->Receive(recvBuffer, recBufCnt, availableSize, SocketFlags::None);

                  if (receiveResult > 0)
                  {
                    recBufCnt += receiveResult;
                  }
                }

                while (recBufCnt > 0)
                {
                    unsigned short id;
                    unsigned char ver;

                    pin_ptr<uint8_t> pinnedBuffer = &recvBuffer[0];

                    // Parse Class-D
                    classDParseError = classDMsg.parseMessage(pinnedBuffer, recBufCnt);

                    if (classDParseError == ClassD::ErrorType::MessageIncomplete)
                    {
                      break;
                    }

                    // Copy EMP message from Class-D message body
                    memcpy(empMsg.getEMPBuffer(), classDMsg.getClassDBodyPtr(), classDMsg.getActualMsgLen());

                    // Parse EMP
                    empParseError = empMsg.parseEMPMessage(id, ver);

                    guiFromATPIdx = 0;

                    // Log the Class-D and EMP status
                    switch (empParseError)
                    {
                    case ATP::TG::EMPParseError::NoError:
                        guiFromATP[guiFromATPIdx++] = "No Error";
                        guiFromATPIdx++;
                        break;
                    case ATP::TG::EMPParseError::IncorrectProtocolVersion:
                        guiFromATP[guiFromATPIdx++] = "Incorrect Protocol Version";
                        guiFromATP[guiFromATPIdx++] = "Incorrect Protocol Version";
                        break;
                    case ATP::TG::EMPParseError::IncorrectFlag:
                        guiFromATP[guiFromATPIdx++] = "Incorrect Flag";
                        guiFromATP[guiFromATPIdx++] = "Incorrect Flag";
                        break;
                    case ATP::TG::EMPParseError::IncorrectMsgNum:
                        guiFromATP[guiFromATPIdx++] = "Incorrect MsgNum";
                        guiFromATP[guiFromATPIdx++] = "Incorrect MsgNum";
                        break;
                    case ATP::TG::EMPParseError::IncorrectMsgTime:
                        guiFromATP[guiFromATPIdx++] = "Incorrect MsgTime";
                        guiFromATP[guiFromATPIdx++] = "Incorrect MsgTime";
                        break;
                    case ATP::TG::EMPParseError::IncorrectVariableHeader:
                        guiFromATP[guiFromATPIdx++] = "Incorrect Variable Header";
                        guiFromATP[guiFromATPIdx++] = "Incorrect Variable Header";
                        break;
                    case ATP::TG::EMPParseError::IncorrectCRC:
                        guiFromATP[guiFromATPIdx++] = "Incorrect CRC";
                        guiFromATP[guiFromATPIdx++] = "Incorrect CRC";
                        break;
                    default:
                        guiFromATP[guiFromATPIdx++] = "Unknown Error";
                        guiFromATP[guiFromATPIdx++] = "Unknown Error";
                        break;
                    }

                    switch (classDParseError)
                    {
                    case  ClassD::ErrorType::NoError:
                        guiFromATP[guiFromATPIdx++] = "No Error";
                        guiFromATPIdx++;
                        break;
                    case ClassD::ErrorType::CommIdIncorrect:
                        guiFromATP[guiFromATPIdx++] = "CommId Incorrect";
                        guiFromATP[guiFromATPIdx++] = "CommId Incorrect";
                        break;
                    case ClassD::ErrorType::MissingSTX:
                        guiFromATP[guiFromATPIdx++] = "Missing STX";
                        guiFromATP[guiFromATPIdx++] = "Missing STX";
                        break;
                    case ClassD::ErrorType::MissingETX:
                        guiFromATP[guiFromATPIdx++] = "Missing ETX";
                        guiFromATP[guiFromATPIdx++] = "Missing ETX";
                        break;
                    case ClassD::ErrorType::IncorrectMessageFormat:
                        guiFromATP[guiFromATPIdx++] = "Incorrect MessageFormat";
                        guiFromATP[guiFromATPIdx++] = "Incorrect MessageFormat";
                        break;
                    case ClassD::ErrorType::NotDataMsg:
                        guiFromATP[guiFromATPIdx++] = "Not DataMsg";
                        guiFromATP[guiFromATPIdx++] = "Not DataMsg";
                        break;
                    default:
                        break;
                    }

                    // There's an empty raw just to make it easier to read.
                    guiFromATPIdx++;

                    // Accept message even if wrong CommId or Delayed (these errors will be shown in GUI)
                    if (((ClassD::ErrorType::NoError == classDParseError) || (ClassD::ErrorType::CommIdIncorrect == classDParseError)) &&
                      ((ATP::TG::EMPParseError::NoError == empParseError) || (ATP::TG::EMPParseError::IncorrectMsgNum == empParseError) || (ATP::TG::EMPParseError::IncorrectMsgTime == empParseError)))
                    {
                        // Decrease buffer count with received Class-D message length.
                        uint32_t consumedBytes = classDMsg.getClassDMessageActualLen();
                        recBufCnt -= consumedBytes;

                        // Move any remaining bytes to the start of the input buffer
                        if (recBufCnt > 0)
                        {
                          pin_ptr<void> dst = &recvBuffer[0];
                          pin_ptr<void> src = &recvBuffer[consumedBytes];
                          memmove(dst, src, recBufCnt);
                        }

                        // Interpret AOS Application message
                        if (InterpretATPBuffer(static_cast<LCSMessageTypeFromATPEnum>(id), empMsg.getEMPBodyBuffer(), empMsg.getEMPMessageMaxBodyLen()))
                        {
                            recMessageTypesFromATP->insert(static_cast<LCSMessageTypeFromATPEnum>(id));
                        }
                    }
                    else
                    {
                      // Clear already buffered messages if an error occurs.
                      recBufCnt = 0;
                    }
                }

                if (recMessageTypesFromATP->size() != 0)
                {
                  return true;
                }
            }

            catch (SocketException^ e)
            {
                // Close if socket is disconnected by client
                if (e->ErrorCode != 10035L)//WSAEWOULDBLOCK)
                {
                    atpSocket->Close();
                    atpClearData();
                }
            }

            // Any other error, close and try again
            catch (...)
            {
                atpSocket->Close();
                atpClearData();
            }
        }
        else
        {
            atpSocket->Close();
            atpClearData();
        }
    }

    return false;
}

/******************************************************************************
* Function:     InterpretATPBuffer
* Description:
******************************************************************************/
bool LCSSimDLL::ATPCom::InterpretATPBuffer(LCSMessageTypeFromATPEnum id, unsigned char *buffer, int length)
{
    bool valid = false;

    // Decode each message type, store parameter and also present in GUI-list.
    switch (id)
    {
    case LCSMessageTypeFromATPAOSStatus:
    {
        recATOModeCabinSelector = static_cast<ATOModeCabinSelectorEnum>(buffer[0]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recATOModeCabinSelector);

        recTrainIdling = static_cast<TrainIdlingEnum>(buffer[1]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recTrainIdling);

        recTrackIdFrontOfTrain = ntohs(*reinterpret_cast<unsigned short*>(&buffer[2]));

        guiFromATP[guiFromATPIdx++] = recTrackIdFrontOfTrain.ToString();
        recPositionOnTrackFrontOfTrain = ntohl(*reinterpret_cast<unsigned long*>(&buffer[4]));
        guiFromATP[guiFromATPIdx++] = recPositionOnTrackFrontOfTrain.ToString();
        recTrainOrientationOnFrontTrack = static_cast<TrainOrientationEnum>(buffer[8]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recTrainOrientationOnFrontTrack);

        recTrackIdRearOfTrain = ntohs(*reinterpret_cast<unsigned short*>(&buffer[9]));
        guiFromATP[guiFromATPIdx++] = recTrackIdRearOfTrain.ToString();
        recPositionOnTrackRearOfTrain = ntohl(*reinterpret_cast<unsigned long*>(&buffer[11]));
        guiFromATP[guiFromATPIdx++] = recPositionOnTrackRearOfTrain.ToString();
        recTrainOrientationOnRearTrack = static_cast<TrainOrientationEnum>(buffer[15]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recTrainOrientationOnRearTrack);
        recTravelDirection = static_cast<TravelDirectionEnum>(buffer[16]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recTravelDirection);
        recAOSVehicleSpeed = ntohs(*reinterpret_cast<unsigned short*>(&buffer[17]));
        guiFromATP[guiFromATPIdx++] = recAOSVehicleSpeed.ToString();
        recBlueFlag = static_cast<BlueFlagEnum>(buffer[19]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recBlueFlag);
        recSystemTime = ntohl(*reinterpret_cast<unsigned long*>(&buffer[20]));
        guiFromATP[guiFromATPIdx++] = recSystemTime.ToString();
        recLimSupMode = static_cast<LimitedSupModeEnum>(buffer[24]);
        guiFromATP[guiFromATPIdx++] = Convert::ToString(recLimSupMode);
        break;
    }
    case LCSMessageTypeFromATPATPCommand:
    {
      newATPComdRec = true;

      recvECPBTrainCompReq = static_cast<CommandMessageEnum>(buffer[0]);
      recvHoldingBrake = static_cast<CommandMessageEnum>(buffer[1]);
      recvTrainWeight = ntohl(*reinterpret_cast<unsigned long*>(&buffer[2]));
    }
        break;
    case LCSMessageTypeFromATPATOCommand:
        break;
    case LCSMessageTypeFromATPMovementAuthority:
    {
      newMARec = true;

      recvEndOfMATrkId = static_cast<unsigned short>(ntohs(*reinterpret_cast<unsigned short*>(&buffer[0])));
      recvEndOfMAPos = ntohl(*reinterpret_cast<unsigned long*>(&buffer[2]));
      recvMADirection = static_cast<MADirectionEnum>(buffer[6]);
      //recvSpeed = static_cast<unsigned short>(ntohs(*reinterpret_cast<unsigned short*>(&buffer[7])));
      recvMAMargin = static_cast<unsigned short>(ntohs(*reinterpret_cast<unsigned short*>(&buffer[7])));
      break;
    }
    case LCSMessageTypeFromATPWarningCurve:
    {
      int idx = 0;
      newWarningCurveRec = true;
      recvNumCurvePoints = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
      idx++;

      for (size_t i = 0; i < recvNumCurvePoints; i++)
      {
        recvTrackIDWarningCurve[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;
        recvPosInTrackWarningCurve[i] = ntohl(*reinterpret_cast<signed long*>(&buffer[idx++]));
        idx += 3;
        recvSpeedWarningCurve[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;
      }
      break;
    }
    case LCSMessageTypeFromATPTrainComposition:
    {
        newAOSTrainCompRec = true;
        int idx = 0;
        recNumberOfVehicles = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;

        for (int i = 0; i < recNumberOfVehicles; i++)
        {
          recVehicleType[i] = static_cast<VehicleTypeEnum>(buffer[idx++]);
          recRoadNumber[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
          idx++;
        }
        break;
    }
    case LCSMessageTypeFromATPPath:
    {
      int idx = 0;
      newPathRec = true;
      recvNumTracks = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
      idx++;

      for (size_t i = 0; i < recvNumTracks; i++)
      {
        recvTrackIds[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;
      }

      recvSpeedBeginPath = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
      idx++; //Speed path is 2 bytes.
      recvNumSpeedChanges = (*reinterpret_cast<unsigned char*>(&buffer[idx++]));

      for (size_t i = 0; i < recvNumSpeedChanges; i++)
      {
        recvTrackIDSpeedChange[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;
        recvPosInTrackSpeedChange[i] = ntohl(*reinterpret_cast<signed long*>(&buffer[idx++]));
        idx += 3;
        recvNewSpeedSpeedChange[i] = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
        idx++;
      }

      recvNextTragetTrackId = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
      idx++;

      recvNextTargetPos = ntohl(*reinterpret_cast<unsigned long*>(&buffer[idx++]));
      idx += 3;

      recvReqTOANextTarget = ntohl(*reinterpret_cast<unsigned long*>(&buffer[idx++]));
      idx += 3;

      recvTCCVerADSMap = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++]));
      idx++;
    }
        break;

    case LCSMessageTypeFromATPRclInformation:
    {
      int idx = 0;
      newRclInfoRec = true;
      recvAosOperationalMode = static_cast<AosOperationalModeEnum>(buffer[idx++]);

      recvAosInterventionApplied = static_cast<AosInterventionEnum>(buffer[idx++]);

      recvAllowedTrainMovement = static_cast<AllowedTrainMovementEnum>(buffer[idx++]);

      recvDtgForward = ntohl(*reinterpret_cast<unsigned long*>(&buffer[idx++]));
      idx += 3;

      recvDtgReverse = ntohl(*reinterpret_cast<unsigned long*>(&buffer[idx++]));
      idx += 3;

      recvRclTrainOrientation = static_cast<RclTrainOrientationEnum>(buffer[idx++]);

      recvCurrentCeilingSpeed = ntohs(*reinterpret_cast<unsigned short*>(&buffer[idx++])) / 10;
      idx++;

      break;

    }

    default:
        break;
    }

    return true;
}

/******************************************************************************
* Function:     AssembleATPData
* Description:
******************************************************************************/
bool LCSSimDLL::ATPCom::AssembleATPData(unsigned char *sendMsgBytes, unsigned short  &appBufappMsgId, unsigned short &appMsgLen)
{
    bool messagePopulated = false;

    // Build array to send
    int cnt = 0;
    int guiIdx = 0;

    // Check which message that is going to be sent to ATO
    if (lcsMessageTypesToATP->find(LCSMessageTypeToATPTrainStatus) != lcsMessageTypesToATP->end())
    {
        lcsMessageTypesToATP->erase(LCSMessageTypeToATPTrainStatus);

        sendMsgBytes[cnt++] = sndATOModeCabinSelectorStatus;
        guiToATP[guiIdx++] = Convert::ToString(sndATOModeCabinSelectorStatus);
        sendMsgBytes[cnt++] = sndATODrivingMode;
        guiToATP[guiIdx++] = Convert::ToString(sndATODrivingMode);
        sendMsgBytes[cnt++] = sndFreeRollingStatus;
        guiToATP[guiIdx++] = Convert::ToString(sndFreeRollingStatus);
        sendMsgBytes[cnt++] = sndBlueFlagStatus;
        guiToATP[guiIdx++] = Convert::ToString(sndBlueFlagStatus);
        sendMsgBytes[cnt++] = sndBlueFlagRequest;
        guiToATP[guiIdx++] = Convert::ToString(sndBlueFlagRequest);
        sendMsgBytes[cnt++] = sndADSETAStatus;
        guiToATP[guiIdx++] = Convert::ToString(sndADSETAStatus);
        *(reinterpret_cast<unsigned long*>(&sendMsgBytes[cnt])) = htonl(sndADSETA);
        cnt += 4;
        guiToATP[guiIdx++] = Convert::ToString(sndADSETA);
        sendMsgBytes[cnt++] = sndLCSATOReady;
        guiToATP[guiIdx++] = Convert::ToString(sndLCSATOReady);
        sendMsgBytes[cnt++] = sndECPBSequenceStatus;
        guiToATP[guiIdx++] = Convert::ToString(sndECPBSequenceStatus);
        sendMsgBytes[cnt++] = sndReadyForPrecisionStop;
        guiToATP[guiIdx++] = Convert::ToString(sndReadyForPrecisionStop);
        sendMsgBytes[cnt++] = sndBrakeSystemInUse;
        guiToATP[guiIdx++] = Convert::ToString(sndBrakeSystemInUse);
        sendMsgBytes[cnt++] = sndECPBOperatingMode;
        guiToATP[guiIdx++] = Convert::ToString(sndECPBOperatingMode);
        sendMsgBytes[cnt++] = sndTrainIntegrityStatusECPB;
        guiToATP[guiIdx++] = Convert::ToString(sndTrainIntegrityStatusECPB);
        sendMsgBytes[cnt++] = sndPercentageOpBreaksECPB;
        guiToATP[guiIdx++] = Convert::ToString(sndPercentageOpBreaksECPB);
        sendMsgBytes[cnt++] = sndLastCarBrakePressure;
        guiToATP[guiIdx++] = Convert::ToString(sndLastCarBrakePressure);


        // GPS Pos Front Loco
        for (int i = 0; i < bytesForGPSFrontPos; i++)
        {
          *(reinterpret_cast<unsigned long*>(&sendMsgBytes[cnt])) = htonl(sndGPSPosLocoFrnt[i]);
          cnt++;          
        }
        guiToATP[guiIdx++] = Convert::ToString(sndGPSPosLocoFrnt[0]);

        // GPS Pos Rear (Last car unit)
        for (int i = 0; i < bytesForGPSFrontPos; i++)
        {
          *(reinterpret_cast<unsigned long*>(&sendMsgBytes[cnt])) = htonl(sndGPSPosLastCarUnit[i]);
          cnt++;
        }
        guiToATP[guiIdx++] = Convert::ToString(sndGPSPosLastCarUnit[0]);

        *(reinterpret_cast<unsigned short*>(&sendMsgBytes[cnt])) = htons(sndVersionADSMap);
        cnt += 2;
        guiToATP[guiIdx++] = Convert::ToString(sndVersionADSMap);

        sendMsgBytes[cnt++] = sndLocoPenaltyBrakeActive;
        guiToATP[guiIdx++] = Convert::ToString(sndLocoPenaltyBrakeActive);

        *(reinterpret_cast<unsigned long*>(&sendMsgBytes[cnt])) = htonl(sndLocoSysFault);
        cnt += 4;
        guiToATP[guiIdx++] = Convert::ToString(sndLocoSysFault);

        *(reinterpret_cast<unsigned long*>(&sendMsgBytes[cnt])) = htonl(sndAutoDrivingSystem);
        cnt += 4;
        guiToATP[guiIdx++] = Convert::ToString(sndAutoDrivingSystem);

        // Out-parameters
        appMsgLen = cnt;
        appBufappMsgId = LCSMessageTypeToATPTrainStatus;

        messagePopulated = true;
    }
    else if (lcsMessageTypesToATP->find(LCSMessageTypeToATPTrainComposition) != lcsMessageTypesToATP->end())
    {
        lcsMessageTypesToATP->erase(LCSMessageTypeToATPTrainComposition);

        *(reinterpret_cast<unsigned short*>(&sendMsgBytes[cnt])) = htons(sndNumberOfVehicles);
        cnt += 2;
        *(reinterpret_cast<unsigned short*>(&sendMsgBytes[cnt])) = htons(sndVehDetectedPosUnknown);
        cnt += 2;
        *(reinterpret_cast<unsigned short*>(&sendMsgBytes[cnt])) = htons(sndVehNotDetected);
        cnt += 2;
        for (int i = 0; i < sndNumberOfVehicles; i++)
        {
          sendMsgBytes[cnt++] = sndVehicleType[i];
          *(reinterpret_cast<unsigned short*>(&sendMsgBytes[cnt++])) = htons(sndRoadNumber[i]);
          cnt++;  // for appMsgLen to be correct
        }

        // Out-parameters
        appMsgLen = cnt;
        appBufappMsgId = LCSMessageTypeToATPTrainComposition;

        messagePopulated = true;
    }
    else if (lcsMessageTypesToATP->find(LCSMessageTypeToATPRclStatus) != lcsMessageTypesToATP->end())
    {
      lcsMessageTypesToATP->erase(LCSMessageTypeToATPRclStatus);

      sendMsgBytes[cnt++] = sndHandlingDone;

      // Out-parameters
      appMsgLen = cnt;
      appBufappMsgId = LCSMessageTypeToATPRclStatus;

      messagePopulated = true;
    }
    else
    {
      // Nothing to send
    }

    return messagePopulated;
}

/******************************************************************************
* Function:     SendToATP
* Description:
******************************************************************************/
void LCSSimDLL::ATPCom::SendToATP(void)
{

    if (nullptr != atpSocket)
    {
        try
        {
            unsigned short appMsgId;
            unsigned short appMsgLen;

            array<Byte>^    sendMsgBytes = gcnew array<Byte>(classDMsg.getClassDMessageMaxTotalLen());
            pin_ptr<void>   pinnedSendBuffer = &sendMsgBytes[0];

            // Fetch Application Data
            if (AssembleATPData(empMsg.getEMPBodyBuffer(), appMsgId, appMsgLen))
            {
                // Add EMP Header
                empMsg.addEMPEnvelope(appMsgId, 1, appMsgLen);

                // Build a Class-D Message from the EMP Message
                classDMsg.buildMessage(empMsg.getEMPBuffer(), empMsg.getEMPMessageActualLen());

                // Copy CLass-D Message to managed array to be able to use Send(...)
                memcpy(pinnedSendBuffer, classDMsg.getClassDBufferPtr(), classDMsg.getClassDMessageActualLen());

                // Send to LCS-Client
                atpSocket->Send(sendMsgBytes, classDMsg.getClassDMessageActualLen(), SocketFlags::None);
            }
        }
        // Socket error, close and let AOS reconnect
        catch (SocketException^)
        {
            atpSocket->Close();
            atpClearData();
        }
    }
}

/******************************************************************************
* Function:     atoClearData
* Description:
******************************************************************************/
void LCSSimDLL::ATPCom::atpClearData(void)
{
    atpConnected = false;
    atpSocket = nullptr;

    classDMsg.resetCommID();
}


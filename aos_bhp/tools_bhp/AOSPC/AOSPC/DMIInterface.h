#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:    Later.
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-11-11    bhermans    File created
*
*******************************************************************************/
#include "LocoConsts.h"

using namespace System;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;



namespace AOSPC
{
  enum atpInfoField
  {
    atpInfoId,
    atpInfoATPMode,
    atpInfoConfigModeSubstate,
    atpInfoDriverVerState,
    atpInfoInterfaceFlags,
    atpInfoCoreTrainStatus,
    atpInfoATOMode,
    atpInfoAdditionalStatus,
    atpInfoConfirmChange,
    atpInfoAllowedTo,
    atpInfoBrakeTestStatus,
    atpInfoAdditionalAllowedTo,
    atpInfoAdditionalConfirm,
    atpInfoAdaptationTrainStatus,
    atpInfoPermittedDrivingDirection,
    atpInfoPermittedSpeed,
    atpInfoTargetSpeed,
    atpInfoTimeToIntervention,
    atpInfoRemainingDistanceTotarget,
    atpInfoRemainingDistanceToBCA,
    atpInfoBrakeRelatedIndications,
    atpInfoSupervisionRelatedIndications,
    atpInfoMAMargin,
    atpInfoCurrentSpeed,
    atpInfoLeadingTrack,
    atpInfoLeadingPos,
    atpInfoTrailingTrack,
    atpInfoTrailingPos,
    atpInfoTrainLength,
    atpInfoTrackGradient,
    atpInfoEffectiveGradient,
    atpInfoBrakeability,
    atpInfoBrakeDelayEB,
    atpInfoBrakeDelaySB
  };

  public ref class DMIInterface
  {
  public:
    DMIInterface(IPAddress^ ip, int dmiPort, int dmiInternalPort);
    void sendCommandToDMI(unsigned int size, array<unsigned char>^ data);
    bool responseReceived(void);
    bool internalResponseReceived(void);
    void Tick(void);

    UInt16             getPermittedSpeed(void); // cm/s 
    EnumDriveDir       getPermittedDriveDir(void);
    ATPModeEnum        getAtpMode(void);
    UInt16             getDistanceToTarget(void); // m 
    UInt16             getDistanceToBCA(void); // m 
    UInt16             getTargetSpeed(void);   // cm/s
    UInt16             getMAMargin(void); // cm
    bool               getIsAllowedToLogin(void);
    bool               getReleaseServiceBrakeFlashes(void);
    bool               getConfirmAcceptAutoConfig(void);
    ConfigModeSubState getConfigModeSubState(void);
    UInt16             getTrailingTrack(void);
    UInt32             getTrailingPos(void); 
    UInt16             getTrainLength(void); // m
    Int16              getTrackGradient(void); // permil
    Int16              getEffectiveGradient(void); // permil
    UInt16             getBrakeability(void); // cm/s2 
    UInt16             getBrakeDelayEB(void); // (0.1 s)
    UInt16             getBrakeDelaySB(void); // (0.1 s)

    void               releaseEB();
    void               releaseSB();

    array<String^>^ fields;
    String^         recString;

  private:
    // Internal functions     
    UInt16 atpInfoFieldIndex(enum atpInfoField infoField);
    void readResponseFromDMI(void);
    void readInternalResponseFromDMI(void);
    void sendInternalCommandToDMI(unsigned int size, array<unsigned char>^ data);
    void connect(Socket^ %socket, IPAddress^ ip, unsigned short int port);
    bool connected(Socket^ socket);

    // Receive buffer for incoming messages 
    array<Byte>^    recvBuffer;
    int             recBufCnt;
    int             recInternalBufCnt;
    Socket^         dmiSocket;
    EndPoint^       endPointDMI;
    Socket^         dmiInternalSocket;
    EndPoint^       endPointDMIInternal;
    IPAddress^      ipToDMI;
    int             portToDMI;
    int             internalPortToDMI;

    // Maximum Buffer Size 
    static const int maxBufferSize = 256;

    // DMI poll period
    unsigned int ticks;
    static const unsigned int dmiPollPeriod = 20U;

    // release brake requested
    bool releaseEBReq;
    bool releaseSBReq;
  };
}

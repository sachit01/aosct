#ifndef _OTELIB_SPL_STUB_
#define _OTELIB_SPL_STUB_

#include "atc_types.hpp"

#ifndef _MSC_VER
#include <string>
#endif
#include <queue>
#include <vector>
//#include "bs_global.h"

class OteLibSplMRT
{
public:
    OteLibSplMRT(const char_t* const messageBuffer, uint32_t messageLength, const uint64_t receptionTime);
    ~OteLibSplMRT();

    uint32_t getMessageLength() const {return messageLength_;};
    void * getMessageBuffer() const {return messageBuffer_;};
    uint64_t getReceptionTime() const {return receptionTime_;};
private:
    OteLibSplMRT(const OteLibSplMRT&);
    OteLibSplMRT& operator=(const OteLibSplMRT&);

protected:
    const uint32_t messageLength_;
    void *messageBuffer_;
    const uint64_t receptionTime_;
};




// This class is similar to Ote_SPL_Connection found in the EXE.
class OteLib_SPL_Connection
{
public:

    // The numbers used here must be same as in OTEDllStart::Ote_SPL_Connection in file "OTEDllStart.h".
    enum ConnectionMode {
        P2P_Slave = 0,
        P2P_Master = 1,
        MCAST_Sender = 2,
        MCAST_Receiver =3
    };

    OteLib_SPL_Connection( const char_t* pName,
                           const uint32_t SourceProfibusAddress,
                           const uint32_t TargetProfibusAddress,
                           const uint32_t SAP,
                           const ConnectionMode connectionMode,
                           const uint32_t SafetyLevel,
                           const uint32_t IdleTime,
                           const /*SPL_P2P_Protocol_Parameters*/void * const pProtocolParameters,
                           const /*SPL_Connection*/void * const pP2PConnection);
    ~OteLib_SPL_Connection();


    const void* const getPP2PConnection() const {return pP2PConnection_;}
    void setPP2PConnection(void *pP2PConnection) {pP2PConnection_=pP2PConnection;}

    uint32_t getSourceProfibusAddress() const {return SourceProfibusAddress_;}
    uint32_t getTargetProfibusAddress() const {return TargetProfibusAddress_;}
    uint32_t getSAP() const {return SAP_;}
    uint32_t getIsMaster() const {return connectionMode_==P2P_Master;}
    uint32_t getIsSlave() const {return connectionMode_==P2P_Slave;}
    uint32_t getIsMcastSender() const {return connectionMode_==MCAST_Sender;}
    uint32_t getIsMcastReceiver() const {return connectionMode_==MCAST_Receiver;}
    uint32_t getSafetyLevel() const {return SafetyLevel_;}
    uint32_t getIdleTime() const {return IdleTime_;}
    void setState(int32_t state);
    int32_t getState() const {return state_;}

    int32_t putSplMessage(const uint8_t* const messageBuffer, uint32_t messageLength, const uint64_t receptionTime);
    int32_t takeSplMessage(uint8_t* const messageBuffer, uint32_t bufferSize, uint64_t* receptionTime);

    void resetQueue();

private:
    OteLib_SPL_Connection(const OteLib_SPL_Connection&);
    OteLib_SPL_Connection& operator=(const OteLib_SPL_Connection&);

    const uint32_t SourceProfibusAddress_;
    const uint32_t TargetProfibusAddress_;  // not to be used for multicast connections (set to zero)
    const uint32_t SAP_;
    const ConnectionMode connectionMode_;
    const uint32_t SafetyLevel_;
    const uint32_t IdleTime_;  // Not used for multicast (set to zero)
    const /*SPL_P2P_Protocol_Parameters*/void * const pProtocolParameters_;  // TODO: We should copy the data not just store the pointer to it.
    const /*SPL_Connection*/void * pP2PConnection_;  // When we have got rid of setSPL/getSPL this pinter can be removed...

    int32_t state_; // For possible "Connection State Codes" see "spl_a_errorcodes.h"

    // TODO:
    // We must have a message queue here. So that OTEDllInterface::handleMessageFromExe() can put messages here. Then When SPL_SIM_Queue_recv is called it shall pick upp the message from here.


    std::queue<OteLibSplMRT*> queueForMessagesReceivedFromExe_;
};

class OteLibFindP2PConnection
{
public:
    enum{
        WildCard=0xFFFFFFFF
    };


    OteLibFindP2PConnection(
        const uint32_t source,
        const uint32_t target,
        const uint32_t sap);

    bool operator()(const OteLib_SPL_Connection* conn);

private:
    const uint32_t source_;
    const uint32_t target_;
    const uint32_t sap_;

};


typedef std::vector<OteLib_SPL_Connection*> oteDllSplStub_splConnectionsListType;


void oteDllSplStub_addSplConnection(OteLib_SPL_Connection *splConnection);
OteLib_SPL_Connection * oteDllSplStub_findSplConnection(const uint32_t My_Profibus_Address, const uint32_t targetAddress, const uint32_t sap);
void oteDllSplStub_removeSplConnection(OteLib_SPL_Connection *splConnection);
void oteDllSplStub_removeSplConnections();


int32_t oteDllSplStub_appSpecificWriteSplDataToAtpBuf(const uint8_t* writeBuffer, int32_t sap);

#endif

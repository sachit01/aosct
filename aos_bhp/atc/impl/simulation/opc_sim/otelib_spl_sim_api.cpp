/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This simulates the functionality of the SPL Library
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-04-26    rquensel    Created from P8/OTE code
*
*******************************************************************************/

/*******************************************************************************
* Includes
*******************************************************************************/

#include <assert.h>
#include <string.h>
#include "spl_api_interface.h"
#include "otelib_splstub.h"
#include "spl_a_errorcodes.h"

#ifdef WIN32
// Its is not possible to include <vfw_time.h> in visual studio
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif


/*******************************************************************************
* Macros
*******************************************************************************/

/*******************************************************************************
* Declarations and Definitions
*******************************************************************************/

extern "C" uint8_t _IOgetProfibusInputData(Profibus_Type* pBuff);
extern "C" void _IOsetProfibusOutputData(Profibus_Type* pBuff);

namespace
{
  const uint32_t splHeaderSize = 7U;
}

extern "C"
{
  /******************************************************************************
   * SPL_SIM_Initialyze
   ******************************************************************************/
   /**
    *      Initialise the SPL library.
    *      The Profibus Manager shall be initialized.
    *      The Profibus Driver, FDL level driver and board shall be initialized too.
    *
    *      @param [in] ProfibusAddress
    *                    The profibus address of this application/station [0..126]
    *
    *      @param [in] usedDriverType  Type of Profibus driver to use, as in list below
    *
    *      @param [in] pDriverInitStruct
    *                    Structure with FDL parameters, defined in Pl_driver2.h
    *                    If this parameter is 0U, the default values are used,
    *                    My_Profibus_Address is copied to the TS field of the structure.
    *                    If this parameter is 0U, the default values are used,
    *                    My_Profibus_Address is copied to the TS field of the structure.
    *
    *      @param [out] pResult  Pointer to variable for function return value.
    *
    *      @param [in]  Token  Input token value.
    *
    *      @return      Token  Output token value. Inverse value of input token.
    *
    *      @return      pResult  SPL_INIT_OK on success, else
    *                            SPL_INIT_BAD_PB_ADDRESS <br>
    *                            SPL_INIT_BAD_DRIVER <br>
    *                            SPL_INIT_BAD_PARAMETER <br>
    *                            SPL_INIT_DRIVER_ERROR <br>
    *
    *****************************************************************************/
  const int32_t
    SPL_SIM_Initialise(const uint32_t /*ProfibusAddress*/,
      const uint32_t /*usedDriverType*/,
      const PL_Driver_Initialisation_Struct* const /*pDriverInitStruct*/,
      int32_t * const pResult,
      const int32_t token)
  {
    *pResult = 0; // OK
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Shutdown
  ******************************************************************************/
  const int32_t
    SPL_SIM_Shutdown(const uint32_t /*myProfibusAddress*/,
      const int32_t token)
  {
    // Clean up local data.
    oteDllSplStub_removeSplConnections();

    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Reference_Clock_Master
  ******************************************************************************/
  const int32_t
    SPL_SIM_Reference_Clock_Master(const uint32_t  /*myProfibusAddress*/,
      const uint32_t  /*Phase_1_Frequency*/,
      const uint32_t  /*Phase_1_Duration*/,
      const uint32_t  /*Phase_2_Frequency*/,
      const uint32_t  /*Local_Clock_Inaccuracy*/,
      const uint32_t  /*SAP*/,
      int32_t * const pResult,
      const int32_t token)
  {
    *pResult = 0;
    return ~token;
  }

  /******************************************************************************
  * SPL_SIM_Get_ClockMasterDataB
  ******************************************************************************/
  int32_t
    SPL_SIM_Get_ClockMasterDataB(const uint32_t /*myProfibusAddress*/,
      uint32_t* const /*LocalClockInaccuracy*/)
  {
    // Not implemented yet
    return ~0;
  }

  /******************************************************************************
  * SPL_SIM_Reference_Clock_Slave
  ******************************************************************************/
  /**
   * Initialize the Reference Clock object as Slave.
   * Create a SPL Connection object for receiving the Reference Clock telegrams.
   * Store the parameters in internal structures, they shall be used
   * in SPL_Clock_Management.
   *
   *      @param [in] LocalClockMaxResyncInterval
   *                            according to subset 056, 9.2,
   *                            in ms, recommended value (subset 059) = 5 000
   *
   *      @param [in] MaxClockInaccuracyAfterAdjustFactor
   *                            according to subset 056, 9.6.
   *                            This constant gives the maximum allowed error
   *                            of LocalRefTime in relation to RefTime.
   *                            in ms, recommended value (subset 059) =  100
   *
   *      @param [in] TimeForLongTermDriftCheck
   *                            according to subset 056, 9.7,
   *                            in ms, recommended value (subset 059) =  10 000
   *
   *      @param [in] MinNumberOfSyncAndRefMsgReceived
   *                            according to subset 056, 9.8,
   *                            recommended value (subset 059) =  30
   *
   *      @param [in] LocalClockInaccuracy Local clock inaccuracy in ms, recommended value = 1
   *
   *      @param [in] SAP       SAP for clock connection, recommended value
   *                            (subset 035) = 1000002 = 20H = 3210
   *
   *      @param [out] pResult  Pointer to variable for function return value.
   *
   *      @param [in]  Token    Input token value.
   *
   *      @return      Token    Output token value. Inverse value of input token.
   *
   *      @return      pResult  SPL_CLOCK_OK on success, SPL_CLOCK_ERROR otherwise.
   *
   *
   * @note
   *    The parameters are used for the check, if the clock is still synchronized,
   *    or lost synchronisation because of missing Reference Clock telegrams,
   *    or short or long term drift over the limits.
   *
   ***************************************************************************/
  const int32_t
    SPL_SIM_Reference_Clock_Slave(const uint32_t  /*myProfibusAddress*/,
      const uint32_t  /*Local_Clock_Max_Resync_Interval*/,
      const uint32_t  /*Max_Clock_Inaccuracy_After_Adjust_Factor*/,
      const uint32_t  /*Time_For_Long_Term_Drift_Check*/,
      const uint32_t  /*Min_Numberof_SyncAndRef_Msg_Received*/,
      const uint32_t  /*Local_Clock_Inaccuracy*/,
      const uint32_t  /*SAP*/,
      int32_t * const pResult,
      const int32_t token)
  {
    *pResult = 0;
    return ~token;
  }

  /******************************************************************************
  * SPL_SIM_Get_ClockSlaveDataB
  ******************************************************************************/
  int32_t
    SPL_SIM_Get_ClockSlaveDataB(const uint32_t  /*myProfibusAddress*/,
      uint32_t* const /*LocalClockMaxResyncInterval*/,
      uint32_t* const /*MaxClockInaccuracyAfterAdjustFactor*/,
      uint32_t* const /*TimeForLongTermDriftCheck*/,
      uint32_t* const /*MinNumberOfSyncAndRefMsgReceived*/,
      uint32_t* const /*LocalClockInaccuracy*/)
  {
    /*
      // Not implemented yet
      *LocalClockMaxResyncInterval         = localClockMaxResyncIntervalB_;
      *MaxClockInaccuracyAfterAdjustFactor = maxClockInaccuracyAfterAdjustFactorB_;
      *TimeForLongTermDriftCheck           = timeForLongTermDriftCheckB_;
      *MinNumberOfSyncAndRefMsgReceived    = minNumberOfSyncAndRefMsgReceivedB_;
      *LocalClockInaccuracy                = localClockInaccuracyB_;
    */
    return ~SPL_CLOCK_OK;
  }


  /******************************************************************************
  *  SPL_SIM_Create_P2P:
  *
  *  Description: The Point to point connection will be simulated by using TCP/IP.
  *
  *  Name                 Debug purpose only
  *  Safety_Level         N/A
  *  Idle_Time            N/A
  *  ProtocolParameters   N/A
  ******************************************************************************/
  /**
   * Create an object describing a SPL peer-to-peer connection.
   *
   *      @param [in] pName      For debugging purposes only. No more than 10 characters.
   *                            Use 0U if unused.
   *
   *      @param [in] TargetProfibusAddress  The Profibus address of the remote
   *                                          station. Values   [0..126].
   *
   *      @param [in] SAP        Profibus Service Access Point [0..62].
   *
   *      @param [in] isMaster   =0: The remote station is master, we are slave. The remote
   *                            station shall call SPL_Connect, we must wait until
   *                            is_Active returns IS_CONNECTED. \n
   *                            =1: We are master. We shall call SPL_Connect, we
   *                            must wait until is_Active returns IS_CONNECTED. \n
   *
   *      @param [in] SafetyLevel  =SPL_SAFETY_LEVEL_LOW_SAFE (Safety level 0), \n
   *                            =SPL_SAFETY_LEVEL_MEDIUM_SAFE (Safety level 2), \n
   *                            =SPL_SAFETY_LEVEL_HIGH_SAFE (Safety level 4) \n
   *
   *      @param [in] IdleTime  Time in ms, this time will be sent to the target.
   *                            If the target does not receive a data or idle telegram within this time,
   *                            the target will disconnect the connection, i.e. the station must send
   *                            data or idle telegrams frequently enough, that the target will receive
   *                            them within this time.
   *                            Depending on the compile option SPL_OPTION_IDLE_BYAPP,
   *                            the application must call SPL_sendIdle to send Idle telegrams
   *                            if it does not send data (option set) or call
   *                            SPL_checkIdle (option not set) at the end of an application cycle,
   *                            to let the Profibus Safety layer library check, if an idle
   *                            telegram has to be sent.
   *
   *      @param [in] pProtocolParameters Structure with Profibus Safety Layer protocol
   *                            parameters for P2P connections as described above.
   *                            If the parameter is 0U the default values will be used.
   *
   *      @param [out] pP2PConnection  Pointer to structur with Point-to-Point-Conection data.
   *
   *      @param [in]  Token  Input token value.
   *
   *      @return      Token  Output token value. Inverse value of input token.
   *
   *      @return pP2PConnection A valid SPL_Connection object pointer on success. 0U on any error.
   *
   *****************************************************************************/
  const int32_t
    SPL_SIM_Create_P2P(const uint32_t myProfibusAddress,
      const uint8_t* pName,
      const uint32_t TargetProfibusAddress,
      const uint32_t SAP,
      const uint32_t isMaster,
      const uint32_t SafetyLevel,
      const uint32_t IdleTime,
      const SPL_P2P_Protocol_Parameters * const pProtocolParameters,
      SPL_Connection * const pP2PConnection,
      const int32_t token)
  {
    OteLib_SPL_Connection::ConnectionMode m = isMaster ? OteLib_SPL_Connection::P2P_Master : OteLib_SPL_Connection::P2P_Slave;

    // The objects created here needs to be deleted by someone, its done by calling removeSplConnections. If not we get a memory leak.
    OteLib_SPL_Connection *tmp = new OteLib_SPL_Connection(
      (const char_t *)pName,
      myProfibusAddress,
      TargetProfibusAddress,
      SAP,
      m,
      SafetyLevel,
      IdleTime,
      (void*)pProtocolParameters,
      NULL);

    *pP2PConnection = (SPL_Connection)tmp;

    // Add this connection to the list that OTEDllInterface keeps so it can put messages to the queue for this connection.
    // Also needed so we can delete the objects later.
    oteDllSplStub_addSplConnection(tmp);

    return ~token;
  }

  /******************************************************************************
  * SPL_SIM_Get_P2PDataB
  ******************************************************************************/
  int32_t
    SPL_SIM_Get_P2PDataB(const uint32_t /*myProfibusAddress*/,
      SPL_Connection      hConnection,
      uint32_t* const TargetProfibusAddress,
      uint32_t* const SAP,
      uint32_t* const Master,
      uint32_t* const SafetyLevel,
      uint32_t* const IdleTime,
      SPL_P2P_Protocol_ParametersB* const /*pProtocolParameters*/)
  {
    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;

    *TargetProfibusAddress = ~c->getTargetProfibusAddress();
    *SAP = ~c->getSAP();
    *Master = ~c->getIsMaster();
    *SafetyLevel = ~c->getSafetyLevel();
    *IdleTime = ~c->getIdleTime();
    //*pProtocolParameters  // Not implemented yet. Data needs to be retreived and inverted if this feature is at all needed.

    return ~SPL_OK;
  }

  /******************************************************************************
  * SPL_SIM_Create_MCast
  ******************************************************************************/
  const int32_t
    SPL_SIM_Create_MCast(const uint32_t myProfibusAddress,
      const uint8_t* pName,
      const uint32_t SAP,
      const uint32_t Receiver, // 0 = sender, 1 receiver
      const uint32_t SafetyLevel,
      const SPL_MCast_Protocol_Parameters * const pProtocolParameters,
      SPL_Connection * const pMCConnection,
      const int32_t token)
  {
    // This code is not tested at all!

    OteLib_SPL_Connection::ConnectionMode m = (Receiver == 0) ? OteLib_SPL_Connection::MCAST_Sender : OteLib_SPL_Connection::MCAST_Receiver;

    // The objects created here needs to be deleted by someone, its done by calling removeSplConnections. If not we get a memory leak.
    OteLib_SPL_Connection *tmp = new OteLib_SPL_Connection(
      (const char_t *)pName,
      myProfibusAddress,
      Receiver,
      SAP,
      m,
      SafetyLevel,
      0,
      (void*)pProtocolParameters,
      NULL);

    //*pP2PConnection=(SPL_Connection)(tmp->getPP2PConnection());
    *pMCConnection = (SPL_Connection)tmp;

    // Add this connection to the list that OTEDllInterface keeps so it can put messages to the queue for this connection.
    // Also needed so we can delete the objects later.
    oteDllSplStub_addSplConnection(tmp);

    return ~token;
  }



  /******************************************************************************
  * SPL_SIM_Bus_Management
  ******************************************************************************/
  /**
   *  Retreive Profibus telegrams from Profibus driver
   *           and dispatch them to Profibus Manager FIFOs.
   *
   *      @param [out] pResult  Pointer to variable for function return value.
   *
   *      @return Result        =SPL_BUS_OK on success, <br>
   *                            =SPL_BUS_FDL_ERROR on FDL error, <br>
   *                            =SPL_BUS_UNEXPECTED as warning on unexpected telegrams, <br>
   *                            =SPL_BUS_FDL_FIFO for receiving overflow FDL. <br>
   *
   *      @param [in]  Token  Input token value.
   *
   *      @return      Token  Output token value. Inverse value of input token.
   *
   ******************************************************************************/
  const int32_t
    SPL_SIM_Bus_Management(const uint32_t /*myProfibusAddress*/,
      int32_t * const pResult,
      const int32_t token)
  {
    // For the time being doing nothing here.

    *pResult = SPL_BUS_OK;
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Clock_Management
  ******************************************************************************/
  const int32_t
    SPL_SIM_Clock_Management(const uint32_t /*myProfibusAddress*/,
      int32_t * const pResult,
      const int32_t token)
  {
    Profibus_Type profibusData;
    const uint16_t lengthWithoutSplHeader = 0U;
    const uint16_t lengthWithSplHeader = lengthWithoutSplHeader + splHeaderSize;

    memset(&profibusData, 0, sizeof(Profibus_Type));
    profibusData.SAP_no = 32U;
    VFW_Buffer mvbBuffer;
    vfwInitBuffer(&mvbBuffer, &profibusData.Profibus_Data[0], lengthWithSplHeader); // Length is 2 bytes..
                                                                                    // Fill in a fake header
    vfwPutU16(&mvbBuffer, lengthWithoutSplHeader);
    // This was hard coded 2, It this looks like it can be HW address of ATPCU CORE (because it is a number 2).
    vfwPutU8(&mvbBuffer, 2);
    // This was hard coded 20, If the one above is source then this is destination HW address. Or is it the other way around?
    vfwPutU8(&mvbBuffer, 20); 
    vfwPutU8(&mvbBuffer, profibusData.SAP_no);
    vfwPutU8(&mvbBuffer, profibusData.SAP_no);
    vfwPutU8(&mvbBuffer, 2U);      // Or is this the HW addres of ATPCU CORE 


    _IOsetProfibusOutputData(&profibusData);


    *pResult = 0;
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_isActive
  ******************************************************************************/
  /**
   * Get the status of an existing connection (SL and FDL).
   * Process received idle and disconnect telegrams for P2P connections.
   *
   *      @param [in]  hConnection  A pointer to a valid SPL_Connection object,
   *                            returned by SPL_Create_P2P or SPL_Create_MCast.
   *
   *      @param [in]  CycleTime  Maximum time since telegrams were read last
   *                     time from bus, good estimation is application cycle time.
   *
   *      @param [out] pResult  Pointer to variable for function return value.
   *
   *      @param [in]  Token  Input token value.
   *
   *      @return      Token  Output token value. Inverse value of input token.
   *
   *      @return pResult     Status of connection, coded as below:
   *                          =SPL_IS_BAD_OBJECT <br>
   *                          =SPL_IS_IDLE <br>
   *                          =SPL_IS_CONNECTED <br>
   *                          =SPL_IS_CONNECTING <br>
   *                          =SPL_IS_CLOSED <br>
   *                          =SPL_IS_CLOSED_2 <br>
   *                          =SPL_IS_SAP_ERROR <br>
   *                          =SPL_IS_DRIVER_ERROR <br>
   *
   ***************************************************************************/
   // IsActive is not a good name since it returns status not just active or not. Also this function have a side effect. 
  const int32_t
    SPL_SIM_IsActive(const uint32_t /*myProfibusAddress*/,
      SPL_Connection hConnection,
      uint32_t /*cycleTime*/,
      int32_t * const pResult,
      const int32_t token)
  {
    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;

    *pResult = c->getState();

    // Is needed here?
    if ((*pResult == SPL_IS_CLOSED) || (*pResult == SPL_IS_CLOSED_2))
    {
      c->resetQueue();
    }

    return ~token;
  }




  /******************************************************************************
  * SPL_SIM_Get_Reference_Time
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Reference_Time(const uint32_t /*myProfibusAddress*/,
      uint64_t * const pReferenceTime,
      int32_t* const pResult,
      const int32_t token)
  {
    const uint32_t refTimeA = static_cast<uint32_t>(vfwGetReferenceTime());

    *pReferenceTime = refTimeA;

    *pResult = 0;
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Get_Clock_Status
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Clock_Status(const uint32_t /*myProfibusAddress*/,
      int32_t * const pStatus,
      const int32_t token)
  {
    *pStatus = SPL_CLOCK_SYNCHRONIZED;
    return ~token;
  }

  //****************************************************************************
  void
    SPL_SIM_Get_Clock_StatusA(const uint32_t myProfibusAddress,
      int32_t * const pResult)
  {
    int32_t token = 0xDEAD;
    SPL_SIM_Get_Clock_Status(myProfibusAddress, pResult, token);
  }



  /******************************************************************************
  * SPL_SIM_Get_Reason
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Reason(const uint32_t /*myProfibusAddress*/,
      SPL_Connection /*TheConnection*/,
      uint8_t* const /*Reason*/,
      uint8_t* const /*Buffer*/,
      const uint32_t /*BufferSize*/,
      int32_t * const pResult,
      const int32_t token)
  {
    *pResult = 0;
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Get_Status
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Status(const uint32_t /*myProfibusAddress*/,
      uint64_t * const pStatus,
      const int32_t token)
  {
    // Not implemented, just give zero.
    *pStatus = 0;
    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Get_Connection_Status
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Connection_Status(const uint32_t myProfibusAddress,
      SPL_Connection hConnection,
      uint64_t * const pStatus,
      const int32_t token)
  {
    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;
    assert(myProfibusAddress == c->getSourceProfibusAddress());

    *pStatus = 0;

    if ((c->getState() != SPL_IS_CONNECTED) && (c->getState() != SPL_IS_CONNECTING))
    {
      /*
       *      @return pResult        Error bit mask as described in spl_a_error_codes,
       *                            Combination of SPL_GETS_CON_-error bit masks.
       */
       // TODO: Need to understand the comment above, what are the expected codes to be returned? just guessing here.
      *pStatus = static_cast<uint64_t>(SPL_RECV_BAD_OBJECT);
    }

    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_Get_Livellist
  ******************************************************************************/
  const int32_t
    SPL_SIM_Get_Livellist(const uint32_t /*myProfibusAddress*/,
      uint8_t* const /*Buffer*/,
      int32_t * const pResult,
      const int32_t Token)
  {
    *pResult = -1; // Not Supported
    return ~Token;
  }


  /******************************************************************************
  * SPL_SIM_Connect
  ******************************************************************************/
  /**
   * Open an existing SPL P2P Connection.
   *
   *      @param [in]  hConnection  A valid SPL_Connection object,
   *                                    returned by SPL_Create_P2P
   *
   *      @param [out] pResult  Pointer to variable for function return value.
   *
   *      @param [in]  Token  Input token value.
   *
   *      @return      Token  Output token value. Inverse value of input token.
   *
   *      @return pResult       =SPL_CONNECT_OK: If the function call was successfull.
   *                            It does *NOT* imply that the connection is Connected (this
   *                            must be checked with the SPL_IsActive function). <br>
   *                            Error codes are as follows: <br>
   *                            =SPL_CONNECT_BAD_OBJECT <br>
   *                            =SPL_CONNECT_BAD_STATE <br>
   *                            =SPL_CONNECT_FINAL_DISC <br>
   *                            =SPL_CONNECT_FDL_ERROR <br>
   *                            =SPL_CONNECT_FDL_ERROR <br>
   *
   *
   ***************************************************************************/
  const int32_t
    SPL_SIM_Connect(const uint32_t myProfibusAddress,
      SPL_Connection hConnection,
      int32_t* const pResult,
      const int32_t token)
  {
    // OTE2014DLL: We need to send a message to the other end 


    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;
    assert(myProfibusAddress == c->getSourceProfibusAddress());

    *pResult = SPL_CONNECT_OK;
    c->setState(SPL_IS_CONNECTED);



    return ~token;

  }


  /******************************************************************************
  * SPL_SIM_Disconnect
  ******************************************************************************/
  const int32_t
    SPL_SIM_Disconnect(const uint32_t myProfibusAddress,
      SPL_Connection hConnection,
      const int32_t /*New_Setup_Desired*/,
      const uint8_t* const /*Disconnect_Reason_Text*/,
      int32_t * const pResult,
      const int32_t token)
  {
    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;
    assert(myProfibusAddress == c->getSourceProfibusAddress());

    c->setState(SPL_IS_CLOSED);
    *pResult = SPL_DISCON_OK;

    // It seems connection shall be deleted also. So will do that. This code is not tested, not att all!
    oteDllSplStub_removeSplConnection(c);

    return ~token;
  }


  /******************************************************************************
  * SPL_SIM_recv
  ******************************************************************************/
  const int32_t
    SPL_SIM_recv(const uint32_t /*myProfibusAddress*/,
      SPL_Connection /*TheConnection*/,
      uint8_t* const pBuffer,
      const uint32_t BufferSize,
      unsigned long* const /*ReceptionTime*/,
      int32_t* const pResult,
      const int32_t token)
  {
    Profibus_Type profibusData;
    memset(&profibusData, 0, sizeof(Profibus_Type));
    const uint8_t readResult = _IOgetProfibusInputData(&profibusData);

    if (readResult == 1)
    {
      // OK
      VFW_Buffer mvbBuffer;
      vfwInitBuffer(&mvbBuffer, &profibusData.Profibus_Data[0], sizeof(&profibusData.Profibus_Data));
      vfwSetReadBuffer(&mvbBuffer, sizeof(&profibusData.Profibus_Data));

      const uint16_t lengthExcludingHeader = vfwGetU16(&mvbBuffer);

      if (lengthExcludingHeader > BufferSize)
      {
        *pResult = 0;
      }
      else
      {
        *pResult = lengthExcludingHeader;

        // Copy the profibus data to the provided buffer...
        memmove(pBuffer, &profibusData.Profibus_Data[splHeaderSize], static_cast<size_t>(*pResult));
      }
    }
    else
    {
      *pResult = 0;
    }

    return token;
  }




  /******************************************************************************
  * SPL_SIM_send
  ******************************************************************************/
  /**
   * Send data on a connection via vfw.
   *
   *      @param [in] hConnection  A pointer to avalid SPL_Connection object.
   *                            Returned by SPL_Create_P2P or SPL_Create_MCast.
   *
   *      @param [in]  pBuffer       A pointer to some readable buffer
   *                                  of at least 'BufferSize' bytes.
   *
   *      @param [in] BufferSize     Size of 'Buffer'.
   *
   *      @param [out] pResult  Pointer to variable for function return value.
   *
   *      @param [in]  Token  Input token value.
   *
   *      @return      Token  Output token value. Inverse value of input token.
   *
   *      @return pResult       =SPL_SEND_OK: If the function call was successfull. \n
   *                            =SPL_SEND_BAD_OBJECT: Error, bad hConnection parameter. \n
   *                            =SPL_SEND_BAD_STATE: Connection is in a state, where no telegram can be sent. \n
   *                            =SPL_SEND_BAD_SIZE: Buffersize > maximum data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)). \n
   *                            =SPL_SEND_FDL_FIFO: FDL FIFO for sending overflow. \n
   *                            =SPL_SEND_FDL_ERROR: other FDL driver error on sending. \n
   *                            =SPL_AB_ERROR: A and B code comparison error in diversified version. \n
   *
   ***************************************************************************/
  const int32_t
    SPL_SIM_send(const uint32_t myProfibusAddress,
      SPL_Connection hConnection,
      const uint8_t* const pBuffer,
      const uint32_t BufferSize,
      int32_t * const pResult,
      const int32_t token)
  {

    OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)hConnection;
    assert(myProfibusAddress == c->getSourceProfibusAddress());

    if ((c != NULL) &&
      (c->getState() == SPL_IS_CONNECTED) &&
      (BufferSize <= 242U))
    {
      Profibus_Type profibusData;
      const uint32_t lengthWithSplHeader = BufferSize + splHeaderSize;

      memset(&profibusData, 0, sizeof(Profibus_Type));
      profibusData.SAP_no = static_cast<uint8_t>(c->getSAP());
      VFW_Buffer mvbBuffer;
      vfwInitBuffer(&mvbBuffer, &profibusData.Profibus_Data[0], lengthWithSplHeader);
      // Fill in a fake header
      vfwPutU16(&mvbBuffer, static_cast<uint16_t>(BufferSize));
      // This was hard coded 2, It this looks like it can be HW address of ATPCU CORE (because it is a number 2).
      vfwPutU8(&mvbBuffer, static_cast<uint8_t>(c->getSourceProfibusAddress()));
      // This was hard coded 20, If the one above is source then this is destination HW address. Or is it the other way around?
      vfwPutU8(&mvbBuffer, static_cast<uint8_t>(c->getTargetProfibusAddress()));
      vfwPutU8(&mvbBuffer, profibusData.SAP_no);
      vfwPutU8(&mvbBuffer, profibusData.SAP_no);
      vfwPutU8(&mvbBuffer, 2U);      // Or is this the HW address of ATPCU CORE 

      // Now copy the data
      vfwCpyFromRawBuffer(&mvbBuffer, pBuffer, BufferSize);

      _IOsetProfibusOutputData(&profibusData);
    }
    else
    {
      *pResult = SPL_SEND_BAD_OBJECT;
    }

    return ~token;
  }



  /******************************************************************************
  * SPL_SIM_sendIdle
  ******************************************************************************/
  const int32_t
    SPL_SIM_sendIdle(const uint32_t /*myProfibusAddress*/,
      SPL_Connection /*TheConnection*/,
      int32_t * const pResult,
      const int32_t token)
  {
    //OteLib_SPL_Connection *c = (OteLib_SPL_Connection*)TheConnection;
    //assert(/*myProfibusAddress*/==c->getSourceProfibusAddress());

    *pResult = 0;
    return ~token;

  }


  //****************************************************************************
  void
    SPL_SIM_Get_Reference_TimeA(const uint32_t myProfibusAddress,
      uint64_t * const pReferenceTime,
      int32_t* const pResult)
  {
    const int32_t token = 0;

    SPL_SIM_Get_Reference_Time(myProfibusAddress,
      pReferenceTime,
      pResult,
      token);
  }




  /******************************************************************************
   * SPL_SIM_MirrorBuffer
   ******************************************************************************/
  void SPL_SIM_MirrorBuffer(spBYTE* const DstBuffer, const spBYTE* const SrcBuffer, const uint32_t Length)
  {
    spBYTE orgByte;
    spBYTE mirByte;
    spUINT i;
    spUINT j;

    // Make sure to support overlapping source and destination buffers
    spBYTE *pSrcBufferCopy = new spBYTE[Length];

    //ASSERT (DstBuffer != 0U);
    //ASSERT (SrcBuffer != 0U);
    for (i = 0; i < Length; i++)
    {
      pSrcBufferCopy[i] = SrcBuffer[i];
    }

    for (i = 0; i < Length; i++)
    {
      mirByte = 0;
      orgByte = pSrcBufferCopy[i];
      for (j = 0; j < 8; j++)
      {
        mirByte = (spBYTE)(mirByte << 1) | (orgByte & 0x01);
        orgByte = orgByte >> 1;
      }
      DstBuffer[(Length - 1) - i] = mirByte;
    }

    delete[] pSrcBufferCopy;
  }

  /******************************************************************************
   * SPL_SIM_InvertBuffer
   ******************************************************************************/
  void SPL_SIM_InvertBuffer(spBYTE* const DstBuffer, const spBYTE* const SrcBuffer, const uint32_t Length)
  {
    spUINT i;

    //ASSERT (DstBuffer != 0U);
    //ASSERT (SrcBuffer != 0U);

    for (i = 0; i < Length; i++)
    {
      DstBuffer[i] = ~SrcBuffer[i];
    }


  }

  /******************************************************************************
   * SPL_SIM_Get_Version
   ******************************************************************************/
  void SPL_SIM_Get_Version(const uint32_t /*myProfibusAddress*/, SPL_Version * const Version)
  {
    if (Version != NULL)
    {
      // Make sure to make all fields into strings.
      Version->Version_String[0] = 0;
      Version->SIL[0] = 0;
      Version->Driver[0] = 0;
      Version->AB_CODE[0] = 0;
      Version->Debug[0] = 0;

      strncat(reinterpret_cast<char_t *>(Version->Version_String), "SPLSim", MAX_VERSION_STRING);
      strncat(reinterpret_cast<char_t *>(Version->SIL), "SPLSim", MAX_SIL_STRING);
      strncat(reinterpret_cast<char_t *>(Version->Driver), "SPLSim", MAX_DRIVER_STRING);
      strncat(reinterpret_cast<char_t *>(Version->AB_CODE), "SPLSim", MAX_AB_CODE_STRING);
      strncat(reinterpret_cast<char_t *>(Version->Debug), "SPLSim", MAX_DEBUG_STRING);
    }
  }
} // extern "C"

/*************************** end of file **************************************/

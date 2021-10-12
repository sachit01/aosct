#ifndef BasicIP_hpp
#define BasicIP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class implements the application- and project-specific adaptations
*  for BasicIP
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-13    bhermans    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-27    adgupta     Implementation of BasicIP functionality
* 2016-09-08    adgupta     Implementation of console call functionality
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    bhermans    Moved implementation of bip console-command to core 
* 2016-09-20    spandita    Added Connection ID for OPC sim
* 2016-09-28    adgupta     Added Connection ID for BTM Handler
* 2016-12-16    saprasad    Added Connection ID for Analyzer Interface
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_basic_ip.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  /**
  * The class BasicIP inherits from the abstract class and implements
  * the interfaces needed.
  *
  */
  class BasicIP : public ATC::AbstractBasicIP
  {
  public:
    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static BasicIP& instance(void);

    /** Connection ID for Analyzer IF ,so that AOS Analyzer will connect to ATP */
    static const uint8_t connectionAnalyzerIF = maxCoreConnections;
#ifndef _SIL
    /** number of total connections available */
    static const uint8_t connectionMax = maxCoreConnections + 1U;
#else
    //
    // Connection Id(s) for the simulated VFW-channels. Connection Ids start after the Core and adaptation connection Ids.
    //

    // Connection simulating the vfw-channels to/from RadioChannel 1
    static const uint8_t connectionSimTCC1 = maxCoreConnections + 1U;
    // simulating the vfw-channels to/from RadioChannel 2
    static const uint8_t connectionSimTCC2 = maxCoreConnections + 2U;
    // simulating the vfw-channels to/from RadioChannel 3
    static const uint8_t connectionSimTCC3 = maxCoreConnections + 3U;
    // simulating the vfw-channels to/from MMIChannel 1
    static const uint8_t connectionSimMMI1 = maxCoreConnections + 4U;
    // simulating the vfw-channels to/from MMIChannel 2 (Not yet supported in SIL)
    static const uint8_t connectionSimMMI2 = maxCoreConnections + 5U;
    // simulating the vfw-channel for 'Config' from Odometry to COD
    static const uint8_t connectionSimOdometryConfig = maxCoreConnections + 6U;
    // simulating the vfw-channel for 'Config response' to Odometry from COD
    static const uint8_t connectionSimOdometryConfigResponse = maxCoreConnections + 7U;
    // simulating the vfw-channel for 'Measurements' to Odometry from COD
    static const uint8_t connectionSimOdometryMeas = maxCoreConnections + 8U;
    // simulating the vfw-channel for 'Config' from Odometry to COD
    static const uint8_t connectionSimCODConfig = maxCoreConnections + 9U;
    // simulating the vfw-channel for 'Config response' to Odometry from COD
    static const uint8_t connectionSimCODConfigResponse = maxCoreConnections + 10U;
    // simulating the vfw-channel for 'Measurements' to Odometry from COD
    static const uint8_t connectionSimCODMeas = maxCoreConnections + 11U;
    // simulating the vfw-channel to CODSim
    static const uint8_t connectionSimCODSim = maxCoreConnections + 12;
    // simulating the vfw-channel to VIOHSim
    static const uint8_t connectionSimVIOHSim = maxCoreConnections + 13U;
    // simulating the vfw-channel to OPCSim
    static const uint8_t connectionSimOPCSimBTMCommand = maxCoreConnections + 14U;
    // simulating the vfw-channel from OPCSim to ATP for BTM telegram Messages
    static const uint8_t connectionSimOPCSimBTMTelegram = maxCoreConnections + 15U;
    /// simulating the vfw-channel to OPCSim
    static const uint8_t connectionSimBTMHandlerBTMCommand = maxCoreConnections + 16U;
    // simulating the vfw-channel from OPCSim to ATP for BTM telegram Messages
    static const uint8_t connectionSimBTMhandlerTelegram  = maxCoreConnections + 17U;
    // simulating the vfw-channel from OPCSim to ATP for Application Status
    static const uint8_t connectionSimBTMhandlerAppStatus = maxCoreConnections + 18U;
    // simulating the vfw-channel from ATP to OPCSim for Application Status
    static const uint8_t connectionSimBTMhandlerToOpcAppStatus = maxCoreConnections + 19U;
    // simulating the vfw-channel from ATP to OPCSim for Clock Sync
    static const uint8_t connectionSimBTMhandlerClockSync = maxCoreConnections + 20U;
    // simulating the vfw-channel to LCS
    static const uint8_t connectionSimLCS                 = maxCoreConnections + 21U;
    // simulating the vfw-channel Tigris Offset
    static const uint8_t connectionSimBTMhandlerTigrisOffset = maxCoreConnections + 22U;
    // simulating the vfw-channel Tigris Offset
    static const uint8_t connectionSimBTMhandlerToOpcTigrisOffset = maxCoreConnections + 23U;
    // simulating the host type vfw-channel to OPCSim
    static const uint8_t connectionSimOPCSim = maxCoreConnections + 24U;
    // simulating the host type vfw-channel to OBRDSim
    static const uint8_t connectionSimOBRDSim = maxCoreConnections + 25U;
    // number of total connections available
    static const uint8_t connectionMax = maxCoreConnections + 26U;
#endif

    /**
    * Translate connection id to printable string
    *
    * @param[in] connectionId An id identifying the connection
    *
    * @returns the connection id as an abbreviated character string
    */
    virtual const char_t *connectionIdStr(const uint8_t connectionId);

  protected:

    /**
    * Get max connections available
    *
    * @returns the max connections available
    */
    virtual uint8_t getMaxConnections();

  private:
    /**
    * Allocate the control-blocks needed for the adaptation
    */
    ConnectionControlBlock connectionCCB[connectionMax];

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    BasicIP();

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    BasicIP(const BasicIP&);

    /**
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    BasicIP& operator = (const BasicIP&);
  };
}
#endif

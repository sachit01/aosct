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
*  This class implements the application and project-specific adaptations
*  for BasicIP
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed    Created
* 2016-11-16    adgupta  Updated after design
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_basic_ip.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace Dispatcher
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

    /**
    * Translate connection id to printable string
    *
    * @param[in] connectionId An id identifying the connection
    *
    * @returns the connection id as an abbreviated character string
    */
    virtual const char_t *connectionIdStr(const uint8_t connectionId);

    /**
    * Connection Id(s) for the connection from Dispatcher to external interfaces. Connection Ids start after the Core connection Ids.
    */
    static const uint8_t connectionTCC1           = maxCoreConnections;          //!< Connection to/from RadioChannel 1
    static const uint8_t connectionDMI1           = maxCoreConnections + 1U;     //!< Connection to/from DMIChannel 1
    static const uint8_t connectionIOSim          = maxCoreConnections + 2U;     //!< Connection to simulate Inputs and Outputs
    static const uint8_t connectionSpeedSim       = maxCoreConnections + 3U;     //!< Connection to simulate Speed
    static const uint8_t connectionTCC2           = maxCoreConnections + 4U;     //!< Connection to/from RadioChannel 2
    static const uint8_t connectionDMI2           = maxCoreConnections + 5U;     //!< Connection to/from DMIChannel 2
    static const uint8_t connectionLIG            = maxCoreConnections + 6U;     //!< Connection to/from LIG
    static const uint8_t connectionOPCClient      = maxCoreConnections + 7U;     //!< Connection to OPC
    static const uint8_t connectionOPCProfibusClient  = maxCoreConnections + 8U;     //!< Connection to OPC
    static const uint8_t connectionOPCServer      = maxCoreConnections + 9U;     //!< Connection from OPC
    static const uint8_t connectionOPCAppStatusServer = maxCoreConnections + 10U; //!< Connection from OPC
    static const uint8_t connectionTCC3           = maxCoreConnections + 11U;     //!< Connection to/from RadioChannel 3
    static const uint8_t connectionOPCSim         = maxCoreConnections + 12U;     //!< Connection to OPCSim
    static const uint8_t connectionOBRD           = maxCoreConnections + 13U;     //!< Connection OBRD BOS
    static const uint8_t connectionATP            = maxCoreConnections + 14U;     //!< Connection ATP
    static const uint8_t connectionMax            = maxCoreConnections + 15U;     //!< Maximum number of total connections available

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

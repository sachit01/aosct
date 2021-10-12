#ifndef Config_hpp
#define Config_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the Config adaptation class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 22-11-2016    saprasad    Created
* 15-12-2016    spandita    Added Config call for TCC 1 &2 IP
* 14-02-2017    saprasad    Added ConfigID for TCC(1-4) and TCC(1-4) function prototype

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_config_base.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace Dispatcher
{
  /**
  * Constant for Version number (Major) for the dispatcher binary file.
  */
  static const uint8_t dispVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the dispatcher binary file.
  */
  static const uint8_t dispVersionMinor = 0U;

  /**
 * Declaration of Config ID's for dispatcher
 */
  static const ATC::ConfigIdType tcc1PortId       = 500U;  //!< TCC1 (Central) will connect to this port number
  static const ATC::ConfigIdType tcc2PortId       = 501U;  //!< TCC2 (Region 1) will connect to this port number
  static const ATC::ConfigIdType tcc3PortId       = 502U;  //!< TCC3 (Region 2) will connect to this port number

  static const ATC::ConfigIdType dmi1IpId         = 504U;  //!< DMI1 IP address
  static const ATC::ConfigIdType dmi2IpId         = 505U;  //!< DMI2 IP address
  static const ATC::ConfigIdType dmiPortId        = 506U;  //!< Dispatcher will connect DMI with DMI  Port number
  static const ATC::ConfigIdType ligPortId        = 507U;  //!< LIG Port ID
  static const ATC::ConfigIdType ligIpId          = 508U;  //!< LIG IP address
  static const ATC::ConfigIdType remoteOpcIpId    = 509U;  //!< Remote OPC IP address
  static const ATC::ConfigIdType remoteOpcPortId  = 510U;  //!< Remote OPC Port (server on OPC)
  static const ATC::ConfigIdType localOpcPortId   = 511U;  //!< OPC Port (server on AOS)
  static const ATC::ConfigIdType obrdPortId       = 512U;  //!< Port for OBRD interface

  /**
  * The class Config instantiates the abstract class and implements
  * the interfaces needed for both inherited classes and component.
  *
  */
  class Config : public ATC::AbstractConfigBase
  {
  public:

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static Config& instance(void);

    /**
    * Implements the virtual preInit function.
    *
    */
    virtual void preInit(void);

    /**
    * Implements the init function.
    * The Dispatcher specific config items are added here.
    *
    * @return Returns true when initialization completed
    */
    virtual bool init(void);

    /**
    * Get the DMI Port number
    * This in turn calls the relevant getconfig function
    *
    * @return DMI Port number
    */
    uint16_t getDMIPort() const;

    /**
    * Get the TCC1 Port number
    * This in turn calls the relevant getconfig function
    *
    * @return TCC1 Port number
    */
    uint16_t getTCC1Port() const;

    /**
    * Get the TCC2 Port number
    * This in turn calls the relevant getconfig function
    *
    * @return TCC2 Port number
    */
    uint16_t getTCC2Port() const;

        /**
    * Get the TCC3 Port number
    * This in turn calls the relevant getconfig function
    *
    * @return TCC3 Port number
    */
    uint16_t getTCC3Port() const;

    /**
    * Get the DMI1 IP address
    * This in turn calls the relevant getconfig function
    *
    * @return DMI1 IP address
    */
    const char_t* getDMI1Ip() const;

    /**
    * Get the DMI2 IP address
    * This in turn calls the relevant getconfig function
    *
    * @return DMI2 IP address
    */
    const char_t* getDMI2Ip() const;
    
    /**
    * Get the LIG IP address
    * This in turn calls the relevant getconfig function
    *
    * @return LIG IP address
    */
    const char_t* getLIGIp() const;

    /**
    * Get the LIG Port number
    * This in turn calls the relevant getconfig function
    *
    * @return LIG Port number
    */
    uint16_t getLIGPort() const;

    /**
    * Get the remote OPC IP address
    * This in turn calls the relevant getconfig function
    *
    * @return Remote OPC IP address
    */
    const char_t* getRemoteOPCIp() const;

    /**
    * Get the remote OPC port number
    * This in turn calls the relevant getconfig function
    *
    * @return Remote OPC Port number
    */
    uint16_t getRemoteOPCPort() const;

    /**
    * Get the local OPC Port number
    * This in turn calls the relevant getconfig function
    *
    * @return Local OPC Port number
    */
    uint16_t getLocalOPCPort() const;

    /**
    * Get the OBRD interface port number
    * This in turn calls the relevant getconfig function
    *
    * @return OBRD Port number
    */
    uint16_t getOBRDPort() const;

  private:
    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    Config();

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    Config(const Config&);

    /**
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    Config& operator = (const Config&);
  };
}
#endif

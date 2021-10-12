/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* Each messageType (LCS->AOS) has an associated parser class inherited from LCSMessageIn.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-23    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_vehicle_com.hpp"
#include "abstract_lcs_message_in.hpp"
#include "log_handler.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    /******************************************************************************
    * AbstractLCSMessageIn Default Constructor
    ******************************************************************************/
    AbstractLCSMessageIn::AbstractLCSMessageIn() :
      implemented(false),
      messageType(LCSMTypeStatusMessage),
      dataProcessState(NoDataAvailable),
      messageVersion(0U)
    {
      trace = AbstractVehicleCom::corePtr()->getTrace();
    }


    /******************************************************************************
    * AbstractLCSMessageIn Constructor
    ******************************************************************************/
    AbstractLCSMessageIn::AbstractLCSMessageIn(const LCSMessageType mType, const uint8_t version, const bool isImplemented) :
      implemented(isImplemented),
      messageType(mType),
      dataProcessState(NoDataAvailable),
      messageVersion(version)
    {
      trace = AbstractVehicleCom::corePtr()->getTrace();
    }

    /******************************************************************************
    * AbstractLCSMessageIn destructor
    ******************************************************************************/
    AbstractLCSMessageIn::~AbstractLCSMessageIn()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }

    /******************************************************************************
    * logToRU
    ******************************************************************************/
    void AbstractLCSMessageIn::logToRU(const EmpMsg* const mData) const
    {
      ATC::AbstractLogHandler::corePtr()->logRU(
        LogHandler::Ifc_LCS, ATC::AbstractLogHandler::Ifc_In,
        mData->getEMPBuffer(), mData->getEMPMessageActualLen());
    }

    /******************************************************************************
    * getImplemented
    ******************************************************************************/
    bool AbstractLCSMessageIn::getImplemented() const
    {
      return implemented;
    }

    /******************************************************************************
    * getVersion
    ******************************************************************************/
    uint8_t AbstractLCSMessageIn::getVersion() const
    {
      return messageVersion;
    }

    /******************************************************************************
    * tracePublishData
    ******************************************************************************/
    void AbstractLCSMessageIn::tracePublishData(const bool publishOk) const
    {
      if (publishOk)
      {
        trace->write(ATC::veryDetailedTrace, "Publishing incoming data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Publishing incoming data failed");
      }
    }

    /******************************************************************************
    * traceParseData
    ******************************************************************************/
    void AbstractLCSMessageIn::traceParseData(const bool parseOk) const
    {
      if (parseOk)
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Parsing incoming data failed");
      }
    }

    /******************************************************************************
    * traceValidateMode
    ******************************************************************************/
    void AbstractLCSMessageIn::traceValidateMode(const bool validateModeOk) const
    {
      if (validateModeOk)
      {
        trace->write(ATC::veryDetailedTrace, "Validating mode Ok");
      }
      else
      {
        // veryDetailedTrace
        trace->write(ATC::veryDetailedTrace, "Validating mode failed");
      }
    }

    /******************************************************************************
    * getDataProcessState
    ******************************************************************************/
    DataProcessState AbstractLCSMessageIn::getDataProcessState() const
    {
      return dataProcessState;
    }

    /******************************************************************************
    * setDataProcessState
    ******************************************************************************/
    void AbstractLCSMessageIn::setDataProcessState(const DataProcessState newState)
    {
      dataProcessState = newState;
    }

    /******************************************************************************
    * setDataProcessState
    ******************************************************************************/
    const ATC::TraceInterface & AbstractLCSMessageIn::getTracer() const
    {
      return *trace;
    }
  }
}

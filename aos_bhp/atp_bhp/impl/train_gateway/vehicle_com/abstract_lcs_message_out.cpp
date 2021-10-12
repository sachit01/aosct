/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-24    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_vehicle_com.hpp"
#include "lcs_message_common.hpp"
#include "abstract_lcs_message_out.hpp"
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
    * AbstractLCSMessageOut Default Constructor
    ******************************************************************************/
    AbstractLCSMessageOut::AbstractLCSMessageOut() :
      implemented(false),
      messageType(LCSMTypeTrainStatusMessage),
      dataProcessState(NoDataAvailable),
      messageVersion(0U)
    {
      trace = AbstractVehicleCom::corePtr()->getTrace();
    }
    
    /******************************************************************************
    * AbstractLCSMessageOut Constructor
    ******************************************************************************/
    AbstractLCSMessageOut::AbstractLCSMessageOut(const LCSMessageType mType, const uint8_t version, const bool isImplemented) :
      implemented(isImplemented),
      messageType(mType),
      dataProcessState(NoDataAvailable),
      messageVersion(version)
    {
      trace = AbstractVehicleCom::corePtr()->getTrace();
    }

    /******************************************************************************
    * AbstractLCSMessageOut destructor
    ******************************************************************************/
    AbstractLCSMessageOut::~AbstractLCSMessageOut()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }

    /******************************************************************************
    * logToRU
    ******************************************************************************/
    void AbstractLCSMessageOut::logToRU(const EmpMsg* const mData) const
    {
      ATC::AbstractLogHandler::corePtr()->logRU(
        ATP::LogHandler::Ifc_LCS, ATC::AbstractLogHandler::Ifc_Out,
        mData->getEMPBuffer(), mData->getEMPMessageActualLen());
    }

    /******************************************************************************
    * getVersion
    ******************************************************************************/
    uint8_t AbstractLCSMessageOut::getVersion() const
    {
      return messageVersion;
    }

    /******************************************************************************
    * traceAssembleData
    ******************************************************************************/
    void AbstractLCSMessageOut::traceAssembleData(const bool assembleOk) const
    {
      if (assembleOk)
      {
        trace->write(ATC::veryDetailedTrace, "Assembling outgoing data Ok");
      }
      else
      {
        trace->write(ATC::veryDetailedTrace, "Assembling outgoing data failed");
      }
    }

    /******************************************************************************
    * getDataProcessState
    ******************************************************************************/
    DataProcessState AbstractLCSMessageOut::getDataProcessState() const
    {
      return dataProcessState;
    }

    /******************************************************************************
    * setDataProcessState
    ******************************************************************************/
    void AbstractLCSMessageOut::setDataProcessState(const DataProcessState newState)
    {
      dataProcessState = newState;
    }

    /******************************************************************************
    * setDataProcessState
    ******************************************************************************/
    ATC::TraceInterface& AbstractLCSMessageOut::getTracer()
    {
      return *trace;
    }

    /******************************************************************************
    * getMessageType
    ******************************************************************************/
    LCSMessageType AbstractLCSMessageOut::getMessageType() const
    {
      return messageType;
    }
  }
}

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc implemented.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-05    arastogi    Created
* 2016-06-09    arastogi    Updated based on the new design
* 2016-06-13    akushwah    Define the Operator = overloading
* 2016-06-20    saprasad    Implemented Event's functions,Lint fixes
* 2016-06-22    saprasad    Incoroparated Event Handler Review Comments
* 2016-10-12    arastogi    Removed the = operator for event. Event should not
*                           be assigned.
* 2016-10-19    nsyed       Re-design Event Types/ Levels
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_application_base.hpp"
#include "event.hpp"
#include "atc_util.hpp"
#include <vfw_string.h>
#include <stdio.h> 

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{

  /******************************************************************************
  * Default Constructor
  ******************************************************************************/
  Event::Event()
  {
    componentID = 0U;
    container = CommonContainer;
    eventNum = 0U;
    type = EventTypeLog;
    releaser = 0U;
    isDynamicTextSet = false;
    dmiEventCode = 0U;
    id = 0U;
    text[0] = '\0';
    dynamicText[0] = '\0';
  }

  /******************************************************************************
  * getWriteCrossCompareMaxSize
  ******************************************************************************/
  uint32_t Event::getWriteCrossCompareMaxSize() const
  {
    // These are the sizes written in writeCrossCompare() below
    // 4 extra for the length used in vfwString
    return 2U + 4U + 2U + 4U + 1U + 1U + 1U + 2U + 4U + sizeof(text) + 4U;
  }

  /******************************************************************************
  * writeCrossCompare
  ******************************************************************************/
  void Event::writeCrossCompare(VFW_Buffer* const buffer) const
  {
    vfwPutU16(buffer, componentID);
    vfwPutU32(buffer, static_cast<uint32_t>(container));
    vfwPutU16(buffer, eventNum);
    vfwPutU32(buffer, static_cast<uint32_t>(type));
    vfwPutU8(buffer, releaser);
    vfwPutU8(buffer, isDynamicTextSet ? 255U : 0U);
    vfwPutU16(buffer, dmiEventCode);
    vfwPutString(buffer, &text[0]);
    vfwPutU32(buffer, id);
  }

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  Event::Event(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const EventType eventType, const uint8_t rel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    componentID = cmpt;
    container = cntnr;
    eventNum = eventNr;
    type = eventType;
    releaser = rel;
    isDynamicTextSet = useDynamicText;
    dmiEventCode = dmi;
    id = 0U;

    memset(&text[0], 0, sizeof(text));
    static_cast<void>(vfw_strlcpy(&text[0], txt, sizeof(text)));

    memset(&dynamicText[0], 0, sizeof(dynamicText));

    uint32_t blockNumber = static_cast<uint32_t>(AbstractApplicationBase::corePtr()->getBlockNr());
    uint32_t cntNumber = static_cast<uint32_t>(cntnr);
    uint32_t cmptID = static_cast<uint32_t>(cmpt);
    uint16_t eventNumber = eventNum;

    //! Generate the event id using Block, Container, Component and Event number
    //! Spare = S, Block = B, Container = C, ComponenetID = I, EventNumber = E
    //! MSB                                                                             LSB
    //! [Spare| Block] [Block| Container| ComponenetID] [ComponenetID | EventNumber] [ EventNumber ] 
    //! SSSS SBBB BBCC IIII IIII EEEE EEEE EEEE */

    if (((cmptID >= minComponentID) && (cmptID <= maxComponentID)))
    {
      if ((eventNumber >= 1U) && (eventNumber <= maxEventNr))
      {
        id = ((blockNumber * blockOffsetMultiplier) + (cntNumber * containerOffsetMultiplier) + (cmptID * componentOffsetMultiplier) + eventNumber);

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
        // Create off-line event-information for events reported to TCC/MDCs
        registerEvent();
#endif
      }
    }
    else
    {
      // OUT of range values
      ATC::aosHalt(__FILE__, __LINE__, "Out of range event ID are generated");
    }
  }

  /******************************************************************************
  * createLogEvent
  ******************************************************************************/
  Event Event::createLogEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeLog, static_cast<uint8_t>(NoEB), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createSBReqEvent
  ******************************************************************************/
  Event Event::createSBReqEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const SBReleaser sbRel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeSBReq, static_cast<uint8_t>(sbRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createEBReqEvent
  ******************************************************************************/
  Event Event::createEBReqEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const EBReleaser ebRel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeEBReq, static_cast<uint8_t>(ebRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createStandstillEvent
  ******************************************************************************/
  Event Event::createStandstillEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr, const SBReleaser sbRel,
    const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeStandstill, static_cast<uint8_t>(sbRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createSafeBrakeSBEvent
  ******************************************************************************/
  Event Event::createSafeBrakeSBEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const SBReleaser sbRel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeSafeBrakeSB, static_cast<uint8_t>(sbRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createSafeBrakeEBEvent
  ******************************************************************************/
  Event Event::createSafeBrakeEBEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const EBReleaser ebRel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeSafeBrakeEB, static_cast<uint8_t>(ebRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * createSafetyHaltEvent
  ******************************************************************************/
  Event Event::createSafetyHaltEvent(const ComponentIDType cmpt, const ComponentContainers cntnr, const uint16_t eventNr,
    const EBReleaser ebRel, const DMIEventCode dmi, const char_t* const txt, const bool useDynamicText)
  {
    return Event(cmpt, cntnr, eventNr, EventTypeSafetyHalt, static_cast<uint8_t>(ebRel), dmi, txt, useDynamicText);
  }

  /******************************************************************************
  * getId
  ******************************************************************************/
  uint32_t Event::getId() const
  {
    return id;
  }

  /******************************************************************************
  * getComponentId
  ******************************************************************************/
  ComponentIDType Event::getComponentId() const
  {
    return componentID;
  }

  /******************************************************************************
  * getComponentContainer
  ******************************************************************************/
  ComponentContainers Event::getComponentContainer() const
  {
    return container;
  }

  /******************************************************************************
  * getComponentContainer
  ******************************************************************************/
  DMIEventCode Event::getDmiEventCode() const
  {
    return dmiEventCode;
  }

  /******************************************************************************
  * getEventNum
  ******************************************************************************/
  uint16_t Event::getEventNum() const
  {
    return eventNum;
  }

  /******************************************************************************
  * getReleaser
  ******************************************************************************/
  uint8_t Event::getReleaser() const
  {
    return releaser;
  }

  /******************************************************************************
  * getText
  ******************************************************************************/
  const char_t* Event::getText() const
  {
    return text;
  }

  /******************************************************************************
  * getType
  ******************************************************************************/
  Event::EventType Event::getType() const
  {
    return type;
  }

  /******************************************************************************
  * getDynamicText
  ******************************************************************************/
  const char_t* Event::getDynamicText() const
  {
    const char_t* t = dynamicText;
    if (*t == '\0')
    {
      t = static_cast<char_t*>(NULL);
    }
    return t;
  }

  /******************************************************************************
  * setDynamicText
  ******************************************************************************/
  void Event::setDynamicText(const char_t* const dynamicTextToBeSet) const
  {
    if (isDynamicTextSet)
    {
      if (dynamicTextToBeSet != NULL)
      {
        strncpy(&dynamicText[0], dynamicTextToBeSet, sizeof(dynamicText));
        dynamicText[sizeof(dynamicText) - 1U] = '\0';
      }
      else
      {
        dynamicText[0U] = '\0';
      }
    }
  }

  /******************************************************************************
  * setDynamicText
  ******************************************************************************/
  void Event::setDynamicText(const uint32_t valueToSet) const
  {
    if (isDynamicTextSet)
    {
      //lint -e{586} snprintf is needed here
      const int32_t res = snprintf(&dynamicText[0], sizeof(dynamicText), "%u", valueToSet);

      if ((res < 0) || (static_cast<size_t>(res) >= sizeof(dynamicText)))
      {
        ATC::aosHalt(__FILE__, __LINE__, "Could not set dynamic text");
      }
    }
  }

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
  /******************************************************************************
  * registerEvent
  ******************************************************************************/
  void Event::registerEvent() const
  {
    std::ofstream eventFile(registerEventCsvFile, std::ios::app);
    eventFile << id << ";" << componentID << ";" << container << ";" << eventNum << ";" << type << ";" << text <<
      ";" << (isDynamicTextSet ? 1 : 0) << "\n";
    eventFile.close();
  }
#endif

}

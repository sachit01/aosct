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
* 2016-03-23    ljoars      Created
* 2016-04-26    lantback    Renamed "only" instance, corrected namespace and init()
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-06-02    arastogi    Moved it to position folder
* 2016-09-28    saprasad    Clean up the unused code 
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "decode.hpp"

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
  namespace Pos
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    Decode::Decode() : AbstractDecode()
    {
      // for satisfy lint
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be usefull)
    *
    ******************************************************************************/
    Decode& Decode::instance()
    {
      static Decode theOnlyDecodeInstance;

      return theOnlyDecodeInstance;
    }

    /******************************************************************************
    * isRedBalise 
    * Bhp adaptation of the red balise detection. Use (bhpRedBaliseNID_C,bhpRedBaliseNID_BG) for Bhp.
    ******************************************************************************/
    bool Decode::isRedBalise(const uint16_t nid_c, const uint16_t nid_bg)
    {
      return (redBaliseNidC == nid_c) && (redBaliseNidBg == nid_bg);
    }
  }
}

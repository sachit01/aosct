#ifndef NormalMode_hpp
#define NormalMode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the Normal mode.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-03    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * The class NormalMode defines the Normal atp mode.
    *
    */
    class NormalMode : public AbstractMode
    {
    public:

      /**
      * Main run function of the mode.
      * @param[in] commonData   reference to train states
      */
      virtual void handleMode(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      NormalMode();

      /**
      * Virtual function for name of the mode.
      *
      * @return ATPModeNormal enum value.
      */
      virtual ATPMode getModeId();

      /**
      * Virtual Function for Q route type check in Normal mode
      */
      virtual bool isValidQRouteType(const RouteType routeType) const;

      /**
      * To get the status of validity of unconditional shortening message in normal mode
      *
      * @return true if the unconditional shortening message is valid in the normal mode
      */
      virtual bool isValidUncondShorteningMsg() const;

    private:

      /**
      * To handle mode change in Normal mode
      *
      * @param[in] mode   ATP mode to change
      * @param[in] commonData   reference to train states
      *
      * @return   true   if mode need to changed
      */
      bool handleModeChangeRequest(ATPMode& mode, CommonDataForModes& commonData) const;

    };
  }
}

#endif

#ifndef TrainSetup_hpp
#define TrainSetup_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class declares the Train setup as it is stored by TSetup
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-17   bhermans    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "radio_message_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * The class TrainSetupChangeDetails declares the change details of a train setup
    */
    class TrainSetupChangeDetails
    {
    public:
      bool        brakeResponseTime;//!< Brake response time changed for this train
      bool        brakeAbility;     //!< Brakeability changed for this train
    };

    /**
    * The class TrainSetup declares the train setup as it is stored by TSetup
    */
    class TrainSetup
    {
    public:
      TrainSetup();

      /**
      * Set all fields to default values 
      */
      virtual void invalidate();

      /**
      * Virtual destructor
      */
      virtual ~TrainSetup();

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      uint16_t    maxSpeed;         //!< Max allowed speed for this train
      uint32_t    length;           //!< Length of this train  
      uint8_t     orientation;      //!< Orientation (only bit2 LocoOrientation used in B_DIRECTION) 
      bool        timsSupNeeded;    //!< TIMS supervision needed 
      uint16_t    vehicleCount;     //!< Number of vehicles in this train, including leading loco
      TrainSetupChangeDetails changeDetails;    //!< Change details for this train setup
      TrainSetupState state;  //!< Q_TS_STATE parameter (permanent or temporary)      
    };
  }
}

#endif

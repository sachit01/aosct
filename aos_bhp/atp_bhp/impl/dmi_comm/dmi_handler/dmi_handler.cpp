/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The DMI Handler component deals with managing interaction between ATP and DMI
*  via DMI channels. It also provides other components a way to send required
*  information to the DMI and collect informations/data from the DMI. It creates,
*  manages and owns the DMI channels over which all the communications to DMI
*  takes place.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-29    akushwah     Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "dmi_handler.hpp"
#include "channel_config.hpp"
#include "dmi_message_in_dmi_status.hpp"
#include "dmi_message_in_dmi_to_atp_data.hpp"
#include "dmi_message_in_driver_id_and_password.hpp"
#include "dmi_message_in_loco_vs_train_dir.hpp"
#include "dmi_message_in_confirmation.hpp"
#include "dmi_message_in_vehicle_data.hpp"
#include "dmi_message_in_train_vs_track_dir.hpp"
#include "dmi_message_in_dmi_startup.hpp"
#include "dmi_message_in_registration_area.hpp"
#include "dmi_message_in_train_name.hpp"
#include "dmi_message_in_train_loaded.hpp"
#include "dmi_message_out_atp_modes_and_states_bhp.hpp"
#include "dmi_message_out_vehicle_data.hpp"
#include "dmi_message_out_ceiling_speed_list.hpp"
#include "dmi_message_out_gradient_data_list.hpp"
#include "dmi_message_out_driver_info.hpp"
#include "dmi_message_out_erase_planning_area.hpp"
#include "dmi_message_out_loco_vs_train_dir.hpp"
#include "dmi_message_out_manual_config_selected.hpp"
#include "dmi_message_out_speed_and_distance.hpp"
#include "dmi_message_out_train_config_data.hpp"
#include "dmi_message_out_train_name.hpp"
#include "dmi_message_out_train_vs_track_dir_wanted.hpp"
#include "dmi_message_out_version.hpp"
#include "dmi_message_out_rereg_selected.hpp"
#include "dmi_message_out_atp_notification.hpp"
#include "dmi_message_out_dmi_startup_history.hpp"
#include "dmi_message_out_text_message.hpp"
#include "dmi_message_out_predefined_text_message.hpp"
#include "dmi_message_out_area_request.hpp"
#include "dmi_message_out_time.hpp"
#include "dmi_message_out_vehicle_types.hpp"
#include  "dmi_message_out_location_data.hpp"
#include "dmi_message_out_radio_channel_bhp.hpp"
#include "dmi_message_out_typical_config_bhp.hpp"
#include "dmi_message_out_train_weight_bhp.hpp"
#include "dmi_message_out_eta_bhp.hpp"
#include "dmi_message_out_ceiling_speed_list_bhp.hpp"

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
  namespace DMICom
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    DMIHandler::DMIHandler(void) :AbstractDMIHandler(), initDone(false)
    {

    }

    /******************************************************************************
    * Function: instance()
    *-----------------------------------------------------------------------------
    * Returns singleton instance
    *
    ******************************************************************************/
    DMIHandler& DMIHandler::instance(void)
    {
      static DMIHandler theOnlyDMIHandlerInstance;

      return theOnlyDMIHandlerInstance;
    }


    /******************************************************************************
    * init
    *-----------------------------------------------------------------------------
    * Fill the containers for parsers and creators.
    ******************************************************************************/
    bool DMIHandler::init(void)
    {
      if (!initDone)
      {
        //lint --e{586} 'new' is acceptable during initialization

        dmiMessageOutCreator.reserve(static_cast<size_t>(MTypeDMIMessageTypeMax));

        //lint --e{586} 'new' is acceptable during initialization

        //fill the container with parsers for incoming messages
        dmiMessageInParser[MTypeDriverIdAndPassword] = new DMIMessageInDriverIDandPassword();
        dmiMessageInParser[MTypeDMIStartup] = new DMIMessageInDMIStartup();
        dmiMessageInParser[MTypeDMIStatus] = new DMIMessageInDMIStatus();
        dmiMessageInParser[MTypeMMIToATPData] = new DMIMessageInDMIToATPData();
        dmiMessageInParser[MTypeLocoVsTrainDir] = new DMIMessageInLocoVsTrainDir();
        dmiMessageInParser[MTypeVehicleData] = new DMIMessageInVehicleData();
        dmiMessageInParser[MTypeConfirmation] = new DMIMessageInConfirmation();
        dmiMessageInParser[MTypeTrainVsTrackDir] = new DMIMessageInTrainVsTrackDir();
        dmiMessageInParser[MTypeRegistrationArea] = new DMIMessageInRegistrationArea();
        dmiMessageInParser[MTypeTrainName] = new DMIMessageInTrainName();
        dmiMessageInParser[MTypeTrainLoaded] = new DMIMessageInTrainLoaded();

        //fill the container with Creator for Outgoing messages
        dmiMessageOutCreator.push_back(new DMIMessageOutATPModesAndStatusBHP());
        dmiMessageOutCreator.push_back(new DMIMessageOutSpeedAndDistance());
        dmiMessageOutCreator.push_back(new DMIMessageOutReRegSelected());
        dmiMessageOutCreator.push_back(new DMIMessageOutTrainConfigData());
        dmiMessageOutCreator.push_back(new DMIMessageOutVehicleData());
        dmiMessageOutCreator.push_back(new DMIMessageOutCeilingSpeedListBHP());
        dmiMessageOutCreator.push_back(new DMIMessageOutGradientDataList());
        dmiMessageOutCreator.push_back(new DMIMessageOutDriverInfo());
        dmiMessageOutCreator.push_back(new DMIMessageOutErasePlanningArea());
        dmiMessageOutCreator.push_back(new DMIMessageOutLocoVsTrainDir());
        dmiMessageOutCreator.push_back(new DMIMessageOutManualConfigSelected());
        dmiMessageOutCreator.push_back(new DMIMessageOutRadioChannelBHP());
        dmiMessageOutCreator.push_back(new DMIMessageOutTrainName());
        dmiMessageOutCreator.push_back(new DMIMessageOutTrainVsTrackDirWanted());
        dmiMessageOutCreator.push_back(new DMIMessageOutVersion());
        dmiMessageOutCreator.push_back(new DMIMessageOutAtpNotification());
        dmiMessageOutCreator.push_back(new DMIMessageOutDMIStartupHistory());
        dmiMessageOutCreator.push_back(new DMIMessageOutTextMessage()); //text message
        dmiMessageOutCreator.push_back(new DMIMessageOutPredefinedTextMessage());
        dmiMessageOutCreator.push_back(new DMIMessageOutAreaRequest());
        dmiMessageOutCreator.push_back(new DMIMessageOutTime());
        dmiMessageOutCreator.push_back(new DMIMessageOutVehicleTypes());
        dmiMessageOutCreator.push_back(new DMIMessageOutLocationData());
        dmiMessageOutCreator.push_back(new DMIMessageOutTypicalConfigBHP());
        dmiMessageOutCreator.push_back(new DMIMessageOutTrainWeightBHP());
        dmiMessageOutCreator.push_back(new DMIMessageOutETABHP());

        if (AbstractDMIHandler::init())
        {
          initDone = true;
        }

      }
      return initDone;
    }

    /******************************************************************************
    * Function: getTrainLoadedStatusRequestedByDriver
    ******************************************************************************/
    bool DMIHandler::getTrainLoadedStatusRequestedByDriver(TrainLoaded & trainLoadedStatusRequested)
    {
      bool retVal = false;

      if (dmiMessageInParser.count(MTypeTrainLoaded) > 0U)
      {
        DMIMessageInTrainLoaded *dmiMessageInTrainLoaded
          = ATC::dynamicCast<AbstractDMIMessageIn*, DMIMessageInTrainLoaded*> (dmiMessageInParser[MTypeTrainLoaded], __FILE__, __LINE__); 

        retVal = dmiMessageInTrainLoaded->getTrainLoadedStatusRequestedByDriver(trainLoadedStatusRequested);

      }
      else
      {
        trace.write(ATC::veryDetailedTrace, "DMI Handler:Parser not found for message: DMIToATPData");
        writeToLog(ATC::VeryDetailedLog, "DMI Handler:Parser not found for message: DMIToATPData", __FILE__, __LINE__);
      }

      return retVal;
    }

    /******************************************************************************
    * Function: getCompatibilityVersion
    ******************************************************************************/
    uint8_t DMIHandler::getCompatibilityVersion() const
    {
      return compatibilityVersion;
    }
  }
}

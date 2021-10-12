#ifndef RadioMessageTypesBHP_hpp
#define RadioMessageTypesBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Types related to FFFIS TCC-AOS BHP Adaptation messages.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-23    akushwah      Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /** BHPB_CONFIG_VERSION, used in Path, TrainSetup */
    static const RadioBlockType BTypeBHPBConfigVersion    = 129U;

    /** BHPB_RADIO_CHANNEL, used in CommandMessage */
    static const RadioBlockType BTypeBHPBRadioChannel     = 130U;

    /** BHPB_TRAIN_STATUS, used in PositionReport */
    static const RadioBlockType BTypeBHPBTrainStatus      = 131U;

    /** BHPB_SAFE_FOR_BOARDING, used in CommandMessage */
    static const RadioBlockType BTypeBHPBSafeForBoarding  = 133U;

    /** BHPB_LOAD_STATUS, used in StartUpMessage */
    static const RadioBlockType BTypeBHPBLoadStatus       = 134U;

    /** BHPB_SET_APPROACH_SPEED, used in PositionReport */
    static const RadioBlockType BTypeBHPBSetApproachSpeed = 135U;

    /**
    *  BHPBSafeForBoarding corresponds with BHPB_SAFE_FOR_BOARDING
    */
    struct BHPBSafeForBoarding
    {
      uint16_t noOfBytesApplicationData; //!< Number of bytes application data
    };

    /**
    * Max string length of Radio channel Name field of an TCC-AOS communication packet
    */
    static const uint8_t radioChannelNameLength = 20U;

    /**
    *  BHPBRadioChannel corresponds with BHPB_RADIO_CHANNEL
    */
    struct BHPBRadioChannel
    {
      uint16_t noOfBytesApplicationData; //!< Number of bytes application data
      char_t radioChannel[radioChannelNameLength + 1U]; //!< Radio channel to be used by locomotive driver
    };

    /**
    *  BHPBConfigVersion corresponds with BHPB_CONFIG_VERSION
    */
    struct BHPBConfigVersion
    {
      uint16_t noOfBytesApplicationData; //!< Number of bytes application data
      uint8_t configurationMajorVersion; //!< Configuration major version, track layout
      uint8_t configurationMinorVersion; //!< Configuration minor version, track layout
    };

    /**
    *  BHPBTrainStatus corresponds with BHPB_TRAIN_STATUS
    */
    struct BHPBTrainStatus
    {
      uint16_t noOfBytesApplicationData; //!< Number of bytes application data
      uint32_t bhpbTrainStatus; //!< Current BHPB train status
    };

    /**
    *  BHPBSetApproachSpeed corresponds with BHPB_SET_APPROACH_SPEED
    */
    struct BHPBSetApproachSpeed
    {
      uint16_t noOfBytesApplicationData; //!< Number of bytes application data
      uint16_t levelCrossingSpeed; //!< Ceiling speed during approach to level crossing
    };

    /**
    * Acoustic signal Country Horn
    */
    static const AcousticSignal bhpQ_SignalCountryHorn = 128U;

    /**
    * Acoustic signal Town Horn
    */
    static const AcousticSignal bhpQ_SignalTownHorn = 129U;

    /**
    * Acoustic signal Bell
    */
    static const AcousticSignal bhpQ_SignalBell = 130U;

    /**
    * Bit Value for Rapid loss of brake pressure detected
    */
    static const uint32_t bhpTrainStatusRapidLossOfBrakePressure = 0x00000001U;

    /**
    * Bit Value for Safe for boarding is active 
    */
    static const uint32_t bhpTrainStatusSafeForBoarding = 0x00000002U;

    /**
    * TrackDataType declares the types corresponding with the field Q_TRACK_DATA_TYPE
    */
    enum TrackDataBHPType
    {
      TrackDataTypeLevelCrossing = 129
    };
  }
}
#endif

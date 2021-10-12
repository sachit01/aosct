#ifndef AbstractConfig_hpp
#define AbstractConfig_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Definition of the AbstractConfig class, the ATP-Core part of Config.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-10    jeneman     Created
* 2016-09-15    nsyed       Added individual access functions for config params
* 2016-09-22    arastogi    Include atp_types.hpp as it is no longer in atc_types.hpp
* 2016-10-04    nsyed       Update config parameter IDs and add new config parameters
* 2016-10-04    nsyed       Add access functions to the new config parameters
* 2017-01-12    saprasad    Added config function prototypes for Analyzer IF
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_config_base.hpp"
#include "atp_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{

  /**
  * Declaration of Config ID's
  */
  static const ATC::ConfigIdType balNidCId = 101U;                      //!< Eurobalise country code
  static const ATC::ConfigIdType radioIdId = 102U;                      //!< RadioId
  static const ATC::ConfigIdType siteIdId = 103U;                       //!< Site ID
  static const ATC::ConfigIdType radioTimeOutId = 104U;                 //!< Time before detecting the lost radio connection
  static const ATC::ConfigIdType radioTimeOutSbId = 105U;               //!< Time to issue service brake after detecting a lost radio connection
  static const ATC::ConfigIdType balSearchDistanceId = 106U;            //!< Allowed search distance for the first balise
  static const ATC::ConfigIdType balSearchSpeedId = 107U;               //!< Balise search speed
  static const ATC::ConfigIdType balWindowMinId = 108U;                 //!< Minimum size of balise window
  static const ATC::ConfigIdType balWindowPermilId = 109U;              //!< Balise window in permil
  static const ATC::ConfigIdType yardSpeedId = 110U;                    //!< Allowed speed in YardMode
  static const ATC::ConfigIdType atoEnableId = 111U;                    //!< ATO functionality status
  static const ATC::ConfigIdType revSupMarginNormId = 112U;             //!< Reversing supervision margin normal MA
  static const ATC::ConfigIdType revSupMarginShuntId = 113U;            //!< Reversing supervision margin in shunting
  static const ATC::ConfigIdType rollAwayMarginId = 114U;               //!< Maximum allowed roll - away movement
  static const ATC::ConfigIdType tachoPulsesPer10Rev1Id = 115U;         //!< Number of tachometer pulses for every 10 revolutions of wheel 1
  static const ATC::ConfigIdType tachoPulsesPer10Rev2Id = 116U;         //!< Number of tachometer pulses for every 10 revolutions of wheel 2
  static const ATC::ConfigIdType wheelSize1Id = 117U;                   //!< WheelSize 1
  static const ATC::ConfigIdType wheelSize2Id = 118U;                   //!< WheelSize 2
  static const ATC::ConfigIdType balAntennaPosEndId = 119U;             //!< Balise antenna position end in cm
  static const ATC::ConfigIdType balAntennaPosFrontId = 120U;           //!< Balise antenna position front in cm
  static const ATC::ConfigIdType enableDiagnosticsId = 121U;            //!< Enable diagnostics (Console and Analyzer)

  static const ATC::ConfigIdType maxWheelSizeErrorId = 124U;            //!< Max estimated wheelsize error
  static const ATC::ConfigIdType revSupMarginFreeRollingId = 125U;      //!< Reversing supervision margin in Free Rolling

  static const ATC::ConfigIdType minSpeedMarginWarnId = 128U;           //!< Minimum speed margin over ceiling speed before warning
  static const ATC::ConfigIdType minSpeedMarginSBId = 129U;             //!< Minimum speed margin over ceiling speed before SB
  static const ATC::ConfigIdType minSpeedMarginEBId = 130U;             //!< Minimum speed margin over ceiling speed before EB
  static const ATC::ConfigIdType maxSpeedMarginWarnId = 131U;           //!< Maximum speed margin over ceiling speed before warning
  static const ATC::ConfigIdType maxSpeedMarginSBId = 132U;             //!< Maximum speed margin over ceiling speed before SB
  static const ATC::ConfigIdType maxSpeedMarginEBId = 133U;             //!< Maximum speed margin over ceiling speed before EB
  static const ATC::ConfigIdType speedMarginWarnId = 134U;              //!< Permil of ceiling speed, Speed margin over ceiling speed before warning
  static const ATC::ConfigIdType speedMarginSBId = 135U;                //!< Permil of ceiling speed, speed margin over ceiling speed before SB
  static const ATC::ConfigIdType speedMarginEBId = 136U;                //!< Permil of ceiling speed, speed margin over ceiling speed before EB

  static const ATC::ConfigIdType fwToSwDelayId = 139U;                  //!< Delay time from driver's first warning until second warning
  static const ATC::ConfigIdType swToSBDelayId = 140U;                  //!< Delay time from driver's second warning until service brake application
  static const ATC::ConfigIdType sbToEbDelayId = 141U;                  //!< Delay time from service brake application until emergency brake application
  static const ATC::ConfigIdType effectiveEbDelayId = 142U;             //!< Effective delay in applying emergency brake
  static const ATC::ConfigIdType opcMajorVersionId = 143U;              //!< 1st version number of the OPC (Integer between 0 to 255)
  static const ATC::ConfigIdType opcMiddleVersionId = 144U;             //!< 2nd version number of the OPC (Integer between 0 to 255)
  static const ATC::ConfigIdType opcMinorVersionId = 145U;              //!< 3rd version number of the OPC (Integer between 0 to 255, 255 is ignore value)
  static const ATC::ConfigIdType analyzerIFPortId = 146U;               //!< Analyzer Port ID
  static const ATC::ConfigIdType sendCycleAIFId = 147U;                 //!< Send Cycle for AOS Analyzer IF 
  static const ATC::ConfigIdType secBalSearchDistanceId = 148U;         //!< Second balise wait MA distance
  static const ATC::ConfigIdType eventKeepActiveTimeId = 149U;          //!< Keep Active event time
  static const ATC::ConfigIdType trainCfgTicTimeoutId = 150U;           //!< The maximum execution time for the TIC system to deliver a train configuration
  static const ATC::ConfigIdType codSensorConfigId = 157U;              //!< The configuration of the COD sensor to enable Tacho, Doppler radar or both.
  static const ATC::ConfigIdType tractionControlId = 158U;              //!< The type of traction system with intentional slipping and sliding mounted.
  static const ATC::ConfigIdType minDopplerSpeedId = 159U;              //!< Minimum speed when Doppler shall be used.
  static const ATC::ConfigIdType maxDopplerSpeedId = 160U;              //!< maximum speed when Doppler shall be used.
  static const ATC::ConfigIdType maxDopplerAccelId = 161U;              //!< Maximum acceleration that Doppler radar can handle.
  static const ATC::ConfigIdType dopplerPulsesPerKilometerId = 162U;    //!< Number of Doppler radar pulses for a km to distance traveled.
  static const ATC::ConfigIdType dopplerPulsePrecisionId = 163U;        //!< Maximum error in the precision of Doppler pulse rate.
  static const ATC::ConfigIdType btmTestNotifyId = 164U;                //!< Minimum time elapsed for performing next BTM test.
  static const ATC::ConfigIdType btmTestMandatoryId = 165U;             //!< Maximum time elapsed for performing next BTM test.
  static const ATC::ConfigIdType lastBTMTestTimeId = 166U;              //!< Time stamp of the last performed BTM test.
  static const ATC::ConfigIdType brakeTestNotifyId = 167U;              //!< Notification time before the max brake test interval is reached
  static const ATC::ConfigIdType brakeTestMandatoryId = 168U;           //!< Maximum time elapsed for performing next Brake test.
  static const ATC::ConfigIdType lastBrakeTestTimeId = 169U;            //!< Time stamp of the last performed Brake test.
  static const ATC::ConfigIdType brakeTestExecTimeId = 172U;            //!< Max execution time for a Brake Test
  static const ATC::ConfigIdType minSafetyMarginId = 173U;              //!< Minimum Safety Margin
  static const ATC::ConfigIdType tacho1DirectionId = 174U;              //!< The direction of the tacho1
  static const ATC::ConfigIdType tacho2DirectionId = 175U;              //!< The direction of the tacho2  
 
  static const ATC::ConfigIdType brakeTestReasonId = 177U;              //!< Number code defining the reason for a Mandatory Brake Test  
  static const ATC::ConfigIdType maxTimeAOSRunId = 178U;                //!< Maximum Time AOS is allowed to run continuously without reset
  static const ATC::ConfigIdType intRelaysTimeoutId = 179U;             //!< Max wait time for the expected Internal Relays Feedback  
  static const ATC::ConfigIdType sbExpectedDecLimitId = 180U;           //!< Percentage of expected deceleration below which deceleration is not sufficient for Service Brake application
  static const ATC::ConfigIdType ebExpectedDecLimitId = 181U;           //!< Percentage of expected deceleration below which deceleration is not sufficient for Emergency Brake application
  static const ATC::ConfigIdType locoTypeId = 184U;                     //!< Configuration to select the type of locomotive.
  static const ATC::ConfigIdType cabinConfigurationId = 185U;           //!< Configuration to indicate if one or two cabins are selected.
  static const ATC::ConfigIdType esaInputAvailId = 187U;                //!< Configuration to indicate if AOS shall read Emergency Stop Active input or not.
  static const ATC::ConfigIdType sbAvailId = 188U;                      //!< Service Brake access available       
  static const ATC::ConfigIdType brakeType1ParamA1Id = 189U;            //!< Brake Parameter A1 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamA1Id = 190U;            //!< Brake Parameter A1 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamA1Id = 191U;            //!< Brake Parameter A1 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamA2Id = 192U;            //!< Brake Parameter A2 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamA2Id = 193U;            //!< Brake Parameter A2 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamA2Id = 194U;            //!< Brake Parameter A2 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamA3Id = 195U;            //!< Brake Parameter A3 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamA3Id = 196U;            //!< Brake Parameter A3 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamA3Id = 197U;            //!< Brake Parameter A3 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamB1Id = 198U;            //!< Brake Parameter B1 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamB1Id = 199U;            //!< Brake Parameter B1 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamB1Id = 200U;            //!< Brake Parameter B1 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamB2Id = 201U;            //!< Brake Parameter B2 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamB2Id = 202U;            //!< Brake Parameter B2 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamB2Id = 203U;            //!< Brake Parameter B2 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamB3Id = 204U;            //!< Brake Parameter B3 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamB3Id = 205U;            //!< Brake Parameter B3 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamB3Id = 206U;            //!< Brake Parameter B3 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamV1Id = 207U;            //!< Brake Parameter V1 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamV1Id = 208U;            //!< Brake Parameter V1 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamV1Id = 209U;            //!< Brake Parameter V1 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1ParamV2Id = 210U;            //!< Brake Parameter V2 for Brake System Type 1
  static const ATC::ConfigIdType brakeType2ParamV2Id = 211U;            //!< Brake Parameter V2 for Brake System Type 2
  static const ATC::ConfigIdType brakeType3ParamV2Id = 212U;            //!< Brake Parameter V2 for Brake System Type 3
  static const ATC::ConfigIdType brakeType1LambdaMinId = 213U;          //!< Minimum lambda for Brake Type 1
  static const ATC::ConfigIdType brakeType2LambdaMinId = 214U;          //!< Minimum lambda for Brake Type 2
  static const ATC::ConfigIdType brakeType3LambdaMinId = 215U;          //!< Minimum lambda for Brake Type 3
  static const ATC::ConfigIdType brakeType1LambdaMaxId = 216U;          //!< Maximum lambda for Brake Type 1
  static const ATC::ConfigIdType brakeType2LambdaMaxId = 217U;          //!< Maximum lambda for Brake Type 2
  static const ATC::ConfigIdType brakeType3LambdaMaxId = 218U;          //!< Maximum lambda for Brake Type 3
  static const ATC::ConfigIdType maxAccelerationId = 219U;              //!< Maximum expected acceleration value
  static const ATC::ConfigIdType maxDecelerationId = 220U;              //!< Maximum expected deceleration value
  static const ATC::ConfigIdType vehicleType1Id = 221U;                 //!< Vehicle Type 1 for Manual configuration
  static const ATC::ConfigIdType vehicleType2Id = 222U;                 //!< Vehicle Type 2 for Manual configuration
  static const ATC::ConfigIdType vehicleType3Id = 223U;                 //!< Vehicle Type 3 for Manual configuration
  static const ATC::ConfigIdType vehicleType4Id = 224U;                 //!< Vehicle Type 4 for Manual configuration
  static const ATC::ConfigIdType vehicleType5Id = 225U;                 //!< Vehicle Type 5 for Manual configuration
  static const ATC::ConfigIdType vehicleType6Id = 226U;                 //!< Vehicle Type 6 for Manual configuration
  static const ATC::ConfigIdType vehicleType7Id = 227U;                 //!< Vehicle Type 7 for Manual configuration
  static const ATC::ConfigIdType vehicleType8Id = 228U;                 //!< Vehicle Type 8 for Manual configuration
  static const ATC::ConfigIdType vehicleType9Id = 229U;                 //!< Vehicle Type 9 for Manual configuration
  static const ATC::ConfigIdType vehicleType10Id = 230U;                //!< Vehicle Type 10 for Manual configuration
  static const ATC::ConfigIdType vehicleType11Id = 231U;                //!< Vehicle Type 11 for Manual configuration
  static const ATC::ConfigIdType vehicleType12Id = 232U;                //!< Vehicle Type 12 for Manual configuration
  static const ATC::ConfigIdType vehicleType13Id = 233U;                //!< Vehicle Type 13 for Manual configuration
  static const ATC::ConfigIdType vehicleType14Id = 234U;                //!< Vehicle Type 14 for Manual configuration
  static const ATC::ConfigIdType vehicleType15Id = 235U;                //!< Vehicle Type 15 for Manual configuration
  static const ATC::ConfigIdType vehicleType16Id = 236U;                //!< Vehicle Type 16 for Manual configuration
  static const ATC::ConfigIdType vehicleType17Id = 237U;                //!< Vehicle Type 17 for Manual configuration
  static const ATC::ConfigIdType vehicleType18Id = 238U;                //!< Vehicle Type 18 for Manual configuration
  static const ATC::ConfigIdType vehicleType19Id = 239U;                //!< Vehicle Type 19 for Manual configuration
  static const ATC::ConfigIdType vehicleType20Id = 240U;                //!< Vehicle Type 20 for Manual configuration
  static const ATC::ConfigIdType maxSupvGradTargId = 241U;              //!< Maximum number of Supervised Gradient Target
  static const ATC::ConfigIdType minSupvGradTargDstId = 242U;           //!< Maximum Supervised Gradient Target Distance
  static const ATC::ConfigIdType tractionCutOffDelayId = 243U;          //!< Delay time for Traction Cut-off for brake curve calculation
  static const ATC::ConfigIdType radioTimeOutYardModeId = 244U;         //!< Radio connection timeout with yard mode allowance
  static const ATC::ConfigIdType upperSpeedSbDecId = 245U;              //!< Upper Speed boundary value during supervision of applied SB
  static const ATC::ConfigIdType lowerSpeedSbDecId = 246U;              //!< Lower Speed boundary value during supervision of applied SB
  static const ATC::ConfigIdType upperSpeedEbDecId = 247U;              //!< Upper Speed boundary value during supervision of applied EB
  static const ATC::ConfigIdType lowerSpeedEbDecId = 248U;              //!< Lower Speed boundary value during supervision of applied EB
  static const ATC::ConfigIdType timeSbv0Id = 249U;                     //!< The supervision of an applied service brake requires zero speed within this time.
  static const ATC::ConfigIdType timeEbv0Id = 250U;                     //!< The supervision of an applied emergency brake requires zero speed within this time.
  static const ATC::ConfigIdType maxLowDecInRowSBId = 251U;             //!< Maximum Number of bad readings in a row while SB deceleration supervision.
  static const ATC::ConfigIdType maxLowDecAccumSBId = 252U;             //!< Maximum Number of bad readings accumulative for each SB application.
  static const ATC::ConfigIdType maxLowDecInRowEBId = 253U;             //!< Maximum Number of bad readings in a row while EB deceleration supervision.
  static const ATC::ConfigIdType maxLowDecAccumEBId = 254U;             //!< Maximum Number of bad readings accumulative for each EB application.
  static const ATC::ConfigIdType eventStandstillKeepActiveTimeId = 255U; //!<Keep Standstill Active event time

  static const ATC::ConfigIdType brakeType1ParamT1SBId = 257U;          //!< Brake Parameter T1 for Brake System Type 1(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType1ParamT2SBId = 258U;          //!< Brake Parameter T2 for Brake System Type 1(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType1ParamT3SBId = 259U;          //!< Brake Parameter T3 for Brake System Type 1(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType2ParamT1SBId = 260U;          //!< Brake Parameter T1 for Brake System Type 2(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType2ParamT2SBId = 261U;          //!< Brake Parameter T2 for Brake System Type 2(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType2ParamT3SBId = 262U;          //!< Brake Parameter T3 for Brake System Type 2(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType3ParamT1SBId = 263U;          //!< Brake Parameter T1 for Brake System Type 3(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType3ParamT2SBId = 264U;          //!< Brake Parameter T2 for Brake System Type 3(For time delay calculations), SB
  static const ATC::ConfigIdType brakeType3ParamT3SBId = 265U;          //!< Brake Parameter T3 for Brake System Type 3(For time delay calculations), SB
  static const ATC::ConfigIdType defaultBrakeDelayId = 266U;            //!< Default break delay
  static const ATC::ConfigIdType defaultBrakeabilityId = 267U;          //!< Default break ability

  static const ATC::ConfigIdType ebMarginAddedId = 269U;                //!< Margin before EB is applied
  static const ATC::ConfigIdType timsManualConfTimeId = 270U;           //!< Time for performing manual integrity confirmation
  static const ATC::ConfigIdType timsSensorTimeDiffId = 271U;           //!< Clock difference between AOS and TIMS sensor
  static const ATC::ConfigIdType ebPrPropagationSpeedId = 272U;         //!< EB pressure propagation speed
  static const ATC::ConfigIdType releaseSpeedId = 273U;                 //!< Release speed
  static const ATC::ConfigIdType departureWarningId = 274U;             //!< Number of seconds from first warning sound of departure warning until train movement allowed
  static const ATC::ConfigIdType vNomMarginId = 275U;                   //!< Margin for allowed variance in difference in vMax/vMin and vNom
  static const ATC::ConfigIdType timeSyncServerVersion1Id = 276U;       //!< 1st number of the time sync server version (or 255 if this number isn't used)
  static const ATC::ConfigIdType timeSyncServerVersion2Id = 277U;       //!< 2nd number of the time sync server version (or 255 if this number isn't used)
  static const ATC::ConfigIdType timeSyncServerVersion3Id = 278U;       //!< 3rd number of the time sync server version (or 255 if this number isn't used)
  static const ATC::ConfigIdType timeSyncServerVersion4Id = 279U;       //!< 4th number of the time sync server version (or 255 if this number isn't used)
  static const ATC::ConfigIdType brakeType1ParamT1EBId = 280U;          //!< Brake Parameter T1 for Brake System Type 1(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType1ParamT2EBId = 281U;          //!< Brake Parameter T2 for Brake System Type 1(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType1ParamT3EBId = 282U;          //!< Brake Parameter T3 for Brake System Type 1(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType2ParamT1EBId = 283U;          //!< Brake Parameter T1 for Brake System Type 2(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType2ParamT2EBId = 284U;          //!< Brake Parameter T2 for Brake System Type 2(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType2ParamT3EBId = 285U;          //!< Brake Parameter T3 for Brake System Type 2(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType3ParamT1EBId = 286U;          //!< Brake Parameter T1 for Brake System Type 3(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType3ParamT2EBId = 287U;          //!< Brake Parameter T2 for Brake System Type 3(For time delay calculations), EB
  static const ATC::ConfigIdType brakeType3ParamT3EBId = 288U;          //!< Brake Parameter T3 for Brake System Type 3(For time delay calculations), EB


  /**
  * The class AbstractConfig implements the interface defined by the AbstractConfigBase class.
  *
  */
  class AbstractConfig : public ATC::AbstractConfigBase
  {
  public:
    /**
    * Implements the virtual init function.
    * The ATP-Core specific config items are added here.
    *
    * @return true when initialization completed
    */
    virtual bool init(void);

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractConfig* corePtr();

    /**
    * Get the Eurobalise country code
    * This in turn calls the relevant getconfig function
    *
    * @return Eurobalise country code
    */
    uint16_t getBalNidC() const;

    /**
    * Get the Radio ID
    * This in turn calls the relevant getconfig function
    *
    * @return Radio ID
    */
    uint16_t getRadioId() const;

    /**
    * Get the Site ID
    * This in turn calls the relevant getconfig function
    *
    * @return Site ID
    */
    uint8_t getSiteId() const;

    /**
    * Get the Time before detecting the lost radio connection
    * This in turn calls the relevant getconfig function
    *
    * @return Time before detecting the lost radio connection
    */
    uint8_t getRadioTimeOut() const;

    /**
    * Get the Time to issue service brake after detecting a lost radio connection
    * This in turn calls the relevant getconfig function
    *
    * @return Time to issue service brake after detecting a lost radio connection
    */
    uint8_t getRadioTimeOutSb() const;

    /**
    * Get the search distance for the first balise
    * This in turn calls the relevant getconfig function
    *
    * @return First balise search distance
    */
    uint16_t getBalSearchDistance() const;

    /**
    * Get the Second Balise search distance
    * This in turn calls the relevant getconfig function
    *
    * @return Second Balise search distance
    */
    uint16_t getSecondBaliseSearchDistance() const;

    /**
    * Get the Balise search speed
    * This in turn calls the relevant getconfig function
    *
    * @return Balise search speed
    */
    uint16_t getBalSearchSpeed() const;

    /**
    * Get the Minimum size of balise window
    * This in turn calls the relevant getconfig function
    *
    * @return Minimum size of balise window
    */
    uint16_t getBalWindowMin() const;

    /**
    * Get the Balise window in permil
    * This in turn calls the relevant getconfig function
    *
    * @return Balise window in permil
    */
    uint8_t getBalWindowPermil() const;

    /**
    * Get the Allowed speed in YardMode
    * This in turn calls the relevant getconfig function
    *
    * @return Allowed speed in YardMode
    */
    uint16_t getYardSpeed() const;

    /**
    * Get the ATO functionality status
    * This in turn calls the relevant getconfig function
    *
    * @return true if ATO function is enabled
    */
    bool getAtoEnable() const;

    /**
    * Get the Reversing supervision margin for Normal, 
    * Staff Responsible, Split, Join and Balise Search Modes
    * This in turn calls the relevant getconfig function
    *
    * @return Reversing supervision margin for normal MA
    */
    uint16_t getRevSupMargin() const;

    /**
    * Get the Reversing supervision margin in shunting route mode
    * This in turn calls the relevant getconfig function
    *
    * @return Reversing supervision margin in shunting route
    */
    uint16_t getRevSupMargForShuntRoute() const;

    /**
    * Get the Reversing supervision margin in Free Rolling.
    * This in turn calls the relevant getconfig function.
    *
    * @return Reversing supervision margin in Free Rolling
    */
    uint16_t getRevSupMargForFreeRolling() const;

    /**
    * Get the Maximum allowed roll-away movement
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum allowed roll-away movement
    */
    uint16_t getRollAwayMargin() const;

    /**
    * Get the Number of tachometer pulses for revolution of wheel1
    * This in turn calls the relevant getconfig function
    *
    * @return Number of tachometer pulses for every 10 revolutions of the wheel 1
    */
    uint16_t getTachoPulsesPer10Revolutions1() const;

    /**
    * Get the Number of tachometer pulses for revolution of wheel2
    * This in turn calls the relevant getconfig function
    *
    * @return Number of tachometer pulses for every 10 revolutions of the wheel 2
    */
    uint16_t getTachoPulsesPer10Revolutions2() const;

    /**
    * Get the Wheel Size 1
    * This in turn calls the relevant getconfig function
    *
    * @return Wheel Size 1
    */
    uint16_t getWheelSize1() const;

    /**
    * Get the Wheel Size 2
    * This in turn calls the relevant getconfig function
    *
    * @return Wheel Size 2
    */
    uint16_t getWheelSize2() const;

    /**
    * Get the Balise antenna position end in cm
    * This in turn calls the relevant getconfig function
    *
    * @return Balise antenna position end in cm
    */
    uint16_t getBalAntennaPosEnd() const;

    /**
    * Get the Balise antenna position front in cm
    * This in turn calls the relevant getconfig function
    *
    * @return Balise antenna position front in cm
    */
    uint16_t getBalAntennaPosFront() const;

    /**
    * Get the max wheel size error
    * This in turn calls the relevant getconfig function
    *
    * @return Max wheel size error
    */
    uint8_t getMaxWheelSizeError() const;

    /**
    * Get the Minimum speed margin over ceiling speed before warning
    * This in turn calls the relevant getconfig function
    *
    * @return Minimum speed margin over ceiling speed before warning
    */
    uint16_t getMinSpeedMarginWarn() const;

    /**
    * Get the Minimum speed margin over ceiling speed before SB
    * This in turn calls the relevant getconfig function
    *
    * @return Minimum speed margin over ceiling speed before SB
    */
    uint16_t getMinSpeedMarginSB() const;

    /**
    * Get the Minimum speed margin over ceiling speed before EB
    * This in turn calls the relevant getconfig function
    *
    * @return Minimum speed margin over ceiling speed before EB
    */
    uint16_t getMinSpeedMarginEB() const;

    /**
    * Get the Maximum speed margin over ceiling speed before warning
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum speed margin over ceiling speed before warning
    */
    uint16_t getMaxSpeedMarginWarn() const;

    /**
    * Get the Maximum speed margin over ceiling speed before SB
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum speed margin over ceiling speed before SB
    */
    uint16_t getMaxSpeedMarginSB() const;

    /**
    * Get the Maximum speed margin over ceiling speed before EB
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum speed margin over ceiling speed before EB
    */
    uint16_t getMaxSpeedMarginEB() const;

    /**
    * Get the Permil of ceiling speed, Speed margin over ceiling speed
    * before warning
    * This in turn calls the relevant getconfig function
    *
    * @return Permil of ceiling speed, Speed margin over ceiling speed
    */
    uint16_t getSpeedMarginWarn() const;

    /**
    * Get the Permil of ceiling speed, speed margin over ceiling speed
    * before SB
    * This in turn calls the relevant getconfig function
    *
    * @return Permil of ceiling speed, speed margin over ceiling speed
    * before SB
    */
    uint16_t getSpeedMarginSB() const;

    /**
    * Get the Permil of ceiling speed, speed margin over ceiling speed
    * before EB
    * This in turn calls the relevant getconfig function
    *
    * @return Permil of ceiling speed, speed margin over ceiling speed
    * before EB
    */
    uint16_t getSpeedMarginEB() const;

    /**
    * Get the Delay time from first warning to second warning curve
    * This in turn calls the relevant getconfig function
    *
    * @return Delay time from driver's first warning until second warning (seconds)
    */
    uint16_t getFirstToSecondWarnCurveDelay() const;

    /**
    * Get the Delay time from second warning until service
    * brake application curve
    * This in turn calls the relevant getconfig function
    *
    * @return Delay time from driver's second warning until service (seconds)
    * brake application
    */
    uint16_t getSecondToSBCurveDelay() const;

    /**
    * Get the Delay time from service brake application until emergency
    * brake application
    * This in turn calls the relevant getconfig function
    *
    * @return Delay time from service brake application until emergency (0.1 seconds)
    * brake application
    */
    uint16_t getSbToEbDelay() const;

    /**
    * Get the Effective delay in applying emergency brake
    * This in turn calls the relevant getconfig function
    *
    * @return Effective delay in applying emergency brake (0.1 seconds)
    */
    uint16_t getEffectiveEbDelay() const;

    /**
    * Get the maximum execution time for the TIC system to deliver a train configuration
    * This in turn calls the relevant getconfig function
    *
    * @return The timeout value in 's'
    */
    uint16_t getTrainCfgTicTimeout() const;

    /**
    * Get the event keep active time
    * The constant time in seconds the events should remain as active
    * list from the time they were last reported.
    * This in turn calls the relevant getconfig function
    * @return eventKeepActiveTime
    */
    uint8_t getEventKeepActiveTime() const;

    /**
    * Get the event keep active time for standstill event
    * The constant time in seconds the events should remain as active
    * list from the time they were last reported.
    * This in turn calls the relevant getconfig function
    * @return standstillEventKeepActiveTime
    */
    uint8_t getStandstillEventKeepActiveTime() const;

    /**
    * Get the COD sensor configuration value.
    * The set of sensors connected to the train
    * 1 - Tacho1, Tacho2 and Doppler radar
    * 2 - Tacho1 and Tacho2
    * @return codSensorConfig
    */
    uint8_t getCODSensorConfiguration() const;

    /**
    * Get the type of Traction control system with intentional slipping and sliding.
    * @return tractionControl
    */
    uint8_t getTractionControlValue() const;

    /**
    * Get the value of lower doppler radar speed range
    * @return minDopplerSpeedRange
    */
    uint16_t getMinDopplerSpeed() const;

    /**
    * Get the value of upper doppler radar speed range
    * @return maxDopplerSpeedRange
    */
    uint16_t getMaxDopplerSpeed() const;

    /**
    * Get maximum allowed acceleration/deceleration from Doppler radar
    * @return maxAccelerationDoppler
    */
    uint16_t getMaxDopplerAccel() const;

    /**
    * Get Doppler number of  pulses per kilometer
    * @return dopplerPulsesPerKilometer
    */
    uint32_t getDopplerPulsesPerKm() const;

    /**
    * Get Doppler radar pulse rate precision
    * return dopplerPulsePrecision
    */
    uint8_t getDopplerPulsePrecision() const;

    /**
    * Get Minimum time elapsed for performing next BTM test
    * This in turn calls the relevant getconfig function
    *
    * @return btmTestNotify
    */
    uint16_t getBTMTestNotify() const;

    /**
    * Get Maximum time elapsed for performing next BTM test
    * This in turn calls the relevant getconfig function
    *
    * @return btmTestMandatory
    */
    uint16_t getBTMTestMandatory() const;

    /**
    * Get Last performed time stamp for BTM test
    * This in turn calls the relevant getconfig function
    *
    * @return lastBTMTest
    */
    uint32_t getLastBTMTestTime() const;

    /**
    * Get Notification time before the max brake test interval is reached
    * This in turn calls the relevant getconfig function
    *
    * @return Time before the next mandatory Brake test, at which a notification
    * needs to be sent to the Driver about the upcoming Brake Test.
    */
    uint16_t getBrakeTestNotifyTime() const;

    /**
    * Get Maximum time elapsed for performing next Brake test
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum time for performing next Brake test
    */
    uint16_t getBrakeTestMandatoryTime() const;

    /**
    * Get Last performed time stamp for Brake test
    * This in turn calls the relevant getconfig function
    *
    * @return lastBrakeTest
    */
    uint32_t getLastBrakeTestTime() const;

    /**
    * Get the Maximum execution time for a complete Brake Test
    * This in turn calls the relevant getconfig function
    *
    * @return brakeTestExecTime
    */
    uint16_t getBrakeTestExecTime() const;

    /**
    * Get the percentage of expected decelration below which deceleration is not suffiecient for Service Brake.
    * This in turn calls the relevant getconfig function
    *
    * @return sbExpectedDecLimit
    */
    uint8_t getSbExpectedDecLimit() const;

    /**
    * Get the percentage of expected decelration below which deceleration is not suffiecient for Emergency Brake.
    * This in turn calls the relevant getconfig function
    *
    * @return ebExpectedDecLimit
    */
    uint8_t getEbExpectedDecLimit() const;

    /**
    * Get the Upper Speed boundary value used during supervision of applied SB.
    * This in turn calls the relevant getconfig function
    *
    * @return upperSpeedSbDec
    */
    uint16_t getUpperSpeedSbDec() const;

    /**
    * Get the Lower Speed boundary value used during supervision of applied SB.
    * This in turn calls the relevant getconfig function
    *
    * @return lowerSpeedSbDec
    */
    uint16_t getLowerSpeedSbDec() const;

    /**
    * Get the Upper Speed boundary value used during supervision of applied EB.
    * This in turn calls the relevant getconfig function
    *
    * @return upperSpeedEbDec
    */
    uint16_t getUpperSpeedEbDec() const;

    /**
    * Get the Lower Speed boundary value used during supervision of applied EB.
    * This in turn calls the relevant getconfig function
    *
    * @return lowerSpeedEbDec
    */
    uint16_t getLowerSpeedEbDec() const;

    /**
    * Get the Time measured from detection of va < vSbv0. 
    * The supervision of an applied service brake requires zero speed within this time.
    * This in turn calls the relevant getconfig function
    *
    * @return timeSbv0Id
    */
    uint16_t getTimeSbv0() const;

    /**
    * Get the Time measured from detection of va < vEbv0.
    * The supervision of an applied emergency brake requires zero speed within this time.
    * This in turn calls the relevant getconfig function
    *
    * @return timeEbv0Id
    */
    uint16_t getTimeEbv0() const;

    /**
    * Get maximum number of cycles in a row having deceleration value lower than expected while doing SB deceleration supervision to trigger EB.
    * This in turn calls the relevant getconfig function
    *
    * @return maxLowDecInRowSB
    */
    uint16_t getMaxLowDecInRowSB() const;

    /**
    * Get maximum number of accumulative cycles having deceleration value lower than expected while doing SB deceleration supervision to trigger EB.
    * This in turn calls the relevant getconfig function
    *
    * @return maxLowDecAccumSB
    */
    uint16_t getMaxLowDecAccumSB() const;

    /**
    * Get maximum number of cycles in a row having deceleration value lower than expected while doing EB deceleration supervision to trigger SH.
    * This in turn calls the relevant getconfig function
    *
    * @return maxLowDecInRowEB
    */
    uint16_t getMaxLowDecInRowEB() const;

    /**
    * Get maximum number of accumulative cycles having deceleration value lower than expected while doing EB deceleration supervision to trigger SH.
    * This in turn calls the relevant getconfig function
    *
    * @return maxLowDecAccumEB
    */
    uint16_t getMaxLowDecAccumEB() const;
    
    /**
    * Get the configuration towards one or two cabins.
    * This in turn calls the relevant getconfig function
    *
    * @return cabinconfiguration
    */
    uint8_t getCabinConfiguration() const;

    /**
    * Get the getAnalyzerIFPort Port
    * This in turn calls the relevant getconfig function
    * It shall be define in ATP/ATO
    * @return getAnalyzerIFPort Port
    */
    uint16_t getAnalyzerIFPort() const;

    /**
    * Get the getSendCycleAIF
    * config parameter that indicates number of cycles when
    * write measurable variables to AOS analyzer
    * It shall be define in ATP/ATO
    * @return sendCycle
    */
    uint8_t getSendCycleAIF() const;

    /**
    * Set the number code denoting the reason for the mandatory Brake Test
    *
    * @param [in] brakeTestReason Reason for Mandatory Brake Test
    *
    * @return true, if successfully written the brakeTestReason in NVS
    */
    bool setBrakeTestReason(const BrakeTestReasonType brakeTestReason);

    /**
    * Get the locomotive type
    *
    * @return locoType
    */
    uint8_t getLocoType() const;

    /**
    * Get the max wait time for the expected internal relays Feedback
    *
    * @return internal RelaysFeedbackTimeout
    */
    uint16_t getInternalRelaysTimeout() const;

    /**
    * Get the status if Emergency Stop Active inputs available.
    *
    * @return eSAInputAvail
    */
    bool getESAInputAvail() const;

    /**
    * Get the status if Service Brake access is available
    *
    * @return sbAvail
    */
    bool getSbAvailable() const;

    /**
    * Set Last performed time stamp for BTM test
    *
    * @param [in] timeToStoreInNVS current time to write in NVS
    *
    * @return true, if successfully written the time in NVS
    */
    bool setLastBTMTestTime(const uint32_t timeToStoreInNVS);

    /**
    * Set Last performed time stamp for Brake test
    *
    * @param [in] timeToStoreInNVS current time to write in NVS
    *
    * @return true, if successfully written the time in NVS
    */
    bool setLastBrakeTestTime(const uint32_t timeToStoreInNVS);

    /**
    * Get the direction of tacho1.
    * @return Tacho1Direction
    */
    uint8_t getTacho1Direction() const;

    /**
    * Get the direction of tacho2.
    * @return Tacho2Direction
    */
    uint8_t getTacho2Direction() const;

    /**
    * Get the number code defining the reason for a Mandatory Brake Test
    * @return BrakeTestReason
    */
    uint32_t getBrakeTestReason() const;

    /**
    * Get Maximum time AOS is allowed to run continuously without reset
    * This in turn calls the relevant getconfig function
    *
    * @return maxTimeAOSRun Value
    */
    uint16_t getMaxTimeAOSRun() const;

    /**
    * Get Brake System Parameters
    *
    *  @param[in]  brakeType       Brake System Type
    *  @param[in]  brakeParameter  Brake parameter for which value will be retrieved
    *
    * @return brakeParameter configuration value
    */
    uint16_t getBrakeParameter(const BrakeSystemType brakeType, const BrakeParameter brakeParameter) const;

    /**
    * Get Brake System Minimum Lambda Values
    *
    *  @param[in]  brakeType  Brake System for which minimum Lambda values are required
    *
    * @return minimum lambda value for corresponding brakeType
    */
    uint8_t getBrakeLambdaMin(const BrakeSystemType brakeType) const;

    /**
    * Get Brake System Maximum Lambda Values
    *
    *  @param[in]  brakeType  Brake System for which maximum Lambda values are required
    *
    * @return maximum lambda value for corresponding brakeType
    */
    uint8_t getBrakeLambdaMax(const BrakeSystemType brakeType) const;

    /**
    * Get the Maximum expected acceleration value in cm
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum expected acceleration in cm/s2
    */
    uint16_t getMaxAcceleration() const;

    /**
    * Get the Maximum expected deceleration value in cm/s2
    * This in turn calls the relevant getconfig function
    *
    * @return Maximum expected deceleration in cm/s2
    */
    uint16_t getMaxDeceleration() const;

    /**
    * Get the value of Vehicle Type 1
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType1() const;

    /**
    * Get the value of Vehicle Type 2
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType2() const;

    /**
    * Get the value of Vehicle Type 3
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType3() const;

    /**
    * Get the value of Vehicle Type 4
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType4() const;

    /**
    * Get the value of Vehicle Type 5
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType5() const;

    /**
    * Get the value of Vehicle Type 6
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType6() const;

    /**
    * Get the value of Vehicle Type 7
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType7() const;

    /**
    * Get the value of Vehicle Type 8
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType8() const;

    /**
    * Get the value of Vehicle Type 9
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType9() const;

    /**
    * Get the value of Vehicle Type 10
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType10() const;

    /**
    * Get the value of Vehicle Type 11
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType11() const;

    /**
    * Get the value of Vehicle Type 12
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType12() const;

    /**
    * Get the value of Vehicle Type 13
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType13() const;

    /**
    * Get the value of Vehicle Type 14
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType14() const;

    /**
    * Get the value of Vehicle Type 15
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType15() const;

    /**
    * Get the value of Vehicle Type 16
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType16() const;

    /**
    * Get the value of Vehicle Type 17
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType17() const;

    /**
    * Get the value of Vehicle Type 18
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType18() const;

    /**
    * Get the value of Vehicle Type 19
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType19() const;

    /**
    * Get the value of Vehicle Type 20
    * This in turn calls the relevant getconfig function
    */
    const char_t * getVehicleType20() const;

        /**
    * Get the maximum number of supervised gradient target
    * This in turn calls the relevant getconfig function
    */
    uint8_t getMaxNrSupvGradTarget() const;

      /**
      * Get the minimum distance for the supervised gradient target
      * This in turn calls the relevant getconfig function
      */
    uint32_t getMinSupvGradTargetDist() const;
    
    /**
    * Get the default brakeability in cm/s2
    * This in turn calls the relevant getconfig function
    * @return default brakeability
    */
    uint16_t getDefaultBrakeability() const;

    /**
    * Get the Delay time for traction cut-off for
    * brake curve calculation
    * This in turn calls the relevant getconfig function
    *
    * @return Delay time for traction cut-off 0.1 s
    */
    uint8_t getTractionCutoffDelay() const;

    /**
    * Get the Time from when the radio connection is lost
    * until AOS is allowed to enter Yard mode
    * This in turn calls the relevant getconfig function
    * @return Time to enter yard mode
    */
    uint8_t getRadioTimeOutYardMode() const;

    /**
    * Get the default brake delay (both for EB and SB)
    * This in turn calls the relevant getconfig function
    * @return default brake delay in ms.
    */
    uint16_t getDefaultBrakeDelay() const;

    /**
    * Get the margin added to supervised SB distance before EB applied
    * This in turn calls the relevant getconfig function
    * @return Margin added to supervised SB distance before EB applied [%].
    */
    uint8_t getEBMarginAdded() const;

    /**
    * Get the time for performing manual integrity confirmation
    * This in turn calls the relevant getconfig function
    * @return Time for performing manual integrity confirmation [s].
    */
    uint8_t getTimsManualConfTime() const;

    /**
    * Get the clock difference between AOS and TIMS sensor
    * This in turn calls the relevant getconfig function
    * @return Clock difference between AOS and TIMS sensor [s].
    */
    uint8_t getTimsSensorTimeDiff() const;

    /**
    * Get the EB pressure propagation speed
    * This in turn calls the relevant getconfig function
    * @return Brake pressure propagation speed [cm/s].
    */
    uint16_t getEbPrPropagationSpeed() const;

    /**
    * Get the 1st number of the expected time sync server version
    * @return 1st number of the time sync server version (or 255 if this number isn't used)
    */
    uint8_t getExpectedTimeSyncServerVersion1() const;

    /**
    * Get the 2nd number of the expected time sync server version
    * @return 2nd number of the time sync server version (or 255 if this number isn't used)
    */
    uint8_t getExpectedTimeSyncServerVersion2() const;

    /**
    * Get the 3rd number of the expected time sync server version
    * @return 3rd number of the time sync server version (or 255 if this number isn't used)
    */
    uint8_t getExpectedTimeSyncServerVersion3() const;

    /**
    * Get the 4th number of the expected time sync server version
    * @return 4th number of the time sync server version (or 255 if this number isn't used)
    */
    uint8_t getExpectedTimeSyncServerVersion4() const;

    /**
    * Get the release speed
    * This in turn calls the relevant getconfig function
    * @return release speed [cm/s].
    */
    uint16_t getReleaseSpeed() const;

    /**
    * Get the departure warning time
    * This in turn calls the relevant getconfig function
    * @return Departurewarning[s]
    */
    uint8_t getDepartureWarningId() const;

    /**
    * Retrieves the delete track margin.
    *
    * @return the delete track margin
    */
    virtual uint32_t getDeleteTrackMargin() const;

    /**
    * Get the margin for the maximum allowed difference between
    * vNom and vMax/vMin in percentage of the current vNom
    * This in turn calls the relevant getconfig function
    * @return vNomMargin[s]
    */
    uint8_t getVNomMargin() const;

    /**
    * Get the expected OPC major version
    * @return the expected OPC major version
    */
    uint8_t getExpectedOpcMajorVersion() const;

    /**
    * Get the expected OPC middle version
    * @return the expected OPC middle version
    */
    uint8_t getExpectedOpcMiddleVersion() const;

    /**
    * Get the expected OPC minor version
    * @return the expected OPC minor version (or 255 if this number isn't used)
    */
    uint8_t getExpectedOpcMinorVersion() const;

  protected:
    /**
    * Constructor
    */
    AbstractConfig();

    /**
    * Get the min-value for locoType
    *
    * @return locoType min-value
    */
    virtual uint8_t getMinValLocoType() const = 0;

    /**
    * Get the max-value for locoType
    *
    * @return locoType max-value
    */
    virtual uint8_t getMaxValLocoType() const = 0;
    
    /**
    * Get the default value for locoType
    *
    * @return locoType default-value
    */
    virtual uint8_t getDefaultValLocoType() const = 0;

    /**
    * Initialize cross compare
    */
    virtual void initCrossCompare() const;

  private:

    /**
    * Flag to prevent multiple initialization.
    */
    bool coreInitDone;
  };
}

#endif

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
* 2016-06-10    jeneman     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  /**
  * Constant for Version number (Major) for the common binary file
  */
  static const uint8_t commonVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the common binary file.
  */
  static const uint8_t commonVersionMinor = 0U;

  /**
  * Constant for Version number (Major) for the type binary file
  */
  static const uint8_t typeVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the type binary file.
  */
  static const uint8_t typeVersionMinor = 0U;

  /**
  * Constant for Version number (Major) for the instance binary file
  */
  static const uint8_t instVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the instance binary file.
  */
  static const uint8_t instVersionMinor = 0U;

  /**
  * Constant for Version number (Major) for the maintenance binary file
  */
  static const uint8_t mntVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the maintenance binary file.
  */
  static const uint8_t mntVersionMinor = 0U;

  /**
  * Constant for Version number (Major) for the run-time binary file
  */
  static const uint8_t rtVersionMajor = 1U;

  /**
  * Constant for Version number (Minor) for the run-time binary file.
  */
  static const uint8_t rtVersionMinor = 0U;

  /**
  * Declaration of Config ID's
  */
  /** Timeout for ticInitToRunTimer */
  static const ATC::ConfigIdType ticInitToRunTimeoutId = 300U;

  /** Minimum Range value of Brake Pressure 1 */
  static const ATC::ConfigIdType rangeMinBP1Id = 301U;

  /** Maximum Range value of Brake Pressure 1 */
  static const ATC::ConfigIdType rangeMaxBP1Id = 302U;

  /** Minimum Range value of Brake Pressure 2 */
  static const ATC::ConfigIdType rangeMinBP2Id = 303U;
  
  /** Maximum Range value of Brake Pressure 2 */
  static const ATC::ConfigIdType rangeMaxBP2Id = 304U;
  
  /**  Maximum Brake Pressure Feedback to qualify as Brakes Applied */
  static const ATC::ConfigIdType maxEbApplyFbId = 305U;

  /**  Minimum Brake Pressure Feedback to qualify as Brakes Released */
  static const ATC::ConfigIdType minEbReleaseFbId = 306U;

  /**  Maximum acceptable difference between the feedback read from the two Brake Pressure Sensors */
  static const ATC::ConfigIdType maxEbFbDiffId = 307U;

  /**  Maximum acceptable inaccuracy in the read Brake Pressure Feedback */
  static const ATC::ConfigIdType ebFbInaccuracyId = 308U;

  /**  EB Feedback signal availability status */
  static const ATC::ConfigIdType ebFbSignalStatusId = 309U;

  /**  Configuration regarding the use of input signal Rail/Road */
  static const ATC::ConfigIdType railRoadInputAvailId = 310U;

  /**  Boolean flag to indicate if EB cut-out inputs are configured */
  static const ATC::ConfigIdType ebCutOutConfiguredId = 311U;  

  /**  Evaluation time for comparing EB feedback signals BP1 and BP2 */
  static const ATC::ConfigIdType maxEbFbDiffTimeoutId = 312U; 

  /**  Minimum percentage of cars with functional brakes on ECPB equipped train */
  static const ATC::ConfigIdType ecpbMinPercBrakeCarsId = 313U;

  /**  Input signal for Non leading Locomotive should be used or not */
  static const ATC::ConfigIdType nonLeadingLocoInputId = 314U;

  /**  Lowest allowed brake pipe pressure in locomotive to keep the train intact */
  static const ATC::ConfigIdType timsMinBPLocoId = 315U;

  /**  Lowest allowed brake pipe pressure in last car to keep the train intact */
  static const ATC::ConfigIdType timsMinBPLastCarId = 316U;

  /**  TCO Order and TCO Feedback signal availability */
  static const ATC::ConfigIdType tcoOrderAndTCOFbId = 317U;

  /**  Max wait time for the expected Emergency Brake Feedback */
  static const ATC::ConfigIdType ebFeedbackTimeoutId = 318U;

  /**  Max wait time for the expected TCO Feedback */
  static const ATC::ConfigIdType tcoFeedbackTimeoutId = 319U;

  /**  Status denoting the usage of TCO- and EB-Feedback during Brake Test */
  static const ATC::ConfigIdType useEbTcoFbBrakeTestId = 320U;

  /**  Speed Difference to take sample for LCS Warning Curve message */
  static const ATC::ConfigIdType lcsSampleSpeedDiffId = 321U;             

  /**  Maximum number of sample points in LCS Warning Curve Message */
  static const ATC::ConfigIdType lcsMaxSampleId = 322U;             

  /**  Percentage of connected ECPB cars used in brake calculations */
  static const ATC::ConfigIdType ecpbCarInBrakeCalcId = 323U;         

  /**  Delta pressure to trigger rapid loss of pressure */
  static const ATC::ConfigIdType bpDeltaRapidLossId = 324U;           

  /**  Time window to detect rapid loss of pressure */
  static const ATC::ConfigIdType bpTimeRapidLossId = 325U;            

  /**  Time limit to consider TIMS as broken unless reported intact by ECPB solution. */
  static const ATC::ConfigIdType timsEcbpTimeOutId = 326U;

  /**  Time limit to consider TIMS as broken unless reported intact by EOT solution. */
  static const ATC::ConfigIdType timsEotTimeOutId = 327U;

  /**  Margin to be used determine train length for the TIMS function */
  static const ATC::ConfigIdType timsLengthMarginId = 328U;

  /** typical train configuration name at index 1 */
  static const ATC::ConfigIdType typicalConfigName1Id = 331U;

  /** typical train configuration car type at index 1 */
  static const ATC::ConfigIdType typicalConfigCarType1Id = 332U;

  /** typical train configuration number of cars at index 1 */
  static const ATC::ConfigIdType typicalConfigNoOfCars1Id = 333U;

  /** typical train configuration name at index 2 */
  static const ATC::ConfigIdType typicalConfigName2Id = 334U;

  /** typical train configuration car type at index 2 */
  static const ATC::ConfigIdType typicalConfigCarType2Id = 335U;

  /** typical train configuration number of cars at index 2 */
  static const ATC::ConfigIdType typicalConfigNoOfCars2Id = 336U;

  /** typical train configuration name at index 3 */
  static const ATC::ConfigIdType typicalConfigName3Id = 337U;

  /** typical train configuration car type at index 3 */
  static const ATC::ConfigIdType typicalConfigCarType3Id = 338U;

  /** typical train configuration number of cars at index 3 */
  static const ATC::ConfigIdType typicalConfigNoOfCars3Id = 339U;

  /** typical train configuration name at index 4 */
  static const ATC::ConfigIdType typicalConfigName4Id = 340U;

  /** typical train configuration car type at index 1 */
  static const ATC::ConfigIdType typicalConfigCarType4Id = 341U;

  /** typical train configuration number of cars at index 4 */
  static const ATC::ConfigIdType typicalConfigNoOfCars4Id = 342U;

  /** typical train configuration name at index 5 */
  static const ATC::ConfigIdType typicalConfigName5Id = 343U;

  /** typical train configuration car type at index 5 */
  static const ATC::ConfigIdType typicalConfigCarType5Id = 344U;

  /** typical train configuration number of cars at index 5 */
  static const ATC::ConfigIdType typicalConfigNoOfCars5Id = 345U;

  /** Brake pressure propagation speed */
  static const ATC::ConfigIdType sbPrPropagationSpeedId = 346U;

  /** The maximal extended reversing distance */   
  static const ATC::ConfigIdType maxExtRevDistanceId = 347U;

  /** The maximal extended reversing speed */
  static const ATC::ConfigIdType maxExtRevSpeedId = 348U;

  /** The maximal extended reversing speed */
  static const ATC::ConfigIdType minApproachSpeedId = 349U;

  /** Margin to be used determine train length for the OBRD function */
  static const ATC::ConfigIdType obrdLengthMarginId = 350U;

  /** OBRD brake test pressure */
  static const ATC::ConfigIdType obrdBrakeTestPressureId = 351U;

  /** OBRD report timeout */
  static const ATC::ConfigIdType obrdReportTimeoutId = 352U;

  /** EB pressure margin in kPa per second, when the rise of the brake pressure has stabilized */
  static const ATC::ConfigIdType brakePressureStabilizeMarginId = 353U;

  /**
  * Locomotive Type
  */
  //lint -esym(769,ATP::LocoTypeAdap::TMM) Might be needed by adaptation
  enum LocoTypeAdap
  {
   /**  Production locomotives */
   EMD = 0, 
   /**  Track Maintenance Machines */
   TMM,     
   /**  Pickup trucks with rail operation gear */
   HiRail   
  };

  /**
  * Neither EB- or TCO-feedback are used during brake-test (see @ref getUseEbTcoFbDuringBrakeTest()).
  */
  static const uint8_t noFeedbackDuringBrakeTest = 0U;

  /**
  * Only EB-feedback is used during brake-test (see @ref getUseEbTcoFbDuringBrakeTest()).
  */
  static const uint8_t EbFeedbackDuringBrakeTest = 1U;

  /**
  * Only TCO-feedback is used during brake-test (see @ref getUseEbTcoFbDuringBrakeTest()).
  */
  static const uint8_t TcoFeedbackDuringBrakeTest = 2U;

  /**
  * Both EB- and TCO-feedback is used during brake-test (see @ref getUseEbTcoFbDuringBrakeTest()).
  */
  static const uint8_t EbAndTcoFeedbackDuringBrakeTest = 3U;

  /**
  * EB Feedback not available (see @ref getEbFbSignalStatus()).
  */
  static const uint8_t eBFeedbackSignalStatusNotUsed = 0U;

  /**
  * Only the first EB Feedback Sensor available (see @ref getEbFbSignalStatus()).
  */
  static const uint8_t eBFeedbackSignalStatusFirstEbFbOnly = 1U;

  /**
  * Only the second EB Feedback Sensor available (see @ref getEbFbSignalStatus()).
  */
  static const uint8_t eBFeedbackSignalStatusSecondEbFbOnly = 2U;

  /**
  * Both the EB feedback sensors available (see @ref getEbFbSignalStatus()).
  */
  static const uint8_t eBFeedbackSignalStatusTwoEbFb = 3U;

  /**
  * TCO Order and Feedback not used (see @ref getTcoOrderAndTCOFb()).
  */
  static const uint8_t tcoFbAndOrderNotUsed = 0U;

  /**
  * No TCO order used Only TCO Fb used (see @ref getTcoOrderAndTCOFb()).
  */
  static const uint8_t tcoFbOnly = 1U;

  /**
  *  Only TCO order used No TCO Fb used (see @ref getTcoOrderAndTCOFb()).
  */
  static const uint8_t tcoOrderOnly = 2U;

  /**
  *  Both TCO order and TCO Fb used (see @ref getTcoOrderAndTCOFb()).
  */
  static const uint8_t tcoFbAndOrderUsed = 3U;

  /**
  * Max string length of names in BHPTypicalConfig
  */
  static const uint8_t bhpConfigNameLength = 20U;
  //lint -esym(551,ATP::bhpConfigNameLength) Lint is wrong, this constant *is* used

  /**
  * Name and attributes of a typical configuration.
  */
  struct BHPTypicalConfig
  {
    BHPTypicalConfig();

    /** Inequality operator */
    bool operator !=(const BHPTypicalConfig& that) const;

    /** typical train configuration name */
    char_t configName[bhpConfigNameLength + 1U];

    /** typical train configuration type name */
    char_t vehTypeName[bhpConfigNameLength + 1U];

    /** typical train configuration number of cars */
    uint8_t noOfCars;
  };

  /**
  * The class Config instantiates the abstract class and implements
  * the interfaces needed for both inherited classes and component.
  *
  */
  class Config : public AbstractConfig
  {
  public:

    /**
    * Implements the virtual preInit function.
    *
    */
    virtual void preInit(void);

    /**
    * Implements the init function.
    * The ATP-adaptation specific config items are added here.
    *
    * @return Returns true when initialization completed
    */
    virtual bool init(void);

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static Config& instance(void);

    /**
    * Get the maximum execution time for the TIC system to transition from Init to Run
    * This in turn calls the relevant getconfig function
    *
    * @return The timeout value in 's'
    */
    uint16_t getTicInitToRunTimeout() const;

    /**
    * Get the Minimum Range value of Brake Pressure 1
    * This in turn calls the relevant getconfig function
    *
    * @return The minimum range of Brake Pressure 1
    */
    uint16_t getRangeMinBP1() const;

    /**
    * Get the Maximum Range value of Brake Pressure 1
    * This in turn calls the relevant getconfig function
    *
    * @return The maximum range of Brake Pressure 1
    */
    uint16_t getRangeMaxBP1() const;

    /**
    * Get the Minimum Range value of Brake Pressure 2
    * This in turn calls the relevant getconfig function
    *
    * @return The minimum range of Brake Pressure 2
    */
    uint16_t getRangeMinBP2() const;

    /**
    * Get the Maximum Range value of Brake Pressure 2
    * This in turn calls the relevant getconfig function
    *
    * @return The maximum range of Brake Pressure
    */
    uint16_t getRangeMaxBP2() const;

    /**
    * Get the Maximum Brake Pressure value to qualify as 'Brakes Applied'
    * This in turn calls the relevant getconfig function
    *
    * @return The Maximum Brake Pressure value to qualify as 'Brakes Applied'
    */
    uint16_t getMaxEbApplyFeedback() const;

    /**
    * Get the Minimum Brake Pressure value to qualify as 'Brakes Released'
    * This in turn calls the relevant getconfig function
    *
    * @return The Minimum Brake Pressure value to qualify as 'Brakes Released'
    */
    uint16_t getMinEbReleaseFeedback() const;

    /**
    * Get the Maximum acceptable difference between the two Brake Pressure Sensors
    * This in turn calls the relevant getconfig function
    *
    * @return The max difference between the Brake Pressure sensors
    */
    uint16_t getMaxEbFbDiff() const;

    /**
    * Get the Maximum acceptable inaccuracy in the read Brake Pressure Feedback
    * This in turn calls the relevant getconfig function
    *
    * @return The max acceptable inaccuracy in the read Brake Pressure feedback values
    */
    uint16_t getEbFbInaccuracy() const;
    
    /**
    * Get the usage of EB- and TCO-feedback usage during brake-test.
    * This in turn calls the relevant getconfig function
    *
    * @return 0 = No usage (or) 1 = Only EB Feedback is used (or) 2 = Only TCO Feedback is used
    * 3 = Both the EB and TCO feedback signals are used.
    */
    uint8_t getUseEbTcoFbDuringBrakeTest() const;

    /**
    * Get the analog EB Feedback signal(s) availability status.
    * This in turn calls the relevant getconfig function
    *
    * @return 0 = Not Used (or) 1 = Only the First EB Feedback Used (or) 2 = Only the second EB Feedback signals used.
    * 3 = Both the EB feedback signals used.
    */
    uint8_t getEbFbSignalStatus() const;
    
    /**
    * Get the usage of TCO-order and TCO feedback signals.
    * This in turn calls the relevant getconfig function
    *
    * @return 0 = Both TCO order and TCO FB  not available, 1 = No TCO order available, only TCO FB is used,
    *         2 = Only TCO order is used no TCO FB is available, 3= Both TCO order and TCO feedback is used.
    */
    uint8_t getTcoOrderAndTCOFb() const;
    
    /**
    * Get the max wait time (seconds) for the expected Emergency Brake Feedback
    *
    * @return ebFeedbackTimeout
    */
    uint16_t getEbFeedbackTimeout() const;

    /**
    * Get the max wait time (seconds) for the expected TCO Feedback
    *
    * @return tcoFeedbackTimeout
    */
    uint16_t getTcoFeedbackTimeout() const;

    /**
    * Get the Boolean flag indicating if EB cut-out inputs are configured
    *
    * @return ebCutOutConfigured
    */
    bool getEbCutOutConfigured() const;
   
    /**
    * Get the evaluation time for comparing EB feedback input signals
    *
    * @return maxEbFbDiffTimeout
    */
    uint8_t getMaxEbFbDiffTimeout() const;

    /**
    * Get the minimum percentage of cars with functional brakes on ECPB train.
    *
    * @return eCPBMinPercBrakeCars
    */
    uint8_t getECPBMinPercBrakeCars() const;

    /**
    * Get the status Rail/Road input is available.
    *
    * @return railRoadInputAvail
    */
    bool getRailRoadInputAvail() const;

    /**
    * Get the status if input signal for Non leading Locomotive should be used or not.
    *
    * @return nonLeadingLocoInput
    */
    bool getNonLeadingLocoInput() const;

    /**
    * Get the lowest allowed brake pipe pressure in locomotive to consider the train as intact
    *
    * @return timsMinBPLoco
    */
    uint16_t getTimsMinBPLoco() const;

    /**
    * Get the lowest allowed brake pipe pressure in last car to consider the train as intact
    *
    * @return timsMinBPLastCar
    */
    uint16_t getTimsMinBPLastCar() const;

    /**
    * Get the Speed Difference to take next sample for LCS Warning Curve Message
    * @return Configured value of speed difference in cm/s
    */
    uint8_t getLCSSpeedDiffForSample() const;

    /**
    * Get the value of Maximum Number of sample for LCS Warning Curve Message
    * @return Configured value of maximum number of samples
    */
    uint8_t getLCSMaxSample() const;

    /**
    * Get the Percentage of connected ECPB cars used in brake calculations
    *
    * @return ECPBCarInBrakeCalc 
    */
    uint8_t getECPBCarsInBrakeCalc() const;

    /**
    * Get the Delta pressure to trigger rapid loss of pressure
    *
    * @return bpDeltaRapidLoss
    */
    uint16_t getBPDeltaRapidLoss() const;

    /**
    * Get the Time window to detect rapid loss of pressure in 0.1 s
    *
    * @return bpTimeRapidLoss
    */
    uint8_t getBPTimeRapidLoss() const;

    /**
    * Get the time limit to consider TIMS as broken unless reported intact
    *
    * @return TimsEcbpTimeOut
    */
    uint8_t getTimsEcbpTimeOut() const;

    /**
    * Get the time limit to consider TIMS as broken unless reported intact
    *
    * @return TimsEotTimeOut
    */
    uint8_t getTimsEotTimeOut() const;

    /**
    * Get the margin to be used determine train length for the TIMS function
    *
    * @return TimsLengthMargin
    */
    uint8_t getTimsLengthMargin() const;
    
    /**
    * Get the SB pressure propagation speed
    *
    * @return Brake pressure propagation speed [cm/s].
    */
    uint16_t getSbPrPropagationSpeed() const;

    /**
    * Get the maximum waiting time for receiving a valid brake pressure from OBRD
    *
    * @return OBRDBrakePrTimeout
    */
    uint8_t getOBRDReportTimeout() const;

    /**
    * Get the maximal extended reversing distance
    *
    * @return ExtReversingDistance
    */
    uint16_t getMaxExtReversingDistance() const;

    /**
    * Get the ceiling speed allowed during extended reversing
    *
    * @return ExtReversingSpeed
    */
    uint16_t getMaxExtReversingSpeed() const;

    /**
    * Get the minimum speed to be used for approach speed
    *
    * @return MinApproachSpeed
    */
    uint16_t getMinApproachSpeed() const;

    /**
    * Get the margin to be used determine train length for the obrd position
    *
    * @return Config distance value
    */
    uint8_t getOBRDLengthMargin() const;

    /**
    * Get the brake pressure limit used for the pre-departure brake test
    *
    * @return OBRDBrakeTestPressure [kPa]
    */
    uint16_t getOBRDBrakeTestPressure() const;

    /**
    * Get the EB pressure margin in kPa per second, when the rise of the brake pressure has stabilized
    *
    * @return Brake pressure EB pressure margin, when the rise of the brake pressure has stabilized
    */
    uint16_t getBrakePressureStabilizeMargin() const;

    /**
    * Get the typical train configuration with the given index.
    *
    * @param[in]  configIndex  the index of the config to retrieve (0-4)
    * @param[out] config       the typical train configuration at index 'index'; if no such
    *                          configuration exists, 'config' will contain all zeroes
    */
    void getTypicalConfig(const uint8_t configIndex, BHPTypicalConfig& config) const;

    /**
    * Retrieves he delete track margin.
    *
    * @return the delete track margin
    */
    virtual uint32_t getDeleteTrackMargin() const;

  protected:

    /**
    * Get the min-value for locoType
    *
    * @return locoType min-value
    */
    virtual uint8_t getMinValLocoType() const;

    /**
    * Get the max-value for locoType
    *
    * @return locoType max-value
    */
    virtual uint8_t getMaxValLocoType() const;

    /**
    * Get the default-value for locoType in SIL
    *
    * @return locoType default-value
    */
    virtual uint8_t getDefaultValLocoType() const;

    /**
    * Initialize cross compare
    */
    virtual void initCrossCompare() const;

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

    /**
    * Helper for retrieving typical train configurations.
    *
    * @param[in]  configNameId   the id of the parameter for configuration name
    * @param[in]  vehicleTypeId  the id of the parameter for vehicle type
    * @param[in]  noOfCarsId     the id of the parameter for number of cars
    * @param[out] config         the requested typical train configuration; if no such
    *                            configuration exists, 'config' will contain all zeroes
    */
    void getTypicalConfig(
      const ATC::ConfigIdType configNameId, const ATC::ConfigIdType vehicleTypeId,
      const ATC::ConfigIdType noOfCarsId, BHPTypicalConfig& config) const;

    /**
    * Flag to prevent multiple initialization.
    */
    bool initDone;
  };
}
#endif
